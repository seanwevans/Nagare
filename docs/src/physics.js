/**
 * cannon-es rigid body world.
 *
 * Everything in here is expressed in world (three.js, +Y up) coordinates; the
 * app converts to and from Nagare coordinates with ./space.js.
 */
import * as CANNON from 'cannon-es';

/** UV-sphere mesh data for an ellipsoid, used for solid zone colliders. */
function ellipsoidMeshData(rx, ry, rz, widthSegments = 18, heightSegments = 12) {
  const vertices = [];
  const indices = [];
  for (let iy = 0; iy <= heightSegments; iy += 1) {
    const phi = (iy / heightSegments) * Math.PI;
    for (let ix = 0; ix <= widthSegments; ix += 1) {
      const theta = (ix / widthSegments) * Math.PI * 2;
      vertices.push(
        rx * Math.sin(phi) * Math.cos(theta),
        ry * Math.cos(phi),
        rz * Math.sin(phi) * Math.sin(theta),
      );
    }
  }
  const stride = widthSegments + 1;
  for (let iy = 0; iy < heightSegments; iy += 1) {
    for (let ix = 0; ix < widthSegments; ix += 1) {
      const a = iy * stride + ix;
      const b = a + stride;
      indices.push(a, b, a + 1, b, b + 1, a + 1);
    }
  }
  return { vertices, indices };
}

/** Mass of an entity sphere at the given radius, at a fixed density. */
export function sphereMass(radius) {
  return Math.max(0.05, (4 / 3) * Math.PI * radius ** 3 * 220);
}

export function createPhysics() {
  const world = new CANNON.World({ gravity: new CANNON.Vec3(0, 0, 0) });
  world.broadphase = new CANNON.SAPBroadphase(world);
  world.allowSleep = false;
  world.solver.iterations = 12;
  world.defaultContactMaterial.friction = 0.18;
  world.defaultContactMaterial.restitution = 0.55;

  const bodyMaterial = new CANNON.Material('nagare');
  const boundaries = [];
  const solids = new Map();
  let arenaSize = 14;
  let wallsEnabled = false;
  let floorEnabled = false;
  let accumulator = 0;

  const FIXED_STEP = 1 / 120;

  function makePlane(normal, point) {
    const body = new CANNON.Body({ mass: 0, material: bodyMaterial, shape: new CANNON.Plane() });
    body.quaternion.setFromVectors(new CANNON.Vec3(0, 0, 1), normal);
    body.position.copy(point);
    return body;
  }

  function rebuildBoundaries() {
    boundaries.forEach((body) => world.removeBody(body));
    boundaries.length = 0;
    const half = arenaSize / 2;

    if (floorEnabled || wallsEnabled) {
      boundaries.push(makePlane(new CANNON.Vec3(0, 1, 0), new CANNON.Vec3(0, 0, 0)));
    }
    if (wallsEnabled) {
      boundaries.push(makePlane(new CANNON.Vec3(0, -1, 0), new CANNON.Vec3(0, arenaSize, 0)));
      boundaries.push(makePlane(new CANNON.Vec3(1, 0, 0), new CANNON.Vec3(-half, 0, 0)));
      boundaries.push(makePlane(new CANNON.Vec3(-1, 0, 0), new CANNON.Vec3(half, 0, 0)));
      boundaries.push(makePlane(new CANNON.Vec3(0, 0, 1), new CANNON.Vec3(0, 0, -half)));
      boundaries.push(makePlane(new CANNON.Vec3(0, 0, -1), new CANNON.Vec3(0, 0, half)));
    }
    boundaries.forEach((body) => world.addBody(body));
  }

  return {
    world,
    bodyMaterial,

    setGravity(value) {
      world.gravity.set(0, -value, 0);
    },

    setRestitution(value) {
      world.defaultContactMaterial.restitution = value;
    },

    setArena(size, walls, floor) {
      arenaSize = size;
      wallsEnabled = walls;
      floorEnabled = floor;
      rebuildBoundaries();
    },

    sphereMass,

    /** Dynamic sphere for a Nagare entity. `position` is a world-space vector. */
    addEntityBody(position, radius, damping) {
      const body = new CANNON.Body({
        mass: sphereMass(radius),
        shape: new CANNON.Sphere(radius),
        material: bodyMaterial,
        linearDamping: damping,
        angularDamping: 0.35,
      });
      body.position.set(position.x, position.y, position.z);
      body.allowSleep = false;
      world.addBody(body);
      return body;
    },

    removeBody(body) {
      if (body) world.removeBody(body);
    },

    /**
     * Rebuild the static colliders for `solid` zones. Boxes map onto cannon's Box,
     * spheres onto Sphere, and general ellipsoids onto a static Trimesh.
     * `zones` carry world-space `position` and `halfExtents` vectors.
     */
    syncSolidZones(zones) {
      const seen = new Set();
      zones.forEach((zone) => {
        seen.add(zone.key);
        const existing = solids.get(zone.key);
        if (existing && existing.signature === zone.signature) {
          existing.body.position.set(zone.position.x, zone.position.y, zone.position.z);
          return;
        }
        if (existing) world.removeBody(existing.body);

        const { x: hx, y: hy, z: hz } = zone.halfExtents;
        let shape;
        if (zone.kind === 'box') {
          shape = new CANNON.Box(new CANNON.Vec3(hx, hy, hz));
        } else if (Math.abs(hx - hy) < 1e-6 && Math.abs(hy - hz) < 1e-6) {
          shape = new CANNON.Sphere(hx);
        } else {
          const { vertices, indices } = ellipsoidMeshData(hx, hy, hz);
          shape = new CANNON.Trimesh(vertices, indices);
        }

        const body = new CANNON.Body({ mass: 0, shape, material: bodyMaterial });
        body.position.set(zone.position.x, zone.position.y, zone.position.z);
        world.addBody(body);
        solids.set(zone.key, { body, signature: zone.signature });
      });

      [...solids.keys()].forEach((key) => {
        if (seen.has(key)) return;
        world.removeBody(solids.get(key).body);
        solids.delete(key);
      });
    },

    clearSolids() {
      solids.forEach((entry) => world.removeBody(entry.body));
      solids.clear();
    },

    /**
     * Advance the world with a fixed inner step so behaviour is frame-rate independent.
     * `onSubstep` runs immediately before each substep: cannon clears accumulated
     * forces after every `world.step`, so field coupling has to be re-applied here
     * rather than once per Nagare step.
     */
    step(delta, onSubstep) {
      accumulator += Math.min(delta, 0.1);
      let iterations = 0;
      while (accumulator >= FIXED_STEP && iterations < 24) {
        if (onSubstep) onSubstep(FIXED_STEP);
        world.step(FIXED_STEP);
        accumulator -= FIXED_STEP;
        iterations += 1;
      }
      if (iterations === 24) accumulator = 0;
    },
  };
}
