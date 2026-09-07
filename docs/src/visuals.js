/**
 * Meshes for the things the runtime simulates: entities with fading trails,
 * translucent zone volumes with labels, and the drag preview for new zones.
 */
import * as THREE from 'three';

const SPHERE = new THREE.SphereGeometry(1, 32, 24);
const BOX = new THREE.BoxGeometry(2, 2, 2);
const SPHERE_EDGES = new THREE.EdgesGeometry(new THREE.SphereGeometry(1, 14, 8), 1);
const BOX_EDGES = new THREE.EdgesGeometry(BOX);

export const ENTITY_HUES = [190, 265, 32, 145, 330, 55, 215, 300];

export function entityColor(index) {
  return new THREE.Color().setHSL(ENTITY_HUES[index % ENTITY_HUES.length] / 360, 0.78, 0.62);
}

function makeLabel(text, color) {
  const scale = 4;
  const font = `700 ${30 * scale}px Inter, system-ui, sans-serif`;
  const canvas = document.createElement('canvas');
  const ctx = canvas.getContext('2d');

  ctx.font = font;
  canvas.width = Math.ceil(ctx.measureText(text).width) + 28 * scale;
  canvas.height = 46 * scale;
  // Resizing the canvas resets the drawing state, so the font is applied again.
  ctx.font = font;
  ctx.textBaseline = 'middle';
  ctx.fillStyle = 'rgba(3, 9, 20, 0.72)';
  ctx.fillRect(0, 0, canvas.width, canvas.height);
  ctx.fillStyle = `#${color.getHexString()}`;
  ctx.fillText(text, 14 * scale, canvas.height / 2);

  const texture = new THREE.CanvasTexture(canvas);
  texture.colorSpace = THREE.SRGBColorSpace;
  texture.minFilter = THREE.LinearFilter;

  const sprite = new THREE.Sprite(new THREE.SpriteMaterial({ map: texture, depthTest: false, transparent: true }));
  sprite.renderOrder = 3;
  sprite.scale.set((canvas.width / canvas.height) * 0.42, 0.42, 1);
  return sprite;
}

/** A simulated entity: emissive sphere plus a fading polyline trail. */
export function createEntityVisual({ id, index, radius, maxTrail = 1600 }) {
  const color = entityColor(index);

  const mesh = new THREE.Mesh(
    SPHERE,
    new THREE.MeshStandardMaterial({
      color,
      emissive: color.clone().multiplyScalar(0.55),
      emissiveIntensity: 1.1,
      roughness: 0.28,
      metalness: 0.1,
    }),
  );
  mesh.scale.setScalar(radius);
  mesh.castShadow = true;
  mesh.userData.pickId = id;

  const halo = new THREE.Mesh(
    SPHERE,
    new THREE.MeshBasicMaterial({ color, transparent: true, opacity: 0.16, depthWrite: false }),
  );
  halo.scale.setScalar(2.1);
  halo.userData.pickId = id;
  mesh.add(halo);

  const positions = new Float32Array(maxTrail * 3);
  const colors = new Float32Array(maxTrail * 3);
  const geometry = new THREE.BufferGeometry();
  geometry.setAttribute('position', new THREE.BufferAttribute(positions, 3));
  geometry.setAttribute('color', new THREE.BufferAttribute(colors, 3));
  geometry.setDrawRange(0, 0);

  const trail = new THREE.Line(
    geometry,
    new THREE.LineBasicMaterial({ vertexColors: true, transparent: true, opacity: 0.95 }),
  );
  trail.frustumCulled = false;

  let count = 0;
  const last = new THREE.Vector3(Infinity, Infinity, Infinity);

  return {
    id,
    color,
    mesh,
    halo,
    trail,
    // Only the body is pickable; the trail would be far too easy to click by accident.
    objects: [mesh],

    setRadius(value) {
      mesh.scale.setScalar(value);
    },

    setFinished(finished) {
      mesh.material.emissiveIntensity = finished ? 0.25 : 1.1;
      halo.material.opacity = finished ? 0.05 : 0.16;
    },

    pushTrailPoint(position) {
      if (last.distanceToSquared(position) < 1.6e-4) return;
      last.copy(position);
      if (count === maxTrail) {
        positions.copyWithin(0, 3);
        count -= 1;
      }
      positions[count * 3] = position.x;
      positions[count * 3 + 1] = position.y;
      positions[count * 3 + 2] = position.z;
      colors[count * 3] = color.r;
      colors[count * 3 + 1] = color.g;
      colors[count * 3 + 2] = color.b;
      count += 1;

      // Refreshing the whole ramp is O(n), so only do it every few appends; the
      // handful of newest vertices simply stay at full brightness until then.
      if (count % 8 === 0) {
        for (let i = 0; i < count; i += 1) {
          const fade = 0.32 + 0.68 * (i / Math.max(1, count - 1));
          colors[i * 3] = color.r * fade;
          colors[i * 3 + 1] = color.g * fade;
          colors[i * 3 + 2] = color.b * fade;
        }
        geometry.attributes.color.needsUpdate = true;
      }

      geometry.setDrawRange(0, count);
      geometry.attributes.position.needsUpdate = true;
      geometry.attributes.color.needsUpdate = true;
    },

    clearTrail() {
      count = 0;
      last.set(Infinity, Infinity, Infinity);
      geometry.setDrawRange(0, 0);
    },

    setTrailVisible(visible) {
      trail.visible = visible;
    },

    dispose() {
      geometry.dispose();
      trail.material.dispose();
      mesh.material.dispose();
      halo.material.dispose();
    },
  };
}

/** A zone volume: translucent shell, wireframe edges and a floating label. */
export function createZoneVisual(zone) {
  const group = new THREE.Group();
  group.userData.pickId = zone.id;

  const isBox = zone.kind === 'box';
  const accent = new THREE.Color(zone.solid ? 0x2dd4bf : 0x22d3ee);
  const tone = zone.origin === 'drawn' ? new THREE.Color(0xfacc15) : accent;

  const shell = new THREE.Mesh(
    isBox ? BOX : SPHERE,
    new THREE.MeshStandardMaterial({
      color: tone,
      emissive: tone.clone().multiplyScalar(0.22),
      transparent: true,
      opacity: zone.solid ? 0.34 : 0.14,
      roughness: 0.35,
      metalness: 0.05,
      side: THREE.DoubleSide,
      depthWrite: zone.solid,
    }),
  );
  shell.castShadow = zone.solid;
  shell.receiveShadow = zone.solid;
  shell.userData.pickId = zone.id;
  group.add(shell);

  const edges = new THREE.LineSegments(
    isBox ? BOX_EDGES : SPHERE_EDGES,
    new THREE.LineBasicMaterial({ color: tone, transparent: true, opacity: 0.55 }),
  );
  edges.userData.pickId = zone.id;
  group.add(edges);

  const label = makeLabel(zone.solid ? `${zone.label} · solid` : zone.label, tone);
  group.add(label);

  function update(next) {
    group.position.set(next.center.x, next.center.z, next.center.y);
    const scale = new THREE.Vector3(next.radii.x, next.radii.z, next.radii.y);
    shell.scale.copy(scale);
    edges.scale.copy(scale);
    label.position.set(0, next.radii.z + 0.34, 0);
  }

  update(zone);

  return {
    id: zone.id,
    group,
    update,
    setLabelVisible(visible) {
      label.visible = visible;
    },
    dispose() {
      shell.material.dispose();
      edges.material.dispose();
      label.material.map.dispose();
      label.material.dispose();
    },
  };
}

/** Wireframe ghost shown while dragging out a new zone. */
export function createZonePreview() {
  const mesh = new THREE.Mesh(
    SPHERE,
    new THREE.MeshBasicMaterial({ color: 0xfacc15, transparent: true, opacity: 0.18, depthWrite: false }),
  );
  const edges = new THREE.LineSegments(
    SPHERE_EDGES,
    new THREE.LineBasicMaterial({ color: 0xfacc15, transparent: true, opacity: 0.9 }),
  );
  mesh.add(edges);
  mesh.visible = false;
  return {
    mesh,
    show(center, radii) {
      mesh.visible = true;
      mesh.position.set(center.x, center.z, center.y);
      mesh.scale.set(radii.x, radii.z, radii.y);
    },
    hide() {
      mesh.visible = false;
    },
  };
}

/** Marker showing where the current placement plane sits. */
export function createPlacementMarker() {
  const ring = new THREE.Mesh(
    new THREE.RingGeometry(0.92, 1, 64),
    new THREE.MeshBasicMaterial({ color: 0x38bdf8, transparent: true, opacity: 0.5, side: THREE.DoubleSide, depthWrite: false }),
  );
  ring.rotation.x = -Math.PI / 2;
  return {
    mesh: ring,
    update(height, radius) {
      ring.position.y = height;
      ring.scale.setScalar(radius);
    },
    setVisible(visible) {
      ring.visible = visible;
    },
  };
}
