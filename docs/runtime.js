/**
 * Nagare 3D runtime playground.
 *
 * The Nagare program supplies a discrete map on (x, y, z). Every simulation step
 * the map is evaluated at each entity's current position and the difference is
 * handed to a cannon-es rigid body world, either as the body's velocity (flow) or
 * as a steering force (steer). three.js renders the result: entities with fading
 * trails, translucent zone volumes and the solid zones they can bounce off.
 */
import * as CANNON from 'cannon-es';
import {
  EXAMPLE_PROGRAM,
  format,
  insertZoneSource,
  parseNagare,
  removeZoneSource,
  zoneContains,
} from './src/nagare.js';
import { fromWorld, toWorld } from './src/space.js';
import { createStage } from './src/scene.js';
import { createPhysics, sphereMass } from './src/physics.js';
import { createEntityVisual, createPlacementMarker, createZonePreview, createZoneVisual } from './src/visuals.js';

const dom = {
  source: document.getElementById('programSource'),
  status: document.getElementById('status'),
  canvas: document.getElementById('runtimeCanvas'),
  run: document.getElementById('runProgram'),
  restart: document.getElementById('restartRun'),
  loadExample: document.getElementById('loadExample'),
  clear: document.getElementById('resetRuntime'),
  stepLimit: document.getElementById('stepLimit'),
  stepInterval: document.getElementById('stepInterval'),
  entityList: document.getElementById('entityList'),
  zoneList: document.getElementById('zoneList'),
  eventLog: document.getElementById('eventLog'),
  hudStep: document.getElementById('hudStep'),
  hudBodies: document.getElementById('hudBodies'),
  hudFps: document.getElementById('hudFps'),
  tools: [...document.querySelectorAll('.tool')],
  placeHeight: document.getElementById('placeHeight'),
  zoneHeight: document.getElementById('zoneHeight'),
  zoneSolid: document.getElementById('zoneSolid'),
  couplingMode: document.getElementById('couplingMode'),
  gravity: document.getElementById('gravity'),
  fieldResponse: document.getElementById('fieldResponse'),
  damping: document.getElementById('damping'),
  restitution: document.getElementById('restitution'),
  entityRadius: document.getElementById('entityRadius'),
  arenaSize: document.getElementById('arenaSize'),
  enableFloor: document.getElementById('enableFloor'),
  enableWalls: document.getElementById('enableWalls'),
  showTrails: document.getElementById('showTrails'),
  showLabels: document.getElementById('showLabels'),
  showShadows: document.getElementById('showShadows'),
  showGrid: document.getElementById('showGrid'),
  followEntities: document.getElementById('followEntities'),
  viewReset: document.getElementById('viewReset'),
  viewTop: document.getElementById('viewTop'),
  viewFront: document.getElementById('viewFront'),
};

const DEFAULT_ENTITY_STARTS = [
  { x: 2, y: 0.1, z: 0.3 },
  { x: 1.5, y: -1.5, z: 2.8 },
];

const stage = createStage(dom.canvas);
const physics = createPhysics();
const preview = createZonePreview();
const marker = createPlacementMarker();
stage.world.add(preview.mesh);
stage.world.add(marker.mesh);

const state = {
  tool: 'orbit',
  program: null,
  entities: [],
  entityCounter: 1,
  drawnZones: [],
  zoneCounter: 1,
  zoneVisuals: new Map(),
  running: false,
  step: 0,
  stepAccumulator: 0,
  triggered: new Set(),
  events: [],
  drag: null,
  listsDirty: true,
  frameCount: 0,
  fpsClock: 0,
  listClock: 0,
};

const scratchCannon = new CANNON.Vec3();
const scratchForce = new CANNON.Vec3();

/* ------------------------------------------------------------------ helpers */

function setStatus(message, kind = '') {
  dom.status.textContent = message;
  dom.status.className = kind;
}

function numberInput(element, fallback) {
  if (element.value === '') return fallback;
  const value = Number(element.value);
  return Number.isFinite(value) ? value : fallback;
}

function settings() {
  return {
    stepLimit: Math.min(20000, Math.max(1, Math.round(numberInput(dom.stepLimit, 1200)))),
    stepInterval: Math.min(1, Math.max(0.004, numberInput(dom.stepInterval, 45) / 1000)),
    coupling: dom.couplingMode.value,
    response: numberInput(dom.fieldResponse, 8),
  };
}

/* ------------------------------------------------------------------ entities */

function createEntity(start) {
  const radius = numberInput(dom.entityRadius, 0.22);
  const id = `entity-${state.entityCounter}`;
  state.entityCounter += 1;

  const visual = createEntityVisual({ id, index: state.entities.length, radius });
  visual.setTrailVisible(dom.showTrails.checked);
  stage.world.add(visual.mesh);
  stage.world.add(visual.trail);

  const body = physics.addEntityBody(
    toWorld(start, new CANNON.Vec3()),
    radius,
    numberInput(dom.damping, 0.05),
  );

  const entity = {
    id,
    start: { ...start },
    visual,
    body,
    finished: false,
    fieldVelocity: new CANNON.Vec3(),
  };
  state.entities.push(entity);
  syncEntityMesh(entity);
  state.listsDirty = true;
  return entity;
}

function removeEntity(entity) {
  physics.removeBody(entity.body);
  stage.world.remove(entity.visual.mesh);
  stage.world.remove(entity.visual.trail);
  entity.visual.dispose();
  state.entities = state.entities.filter((candidate) => candidate !== entity);
  state.listsDirty = true;
}

function clearEntities() {
  [...state.entities].forEach(removeEntity);
  state.entityCounter = 1;
}

function syncEntityMesh(entity) {
  entity.visual.mesh.position.set(entity.body.position.x, entity.body.position.y, entity.body.position.z);
  entity.visual.mesh.quaternion.set(
    entity.body.quaternion.x,
    entity.body.quaternion.y,
    entity.body.quaternion.z,
    entity.body.quaternion.w,
  );
}

function resetEntities() {
  const radius = numberInput(dom.entityRadius, 0.22);
  state.entities.forEach((entity) => {
    toWorld(entity.start, entity.body.position);
    entity.body.velocity.setZero();
    entity.body.angularVelocity.setZero();
    entity.body.force.setZero();
    entity.body.torque.setZero();
    entity.body.quaternion.set(0, 0, 0, 1);
    entity.fieldVelocity.setZero();
    entity.body.type = CANNON.Body.DYNAMIC;
    entity.body.mass = sphereMass(radius);
    entity.body.updateMassProperties();
    entity.finished = false;
    entity.visual.setFinished(false);
    entity.visual.setRadius(radius);
    entity.visual.clearTrail();
    syncEntityMesh(entity);
  });
}

/* --------------------------------------------------------------------- zones */

function allZones() {
  return [...(state.program ? state.program.zones : []), ...state.drawnZones];
}

function syncZoneVisuals() {
  const zones = allZones();
  const seen = new Set();

  zones.forEach((zone) => {
    seen.add(zone.id);
    const existing = state.zoneVisuals.get(zone.id);
    if (existing && existing.kind === zone.kind && existing.solid === zone.solid && existing.label === zone.label) {
      existing.visual.update(zone);
      existing.visual.setLabelVisible(dom.showLabels.checked);
      return;
    }
    if (existing) {
      stage.world.remove(existing.visual.group);
      existing.visual.dispose();
    }
    const visual = createZoneVisual(zone);
    visual.setLabelVisible(dom.showLabels.checked);
    stage.world.add(visual.group);
    state.zoneVisuals.set(zone.id, { visual, kind: zone.kind, solid: zone.solid, label: zone.label });
  });

  [...state.zoneVisuals.keys()].forEach((id) => {
    if (seen.has(id)) return;
    const entry = state.zoneVisuals.get(id);
    stage.world.remove(entry.visual.group);
    entry.visual.dispose();
    state.zoneVisuals.delete(id);
  });

  physics.syncSolidZones(
    zones
      .filter((zone) => zone.solid)
      .map((zone) => ({
        key: zone.id,
        kind: zone.kind,
        position: { x: zone.center.x, y: zone.center.z, z: zone.center.y },
        halfExtents: { x: zone.radii.x, y: zone.radii.z, z: zone.radii.y },
        signature: `${zone.kind}:${format(zone.radii.x)}:${format(zone.radii.y)}:${format(zone.radii.z)}`,
      })),
  );

  state.listsDirty = true;
}

function addDrawnZone(start, end) {
  const height = numberInput(dom.zoneHeight, 0.6);
  // Skip names the source already uses, so redrawing after a Clear cannot collide.
  const taken = new Set(allZones().map((zone) => zone.label));
  while (taken.has(`drawn${state.zoneCounter}`)) state.zoneCounter += 1;

  const zone = {
    id: `drawn-${state.zoneCounter}`,
    label: `drawn${state.zoneCounter}`,
    origin: 'drawn',
    kind: 'ellipsoid',
    center: {
      x: (start.x + end.x) / 2,
      y: (start.y + end.y) / 2,
      z: start.z,
    },
    radii: {
      x: Math.max(Math.abs(end.x - start.x) / 2, 0.1),
      y: Math.max(Math.abs(end.y - start.y) / 2, 0.1),
      z: height,
    },
    solid: dom.zoneSolid.checked,
    actions: [],
  };
  state.zoneCounter += 1;
  state.drawnZones.push(zone);
  dom.source.value = insertZoneSource(dom.source.value, zone);
  parseSource({ quiet: true });
  setStatus(`Added zone '${zone.label}' to the ZONES block.`, 'ok');
  return zone;
}

function eraseAt(event) {
  const pick = stage.pointerPick(event, [
    ...state.entities.flatMap((entity) => entity.visual.objects),
    ...[...state.zoneVisuals.values()].map((entry) => entry.visual.group),
  ]);
  if (!pick) {
    setStatus('Nothing to erase under the pointer.');
    return;
  }

  const entity = state.entities.find((candidate) => candidate.id === pick.pickId);
  if (entity) {
    removeEntity(entity);
    setStatus(`Removed ${entity.id}.`, 'ok');
    return;
  }

  const drawnIndex = state.drawnZones.findIndex((zone) => zone.id === pick.pickId);
  if (drawnIndex >= 0) {
    const [zone] = state.drawnZones.splice(drawnIndex, 1);
    syncZoneVisuals();
    setStatus(`Removed zone '${zone.label}'.`, 'ok');
    return;
  }

  const zone = (state.program ? state.program.zones : []).find((candidate) => candidate.id === pick.pickId);
  if (!zone) {
    setStatus('Nothing to erase under the pointer.');
    return;
  }

  const trimmed = removeZoneSource(dom.source.value, zone.label);
  if (!trimmed) {
    setStatus(`Could not locate '${zone.label}' in the ZONES block.`, 'error');
    return;
  }
  dom.source.value = trimmed;
  parseSource({ quiet: true });
  setStatus(`Removed zone '${zone.label}' from the program source.`, 'ok');
}

/* -------------------------------------------------------------------- parsing */

function parseSource({ quiet = false } = {}) {
  try {
    const parsed = parseNagare(dom.source.value);
    state.program = parsed;
    // Zones re-declared in the source take over from their drawn counterparts.
    const programLabels = new Set(parsed.zones.map((zone) => zone.label));
    state.drawnZones = state.drawnZones.filter((zone) => !programLabels.has(zone.label));
    syncZoneVisuals();
    if (!quiet) setStatus(`Parsed '${parsed.name}' with ${parsed.zones.length} zone(s).`, 'ok');
    return true;
  } catch (error) {
    state.program = null;
    syncZoneVisuals();
    setStatus(error.message, 'error');
    return false;
  }
}

/* ----------------------------------------------------------------- simulation */

function logEvent(entry) {
  state.events.push(entry);
  if (state.events.length > 200) state.events.shift();
  state.listsDirty = true;
}

function applyAction(entity, zone, action) {
  if (action.type === 'display') {
    logEvent({ type: 'display', text: `${entity.id} displayed "${action.message}" in ${zone.label} at step ${state.step}` });
  } else if (action.type === 'finish') {
    entity.finished = true;
    entity.visual.setFinished(true);
    entity.body.velocity.setZero();
    entity.body.angularVelocity.setZero();
    entity.fieldVelocity.setZero();
    // A finished trajectory stops for good: a massless static body neither moves
    // nor absorbs momentum from whatever runs into it.
    entity.body.type = CANNON.Body.STATIC;
    entity.body.mass = 0;
    entity.body.updateMassProperties();
    logEvent({ type: 'finish', text: `${entity.id} finished in ${zone.label} at step ${state.step}` });
    if (state.entities.every((candidate) => candidate.finished)) {
      pause('Every entity reached a finish zone.');
    }
  } else if (action.type === 'impulse') {
    toWorld(action.vector, scratchCannon);
    entity.body.applyImpulse(scratchCannon.scale(entity.body.mass, new CANNON.Vec3()));
    logEvent({ type: 'impulse', text: `${entity.id} took an impulse in ${zone.label} at step ${state.step}` });
  }
}

function checkZones() {
  const zones = allZones().filter((zone) => zone.actions.length);
  if (!zones.length) return;

  state.entities.forEach((entity) => {
    if (entity.finished) return;
    const point = fromWorld(entity.body.position);
    zones.forEach((zone) => {
      const key = `${entity.id}:${zone.id}`;
      if (state.triggered.has(key) || !zoneContains(zone, point)) return;
      state.triggered.add(key);
      zone.actions.forEach((action) => applyAction(entity, zone, action));
    });
  });
}

/** Re-evaluate the map and cache the velocity the field wants for each entity. */
function advanceField(config) {
  state.entities.forEach((entity) => {
    if (entity.finished) return;
    const point = fromWorld(entity.body.position);
    const target = state.program.field(point.x, point.y, point.z, state.step);
    toWorld(target, scratchCannon);

    // The map gives the next position, so the field velocity is the delta over one step.
    entity.fieldVelocity.set(
      (scratchCannon.x - entity.body.position.x) / config.stepInterval,
      (scratchCannon.y - entity.body.position.y) / config.stepInterval,
      (scratchCannon.z - entity.body.position.z) / config.stepInterval,
    );
  });
}

/**
 * Couple the cached field velocity into the rigid bodies. Runs once per physics
 * substep so the force is integrated over the whole frame, not just one substep.
 */
function applyCoupling(config) {
  state.entities.forEach((entity) => {
    if (entity.finished) return;
    if (config.coupling === 'flow') {
      entity.body.velocity.copy(entity.fieldVelocity);
      return;
    }
    entity.fieldVelocity.vsub(entity.body.velocity, scratchForce);
    scratchForce.scale(entity.body.mass * config.response, scratchForce);
    entity.body.applyForce(scratchForce);
  });
}

function simulationTick(delta) {
  const config = settings();

  if (state.running && state.program) {
    state.stepAccumulator += delta;
    let iterations = 0;
    while (state.stepAccumulator >= config.stepInterval && iterations < 8) {
      if (state.step >= config.stepLimit) {
        pause(`Reached the ${config.stepLimit} step limit.`);
        break;
      }
      try {
        advanceField(config);
      } catch (error) {
        pause(error.message, 'error');
        break;
      }
      state.step += 1;
      state.stepAccumulator -= config.stepInterval;
      iterations += 1;
    }
    if (iterations >= 8) state.stepAccumulator = 0;
  }

  if (!state.running) return;

  physics.step(delta, () => applyCoupling(config));
  checkZones();

  const showTrails = dom.showTrails.checked;
  state.entities.forEach((entity) => {
    syncEntityMesh(entity);
    if (showTrails && !entity.finished) {
      entity.visual.pushTrailPoint(entity.visual.mesh.position);
    }
  });

  if (dom.followEntities.checked && state.entities.length) {
    stage.lookAtPoints(state.entities.map((entity) => entity.visual.mesh.position));
  }
}

function play() {
  if (!state.program && !parseSource()) return;
  if (!state.entities.length) {
    setStatus('Add at least one entity before running.', 'error');
    return;
  }
  if (state.entities.every((entity) => entity.finished)) restart({ quiet: true });
  state.running = true;
  dom.run.textContent = 'Pause';
  dom.run.classList.add('running');
  setStatus(`Running '${state.program.name}'.`, 'ok');
}

function pause(message, kind = 'ok') {
  state.running = false;
  dom.run.textContent = 'Run Nagare';
  dom.run.classList.remove('running');
  if (message) setStatus(message, kind);
}

function toggleRun() {
  if (state.running) pause('Paused.');
  else play();
}

function restart({ quiet = false } = {}) {
  state.running = false;
  dom.run.textContent = 'Run Nagare';
  dom.run.classList.remove('running');
  state.step = 0;
  state.stepAccumulator = 0;
  state.triggered.clear();
  state.events = [];
  resetEntities();
  state.listsDirty = true;
  if (!quiet) setStatus('Run reset. Entities are back at their start positions.');
}

function clearScene() {
  restart({ quiet: true });
  clearEntities();
  state.drawnZones = [];
  state.zoneCounter = 1;
  syncZoneVisuals();
  setStatus('Scene cleared. Pick the Entity tool and click the build plane to start again.');
}

/* ---------------------------------------------------------------------- lists */

function renderLists() {
  dom.entityList.innerHTML = '';
  state.entities.forEach((entity) => {
    const point = fromWorld(entity.body.position);
    const item = document.createElement('li');
    item.innerHTML =
      `<strong>${entity.id}</strong>${entity.finished ? ' <em>finished</em>' : ''}` +
      `<br />(${format(point.x)}, ${format(point.y)}, ${format(point.z)})`;
    item.style.borderColor = `#${entity.visual.color.getHexString()}66`;
    dom.entityList.appendChild(item);
  });
  if (!dom.entityList.children.length) {
    dom.entityList.innerHTML = '<li>No entities yet. Use the Entity tool.</li>';
  }

  dom.zoneList.innerHTML = '';
  allZones().forEach((zone) => {
    const item = document.createElement('li');
    const shape = zone.kind === 'box' ? 'Box' : 'Ellipsoid';
    const actions = zone.actions.map((action) => action.type).join(', ') || 'no action';
    item.innerHTML =
      `<strong>${zone.label}</strong> ${shape}${zone.solid ? ' · solid' : ''}<br />` +
      `centre=(${format(zone.center.x)}, ${format(zone.center.y)}, ${format(zone.center.z)}) ` +
      `size=(${format(zone.radii.x)}, ${format(zone.radii.y)}, ${format(zone.radii.z)})<br />${actions}`;
    dom.zoneList.appendChild(item);
  });
  if (!dom.zoneList.children.length) {
    dom.zoneList.innerHTML = '<li>No zones yet. Drag with the Zone tool or add one to ZONES.</li>';
  }

  dom.eventLog.innerHTML = '';
  state.events.forEach((event) => {
    const item = document.createElement('li');
    item.className = event.type;
    item.textContent = event.text;
    dom.eventLog.appendChild(item);
  });
  if (!dom.eventLog.children.length) dom.eventLog.innerHTML = '<li>No events recorded.</li>';
}

/* ---------------------------------------------------------------------- tools */

function setTool(tool) {
  state.tool = tool;
  dom.tools.forEach((button) => button.classList.toggle('active', button.dataset.tool === tool));
  stage.setOrbitDrag(tool === 'orbit');
  marker.setVisible(tool !== 'orbit');
  dom.canvas.dataset.tool = tool;
}

function onPointerDown(event) {
  if (state.tool === 'orbit' || event.button !== 0) return;
  const point = stage.pointerOnPlane(event, numberInput(dom.placeHeight, 0));

  if (state.tool === 'erase') {
    eraseAt(event);
    return;
  }
  if (!point) return;

  if (state.tool === 'entity') {
    const entity = createEntity(point);
    setStatus(`Placed ${entity.id} at (${format(point.x)}, ${format(point.y)}, ${format(point.z)}).`, 'ok');
    return;
  }

  state.drag = { start: point, current: point };
  dom.canvas.setPointerCapture(event.pointerId);
}

function onPointerMove(event) {
  if (!state.drag) return;
  const point = stage.pointerOnPlane(event, numberInput(dom.placeHeight, 0));
  if (!point) return;
  state.drag.current = point;
  preview.show(
    {
      x: (state.drag.start.x + point.x) / 2,
      y: (state.drag.start.y + point.y) / 2,
      z: state.drag.start.z,
    },
    {
      x: Math.max(Math.abs(point.x - state.drag.start.x) / 2, 0.1),
      y: Math.max(Math.abs(point.y - state.drag.start.y) / 2, 0.1),
      z: numberInput(dom.zoneHeight, 0.6),
    },
  );
}

function onPointerUp(event) {
  if (!state.drag) return;
  const point = stage.pointerOnPlane(event, numberInput(dom.placeHeight, 0)) || state.drag.current;
  preview.hide();
  if (dom.canvas.hasPointerCapture(event.pointerId)) dom.canvas.releasePointerCapture(event.pointerId);
  const { start } = state.drag;
  state.drag = null;
  if (Math.abs(point.x - start.x) < 0.05 && Math.abs(point.y - start.y) < 0.05) return;
  addDrawnZone(start, point);
}

/* -------------------------------------------------------------------- wiring */

function bindRange(input, output, apply, digits = 2) {
  const handler = () => {
    const value = Number(input.value);
    if (output) output.textContent = value.toFixed(digits);
    apply(value);
  };
  input.addEventListener('input', handler);
  handler();
}

function applyArena() {
  const size = numberInput(dom.arenaSize, 14);
  physics.setArena(size, dom.enableWalls.checked, dom.enableFloor.checked);
  stage.setArena(size, dom.enableWalls.checked);
}

dom.tools.forEach((button) => button.addEventListener('click', () => setTool(button.dataset.tool)));
dom.canvas.addEventListener('pointerdown', onPointerDown);
dom.canvas.addEventListener('pointermove', onPointerMove);
dom.canvas.addEventListener('pointerup', onPointerUp);
dom.canvas.addEventListener('pointercancel', () => {
  state.drag = null;
  preview.hide();
});
dom.canvas.addEventListener('contextmenu', (event) => event.preventDefault());

dom.run.addEventListener('click', toggleRun);
dom.restart.addEventListener('click', () => restart());
dom.clear.addEventListener('click', clearScene);
dom.loadExample.addEventListener('click', () => loadExample());

// Re-parsing on every keystroke would rebuild every zone mesh and label texture,
// so wait for a short pause in typing first.
let parseTimer = 0;
dom.source.addEventListener('input', () => {
  window.clearTimeout(parseTimer);
  parseTimer = window.setTimeout(() => {
    parseSource();
    state.triggered.clear();
  }, 220);
});

bindRange(dom.gravity, document.getElementById('gravityValue'), (value) => physics.setGravity(value), 1);
bindRange(dom.restitution, document.getElementById('restitutionValue'), (value) => physics.setRestitution(value));
bindRange(dom.damping, document.getElementById('dampingValue'), (value) => {
  state.entities.forEach((entity) => {
    entity.body.linearDamping = value;
  });
});
bindRange(dom.entityRadius, document.getElementById('entityRadiusValue'), (value) => {
  state.entities.forEach((entity) => {
    entity.body.shapes[0].radius = value;
    entity.body.shapes[0].updateBoundingSphereRadius();
    entity.body.updateBoundingRadius();
    // A finished entity is a massless static body; resizing must not revive it.
    entity.body.mass = entity.finished ? 0 : sphereMass(value);
    entity.body.updateMassProperties();
    entity.visual.setRadius(value);
  });
});
bindRange(dom.fieldResponse, document.getElementById('fieldResponseValue'), () => {}, 1);
bindRange(dom.arenaSize, document.getElementById('arenaSizeValue'), applyArena, 0);
bindRange(dom.placeHeight, document.getElementById('placeHeightValue'), (value) => {
  marker.update(value, Math.max(numberInput(dom.arenaSize, 14), 12) * 0.45);
}, 1);
bindRange(dom.zoneHeight, document.getElementById('zoneHeightValue'), () => {}, 1);

dom.enableFloor.addEventListener('change', applyArena);
dom.enableWalls.addEventListener('change', applyArena);
dom.showTrails.addEventListener('change', () => {
  state.entities.forEach((entity) => entity.visual.setTrailVisible(dom.showTrails.checked));
});
dom.showLabels.addEventListener('change', () => {
  state.zoneVisuals.forEach((entry) => entry.visual.setLabelVisible(dom.showLabels.checked));
});
dom.showShadows.addEventListener('change', () => stage.setShadows(dom.showShadows.checked));
dom.showGrid.addEventListener('change', () => {
  stage.grid.visible = dom.showGrid.checked;
  stage.ground.visible = dom.showGrid.checked;
});
dom.viewReset.addEventListener('click', () => stage.setView('reset'));
dom.viewTop.addEventListener('click', () => stage.setView('top'));
dom.viewFront.addEventListener('click', () => stage.setView('front'));

window.addEventListener('resize', () => stage.resize());
window.addEventListener('keydown', (event) => {
  if (event.target instanceof HTMLTextAreaElement || event.target instanceof HTMLInputElement) return;
  if (event.code === 'Space') {
    event.preventDefault();
    toggleRun();
  }
  if (event.key === 'r' || event.key === 'R') restart();
});

/* ---------------------------------------------------------------- entry point */

function loadExample() {
  clearScene();
  dom.source.value = EXAMPLE_PROGRAM;
  parseSource();
  DEFAULT_ENTITY_STARTS.forEach(createEntity);
  restart({ quiet: true });
  stage.setView('reset');
  setStatus('Loaded the 3D example. Press Run (or space) to let the rigid body world take over.', 'ok');
}

let lastFrame = performance.now();

stage.renderer.setAnimationLoop(() => {
  const now = performance.now();
  const delta = Math.min((now - lastFrame) / 1000, 0.1);
  lastFrame = now;
  simulationTick(delta);

  state.frameCount += 1;
  state.fpsClock += delta;
  if (state.fpsClock >= 0.5) {
    dom.hudFps.textContent = `${Math.round(state.frameCount / state.fpsClock)} fps`;
    state.frameCount = 0;
    state.fpsClock = 0;
  }
  dom.hudStep.textContent = `step ${state.step}`;
  dom.hudBodies.textContent = `${state.entities.length} ${state.entities.length === 1 ? 'body' : 'bodies'}`;

  state.listClock += delta;
  if (state.listsDirty || (state.running && state.listClock >= 0.15)) {
    state.listsDirty = false;
    state.listClock = 0;
    renderLists();
  }

  stage.render();
});

const resizeObserver = new ResizeObserver(() => stage.resize());
resizeObserver.observe(dom.canvas.parentElement);

setTool('orbit');
applyArena();
loadExample();
window.__nagareReady = true;

// Exposed for debugging from the console and for the docs tests.
window.nagarePlayground = { state, stage, physics, parseSource, play, pause, restart, clearScene };
