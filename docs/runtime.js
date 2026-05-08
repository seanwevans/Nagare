const programSource = document.getElementById('programSource');
const runButton = document.getElementById('runProgram');
const loadExampleButton = document.getElementById('loadExample');
const resetButton = document.getElementById('resetRuntime');
const stepLimitInput = document.getElementById('stepLimit');
const frameDelayInput = document.getElementById('frameDelay');
const statusBox = document.getElementById('status');
const canvas = document.getElementById('runtimeCanvas');
const ctx = canvas.getContext('2d');
const zoneList = document.getElementById('zoneList');
const entityList = document.getElementById('entityList');
const eventLog = document.getElementById('eventLog');
const toolButtons = [...document.querySelectorAll('.tool')];

const EXAMPLE = `BEGIN {
  program spiral { x + 0.08 - y * 0.08, y + x * 0.08 }
}

ZONES {
  hello { Ellipse((1.25, 0.25), 0.75, 0.55) }
  done { Ellipse((2.25, 1.2), 0.65, 0.65) }
}

EXECUTE {
  spiral<hello> { display "Hello from a drawn zone" }
  spiral<done> { finish }
}`;

const state = {
  tool: 'entity',
  scale: 80,
  entities: [{ id: 'entity-1', x: 0, y: 0 }],
  entityCounter: 2,
  drawnZones: [],
  zoneCounter: 1,
  parsedZones: [],
  paths: new Map(),
  currentFrame: 0,
  frames: [],
  animationTimer: null,
  dragStart: null,
  dragCurrent: null,
};

const allowedFunctions = {
  sin: Math.sin,
  cos: Math.cos,
  tan: Math.tan,
  asin: Math.asin,
  acos: Math.acos,
  atan: Math.atan,
  atan2: Math.atan2,
  sqrt: Math.sqrt,
  abs: Math.abs,
  log: Math.log,
  exp: Math.exp,
  min: Math.min,
  max: Math.max,
  hypot: Math.hypot,
};

function setStatus(message, kind = '') {
  statusBox.textContent = message;
  statusBox.className = kind;
}

function setTool(tool) {
  state.tool = tool;
  toolButtons.forEach((button) => button.classList.toggle('active', button.dataset.tool === tool));
}

toolButtons.forEach((button) => button.addEventListener('click', () => setTool(button.dataset.tool)));

function cssCanvasSize() {
  const ratio = window.devicePixelRatio || 1;
  return { width: canvas.width / ratio, height: canvas.height / ratio };
}

function resizeCanvas() {
  const ratio = window.devicePixelRatio || 1;
  const rect = canvas.getBoundingClientRect();
  const width = Math.max(640, Math.round(rect.width || 900));
  const height = Math.max(420, Math.round(width * 0.68));
  canvas.width = width * ratio;
  canvas.height = height * ratio;
  ctx.setTransform(ratio, 0, 0, ratio, 0, 0);
  draw();
}

function worldToCanvas(x, y) {
  const { width, height } = cssCanvasSize();
  return { x: width / 2 + x * state.scale, y: height / 2 - y * state.scale };
}

function canvasToWorld(clientX, clientY) {
  const rect = canvas.getBoundingClientRect();
  const { width, height } = cssCanvasSize();
  return {
    x: (clientX - rect.left - width / 2) / state.scale,
    y: (height / 2 - (clientY - rect.top)) / state.scale,
  };
}

function makeSafeExpression(expr) {
  if (!expr || !expr.trim()) throw new Error('Program expressions cannot be empty.');
  if (expr.length > 180) throw new Error(`Expression is too long: ${expr}`);
  if (!/^[\d\sxytepiPI.,_+\-*/%()^A-Za-z]+$/.test(expr)) {
    throw new Error(`Unsupported characters in expression: ${expr}`);
  }
  const jsExpr = expr.replace(/\bpi\b/gi, 'PI').replace(/\be\b/g, 'E').replace(/\^/g, '**');
  const names = [...jsExpr.matchAll(/[A-Za-z_]\w*/g)].map((match) => match[0]);
  const allowed = new Set(['x', 'y', 't', 'PI', 'E', ...Object.keys(allowedFunctions)]);
  const unknown = names.find((name) => !allowed.has(name));
  if (unknown) throw new Error(`Unknown identifier '${unknown}' in expression '${expr}'.`);
  const fn = new Function('x', 'y', 't', 'PI', 'E', ...Object.keys(allowedFunctions), `return (${jsExpr});`);
  return (x, y, t) => {
    const value = fn(x, y, t, Math.PI, Math.E, ...Object.values(allowedFunctions));
    if (!Number.isFinite(value)) throw new Error(`Expression '${expr}' produced a non-finite value.`);
    return Number(value);
  };
}

function parseProgramExpressions(source) {
  const header = source.match(/program\s+(?:\w+\s+)?\{/im);
  if (!header) throw new Error('Could not find a program { x-expression, y-expression } block.');
  const bodyStart = header.index + header[0].length;
  let depth = 0;
  for (let i = bodyStart; i < source.length; i += 1) {
    const char = source[i];
    if (char === '(') depth += 1;
    if (char === ')') depth -= 1;
    if (char === '}' && depth === 0) {
      const body = source.slice(bodyStart, i);
      let commaIndex = -1;
      let commaDepth = 0;
      for (let j = 0; j < body.length; j += 1) {
        const innerChar = body[j];
        if (innerChar === '(') commaDepth += 1;
        if (innerChar === ')') commaDepth -= 1;
        if (innerChar === ',' && commaDepth === 0) {
          commaIndex = j;
          break;
        }
      }
      if (commaIndex < 0) throw new Error('Program block must contain two expressions separated by a comma.');
      return [body.slice(0, commaIndex).trim(), body.slice(commaIndex + 1).trim()];
    }
  }
  throw new Error('Program block is missing its closing brace.');
}

function parseNagare(source) {
  const [fxExpr, fyExpr] = parseProgramExpressions(source);

  const zoneMap = new Map();
  const zonePattern = /(\w+)\s*\{\s*Ellipse\s*\(\s*\(\s*([^,()]+)\s*,\s*([^()]+?)\s*\)\s*,\s*([^,()]+)\s*,\s*([^()]+?)\s*\)\s*\}/gim;
  let zoneMatch;
  while ((zoneMatch = zonePattern.exec(source)) !== null) {
    const [, name, cx, cy, a, b] = zoneMatch;
    const zone = { id: `source-${name}`, label: name, cx: Number(cx), cy: Number(cy), a: Number(a), b: Number(b), action: null };
    if (![zone.cx, zone.cy, zone.a, zone.b].every(Number.isFinite) || zone.a <= 0 || zone.b <= 0) {
      throw new Error(`Zone '${name}' has invalid ellipse parameters.`);
    }
    zoneMap.set(name, zone);
  }

  const executePattern = /\w+<([A-Za-z_]\w*)>\s*\{\s*(display\s+"([^"]*)"|finish)\s*\}/gim;
  let executeMatch;
  while ((executeMatch = executePattern.exec(source)) !== null) {
    const [, zoneName, actionText, message] = executeMatch;
    const zone = zoneMap.get(zoneName);
    if (!zone) throw new Error(`EXECUTE references unknown zone '${zoneName}'.`);
    zone.action = actionText.startsWith('display') ? { type: 'display', message } : { type: 'finish' };
  }

  return {
    fx: makeSafeExpression(fxExpr),
    fy: makeSafeExpression(fyExpr),
    zones: [...zoneMap.values()],
  };
}

function zoneContains(zone, x, y) {
  const dx = (x - zone.cx) / zone.a;
  const dy = (y - zone.cy) / zone.b;
  return dx * dx + dy * dy <= 1;
}

function runNagare(source, initialEntities, stepLimit) {
  const parsed = parseNagare(source);
  state.parsedZones = parsed.zones;
  const entities = initialEntities.map((entity) => ({ ...entity }));
  const frames = [{ step: 0, entities: entities.map((entity) => ({ ...entity })) }];
  const events = [];
  const triggered = new Set();
  let finished = false;

  for (let step = 1; step <= stepLimit && !finished; step += 1) {
    entities.forEach((entity) => {
      const x = parsed.fx(entity.x, entity.y, step - 1);
      const y = parsed.fy(entity.x, entity.y, step - 1);
      entity.x = x;
      entity.y = y;
    });

    frames.push({ step, entities: entities.map((entity) => ({ ...entity })) });

    for (const entity of entities) {
      for (const zone of parsed.zones) {
        const key = `${entity.id}:${zone.id}`;
        if (zone.action && !triggered.has(key) && zoneContains(zone, entity.x, entity.y)) {
          triggered.add(key);
          const event = { step, entityId: entity.id, zoneLabel: zone.label, ...zone.action };
          events.push(event);
          if (zone.action.type === 'finish') finished = true;
        }
      }
    }
  }

  return { frames, events, finished };
}

function ellipseToNagare(zone) {
  return `  ${zone.label} { Ellipse((${format(zone.cx)}, ${format(zone.cy)}), ${format(zone.a)}, ${format(zone.b)}) }`;
}

function format(value) {
  return Number(value.toFixed(3)).toString();
}

function insertDrawnZone(zone) {
  const source = programSource.value;
  const zoneLine = ellipseToNagare(zone);
  const zonesMatch = source.match(/ZONES\s*\{[\s\S]*?\}/i);
  if (zonesMatch) {
    const insertAt = zonesMatch.index + zonesMatch[0].lastIndexOf('}');
    programSource.value = `${source.slice(0, insertAt).trimEnd()}\n${zoneLine}\n${source.slice(insertAt)}`;
    return;
  }
  programSource.value = `${source.trim()}\n\nZONES {\n${zoneLine}\n}\n`;
}

function addEntity(point) {
  state.entities.push({ id: `entity-${state.entityCounter}`, x: point.x, y: point.y });
  state.entityCounter += 1;
  clearRuntimeData();
  draw();
  renderLists();
}

function addDrawnZone(start, end) {
  const cx = (start.x + end.x) / 2;
  const cy = (start.y + end.y) / 2;
  const a = Math.max(Math.abs(end.x - start.x) / 2, 0.08);
  const b = Math.max(Math.abs(end.y - start.y) / 2, 0.08);
  const zone = { id: `drawn-zone-${state.zoneCounter}`, label: `drawn${state.zoneCounter}`, cx, cy, a, b };
  state.zoneCounter += 1;
  state.drawnZones.push(zone);
  insertDrawnZone(zone);
  clearRuntimeData();
  renderLists();
  draw();
}

function eraseAt(point) {
  const entityIndex = state.entities.findIndex((entity) => Math.hypot(entity.x - point.x, entity.y - point.y) < 0.16);
  if (entityIndex >= 0) {
    state.entities.splice(entityIndex, 1);
  } else {
    const zoneIndex = state.drawnZones.findIndex((zone) => zoneContains(zone, point.x, point.y));
    if (zoneIndex >= 0) state.drawnZones.splice(zoneIndex, 1);
  }
  clearRuntimeData();
  renderLists();
  draw();
}

function clearRuntimeData() {
  window.clearTimeout(state.animationTimer);
  state.animationTimer = null;
  state.frames = [];
  state.paths = new Map();
  state.currentFrame = 0;
}

function canvasPointerDown(event) {
  const point = canvasToWorld(event.clientX, event.clientY);
  if (state.tool === 'entity') {
    addEntity(point);
  } else if (state.tool === 'erase') {
    eraseAt(point);
  } else {
    state.dragStart = point;
    state.dragCurrent = point;
    canvas.setPointerCapture(event.pointerId);
    draw();
  }
}

function canvasPointerMove(event) {
  if (!state.dragStart || state.tool !== 'ellipse') return;
  state.dragCurrent = canvasToWorld(event.clientX, event.clientY);
  draw();
}

function canvasPointerUp(event) {
  if (!state.dragStart || state.tool !== 'ellipse') return;
  const end = canvasToWorld(event.clientX, event.clientY);
  addDrawnZone(state.dragStart, end);
  state.dragStart = null;
  state.dragCurrent = null;
  canvas.releasePointerCapture(event.pointerId);
}

function drawGrid() {
  const { width, height } = cssCanvasSize();
  ctx.clearRect(0, 0, width, height);
  ctx.fillStyle = '#07111f';
  ctx.fillRect(0, 0, width, height);
  ctx.strokeStyle = 'rgba(148, 163, 184, 0.12)';
  ctx.lineWidth = 1;
  const grid = state.scale;
  for (let x = width / 2 % grid; x < width; x += grid) {
    ctx.beginPath(); ctx.moveTo(x, 0); ctx.lineTo(x, height); ctx.stroke();
  }
  for (let y = height / 2 % grid; y < height; y += grid) {
    ctx.beginPath(); ctx.moveTo(0, y); ctx.lineTo(width, y); ctx.stroke();
  }
  ctx.strokeStyle = 'rgba(226, 232, 240, 0.28)';
  const origin = worldToCanvas(0, 0);
  ctx.beginPath(); ctx.moveTo(0, origin.y); ctx.lineTo(width, origin.y); ctx.moveTo(origin.x, 0); ctx.lineTo(origin.x, height); ctx.stroke();
}

function drawEllipse(zone, color = 'rgba(34, 211, 238, 0.92)', fill = 'rgba(34, 211, 238, 0.08)') {
  const center = worldToCanvas(zone.cx, zone.cy);
  ctx.save();
  ctx.strokeStyle = color;
  ctx.fillStyle = fill;
  ctx.lineWidth = 2;
  ctx.beginPath();
  ctx.ellipse(center.x, center.y, zone.a * state.scale, zone.b * state.scale, 0, 0, Math.PI * 2);
  ctx.fill();
  ctx.stroke();
  ctx.fillStyle = color;
  ctx.font = '12px sans-serif';
  ctx.fillText(zone.label, center.x + 8, center.y - 8);
  ctx.restore();
}

function drawPaths() {
  ctx.save();
  ctx.lineWidth = 2.5;
  ctx.lineJoin = 'round';
  ctx.lineCap = 'round';
  ctx.strokeStyle = 'rgba(167, 139, 250, 0.78)';
  state.paths.forEach((points) => {
    ctx.beginPath();
    points.slice(0, state.currentFrame + 1).forEach((point, index) => {
      const canvasPoint = worldToCanvas(point.x, point.y);
      if (index === 0) ctx.moveTo(canvasPoint.x, canvasPoint.y);
      else ctx.lineTo(canvasPoint.x, canvasPoint.y);
    });
    ctx.stroke();
  });
  ctx.restore();
}

function drawEntities(entities = state.entities) {
  entities.forEach((entity) => {
    const point = worldToCanvas(entity.x, entity.y);
    ctx.save();
    ctx.fillStyle = '#fb923c';
    ctx.strokeStyle = 'rgba(255, 255, 255, 0.75)';
    ctx.lineWidth = 2;
    ctx.beginPath();
    ctx.arc(point.x, point.y, 6, 0, Math.PI * 2);
    ctx.fill();
    ctx.stroke();
    ctx.restore();
  });
}

function drawDragPreview() {
  if (!state.dragStart || !state.dragCurrent) return;
  const preview = {
    label: 'preview',
    cx: (state.dragStart.x + state.dragCurrent.x) / 2,
    cy: (state.dragStart.y + state.dragCurrent.y) / 2,
    a: Math.max(Math.abs(state.dragCurrent.x - state.dragStart.x) / 2, 0.08),
    b: Math.max(Math.abs(state.dragCurrent.y - state.dragStart.y) / 2, 0.08),
  };
  drawEllipse(preview, 'rgba(253, 224, 71, 0.95)', 'rgba(253, 224, 71, 0.08)');
}

function draw() {
  drawGrid();
  const parsedZonesById = new Set(state.parsedZones.map((zone) => zone.id));
  state.parsedZones.forEach((zone) => drawEllipse(zone));
  state.drawnZones.filter((zone) => !parsedZonesById.has(`source-${zone.label}`)).forEach((zone) => drawEllipse(zone, 'rgba(94, 234, 212, 0.65)'));
  drawDragPreview();
  if (state.frames.length) {
    drawPaths();
    drawEntities(state.frames[state.currentFrame].entities);
  } else {
    drawEntities();
  }
}

function renderLists(events = []) {
  entityList.innerHTML = '';
  (state.frames[state.currentFrame]?.entities || state.entities).forEach((entity) => {
    const item = document.createElement('li');
    item.innerHTML = `<strong>${entity.id}</strong> (${format(entity.x)}, ${format(entity.y)})`;
    entityList.appendChild(item);
  });
  if (!entityList.children.length) entityList.innerHTML = '<li>No entities yet.</li>';

  zoneList.innerHTML = '';
  const zones = state.parsedZones.length ? state.parsedZones : state.drawnZones;
  zones.forEach((zone) => {
    const item = document.createElement('li');
    item.innerHTML = `<strong>${zone.label}</strong> center=(${format(zone.cx)}, ${format(zone.cy)}), axes=(${format(zone.a)}, ${format(zone.b)})`;
    zoneList.appendChild(item);
  });
  if (!zoneList.children.length) zoneList.innerHTML = '<li>No zones yet. Draw an ellipse or add one to the source.</li>';

  eventLog.innerHTML = '';
  events.forEach((event) => {
    const item = document.createElement('li');
    item.className = event.type;
    item.textContent = event.type === 'display'
      ? `${event.entityId} displayed "${event.message}" in ${event.zoneLabel} at step ${event.step}`
      : `${event.entityId} finished in ${event.zoneLabel} at step ${event.step}`;
    eventLog.appendChild(item);
  });
  if (!eventLog.children.length) eventLog.innerHTML = '<li>No events recorded.</li>';
}

function preparePaths(frames) {
  const paths = new Map();
  frames[0].entities.forEach((entity) => paths.set(entity.id, []));
  frames.forEach((frame) => {
    frame.entities.forEach((entity) => paths.get(entity.id)?.push({ x: entity.x, y: entity.y }));
  });
  state.paths = paths;
}

function animate() {
  draw();
  renderLists(state.lastEvents || []);
  if (state.currentFrame >= state.frames.length - 1) return;
  state.currentFrame += 1;
  state.animationTimer = window.setTimeout(animate, Number(frameDelayInput.value) || 0);
}

function runProgram() {
  try {
    clearRuntimeData();
    if (!state.entities.length) throw new Error('Add at least one entity before running.');
    const stepLimit = Number(stepLimitInput.value);
    if (!Number.isInteger(stepLimit) || stepLimit < 1 || stepLimit > 2000) throw new Error('Step limit must be between 1 and 2000.');
    const result = runNagare(programSource.value, state.entities, stepLimit);
    state.frames = result.frames;
    state.lastEvents = result.events;
    state.currentFrame = 0;
    preparePaths(result.frames);
    renderLists(result.events);
    setStatus(`Ran ${result.frames.length - 1} step(s)${result.finished ? ' and reached finish.' : '.'}`, 'ok');
    animate();
  } catch (error) {
    clearRuntimeData();
    state.parsedZones = [];
    setStatus(error.message, 'error');
    renderLists([]);
    draw();
  }
}

function loadExample() {
  clearRuntimeData();
  state.entities = [{ id: 'entity-1', x: 0, y: 0 }];
  state.entityCounter = 2;
  state.drawnZones = [];
  state.zoneCounter = 1;
  state.parsedZones = [];
  state.lastEvents = [];
  programSource.value = EXAMPLE;
  setStatus('Loaded the browser-only Nagare example. Drag an ellipse to add a zone.', 'ok');
  try {
    state.parsedZones = parseNagare(programSource.value).zones;
  } catch (_) {
    state.parsedZones = [];
  }
  renderLists([]);
  draw();
}

function resetRuntime() {
  clearRuntimeData();
  state.entities = [];
  state.entityCounter = 1;
  state.drawnZones = [];
  state.zoneCounter = 1;
  state.parsedZones = [];
  state.lastEvents = [];
  setStatus('Runtime reset. Add an entity and draw zones to begin.');
  renderLists([]);
  draw();
}

canvas.addEventListener('pointerdown', canvasPointerDown);
canvas.addEventListener('pointermove', canvasPointerMove);
canvas.addEventListener('pointerup', canvasPointerUp);
canvas.addEventListener('pointercancel', () => { state.dragStart = null; state.dragCurrent = null; draw(); });
runButton.addEventListener('click', runProgram);
loadExampleButton.addEventListener('click', loadExample);
resetButton.addEventListener('click', resetRuntime);
programSource.addEventListener('input', () => {
  try {
    state.parsedZones = parseNagare(programSource.value).zones;
    setStatus('Program parsed. Ready to run.', 'ok');
  } catch (error) {
    state.parsedZones = [];
    setStatus(error.message, 'error');
  }
  renderLists(state.lastEvents || []);
  draw();
});
window.addEventListener('resize', resizeCanvas);

programSource.value = EXAMPLE;
try { state.parsedZones = parseNagare(programSource.value).zones; } catch (_) { state.parsedZones = []; }
resizeCanvas();
renderLists([]);
setStatus('Ready. Click to add entities, drag with the Ellipse tool to draw zones, then run Nagare.');
