const fxInput = document.getElementById('fx');
const fyInput = document.getElementById('fy');
const durationInput = document.getElementById('duration');
const dtInput = document.getElementById('dt');
const zoneLabelInput = document.getElementById('zoneLabel');
const zoneTypeSelect = document.getElementById('zoneType');
const circleParams = document.getElementById('circleParams');
const rectParams = document.getElementById('rectParams');
const addZoneButton = document.getElementById('addZone');
const zoneList = document.getElementById('zoneList');
const resetEntitiesButton = document.getElementById('resetEntities');
const resetAllButton = document.getElementById('resetAll');
const runButton = document.getElementById('runSimulation');
const errorBox = document.getElementById('error');
const eventLog = document.getElementById('eventLog');
const canvas = document.getElementById('scene');
const ctx = canvas.getContext('2d');

const state = {
  zones: [],
  entities: [],
  zoneCounter: 1,
  entityCounter: 1,
  animation: {
    frames: [],
    dt: 0.05,
    playing: false,
    startTime: null,
    currentFrame: 0,
    trajectories: new Map(),
  },
  scale: 50,
};

function resizeCanvas() {
  const ratio = window.devicePixelRatio || 1;
  const displayWidth = canvas.clientWidth || canvas.width;
  const displayHeight = canvas.clientHeight || canvas.height;
  canvas.width = displayWidth * ratio;
  canvas.height = displayHeight * ratio;
  ctx.setTransform(ratio, 0, 0, ratio, 0, 0);
}

resizeCanvas();
window.addEventListener('resize', () => {
  resizeCanvas();
  drawScene(state.animation.currentFrame || 0);
});

function worldToCanvas(x, y) {
  const centerX = (canvas.width / (window.devicePixelRatio || 1)) / 2;
  const centerY = (canvas.height / (window.devicePixelRatio || 1)) / 2;
  return {
    x: centerX + x * state.scale,
    y: centerY - y * state.scale,
  };
}

function canvasToWorld(px, py) {
  const rect = canvas.getBoundingClientRect();
  const x = px - rect.left;
  const y = py - rect.top;
  const centerX = canvas.clientWidth / 2;
  const centerY = canvas.clientHeight / 2;
  return {
    x: (x - centerX) / state.scale,
    y: (centerY - y) / state.scale,
  };
}

function toggleZoneParams() {
  const type = zoneTypeSelect.value;
  if (type === 'circle') {
    circleParams.classList.remove('hidden');
    rectParams.classList.add('hidden');
  } else {
    rectParams.classList.remove('hidden');
    circleParams.classList.add('hidden');
  }
}

zoneTypeSelect.addEventListener('change', toggleZoneParams);
toggleZoneParams();

function clearError() {
  errorBox.textContent = '';
}

function setError(message) {
  errorBox.textContent = message;
}

function renderZoneList() {
  zoneList.innerHTML = '';
  if (!state.zones.length) {
    const empty = document.createElement('li');
    empty.textContent = 'No zones defined yet.';
    zoneList.appendChild(empty);
    return;
  }
  state.zones.forEach((zone) => {
    const li = document.createElement('li');
    const summary = document.createElement('div');
    summary.textContent = `${zone.label} (${zone.type})`;
    const details = document.createElement('small');
    if (zone.type === 'circle') {
      details.textContent = `center=(${zone.cx.toFixed(2)}, ${zone.cy.toFixed(2)}), r=${zone.r.toFixed(2)}`;
    } else {
      details.textContent = `center=(${zone.cx.toFixed(2)}, ${zone.cy.toFixed(2)}), w=${zone.w.toFixed(2)}, h=${zone.h.toFixed(2)}`;
    }
    const removeBtn = document.createElement('button');
    removeBtn.textContent = 'Remove';
    removeBtn.type = 'button';
    removeBtn.addEventListener('click', () => {
      state.zones = state.zones.filter((z) => z.id !== zone.id);
      renderZoneList();
      drawScene(state.animation.currentFrame || 0);
    });

    li.appendChild(summary);
    li.appendChild(details);
    li.appendChild(removeBtn);
    zoneList.appendChild(li);
  });
}

function addZone() {
  const type = zoneTypeSelect.value;
  const label = zoneLabelInput.value.trim() || `Zone ${state.zoneCounter}`;
  const zone = { id: `zone-${state.zoneCounter++}`, label, type };
  try {
    if (type === 'circle') {
      zone.cx = parseFloat(document.getElementById('circleCx').value);
      zone.cy = parseFloat(document.getElementById('circleCy').value);
      zone.r = parseFloat(document.getElementById('circleR').value);
      if (!(zone.r > 0)) throw new Error('Radius must be positive');
    } else {
      zone.cx = parseFloat(document.getElementById('rectCx').value);
      zone.cy = parseFloat(document.getElementById('rectCy').value);
      zone.w = parseFloat(document.getElementById('rectW').value);
      zone.h = parseFloat(document.getElementById('rectH').value);
      if (!(zone.w > 0 && zone.h > 0)) throw new Error('Width/height must be positive');
    }
  } catch (err) {
    setError(err.message);
    return;
  }

  state.zones.push(zone);
  renderZoneList();
  zoneLabelInput.value = '';
  drawScene(state.animation.currentFrame || 0);
  clearError();
}

addZoneButton.addEventListener('click', addZone);

function addEntity(worldX, worldY) {
  const entity = {
    id: `entity-${state.entityCounter++}`,
    x: worldX,
    y: worldY,
  };
  state.entities.push(entity);
  drawScene(state.animation.currentFrame || 0);
}

canvas.addEventListener('click', (event) => {
  const { x, y } = canvasToWorld(event.clientX, event.clientY);
  addEntity(x, y);
});

resetEntitiesButton.addEventListener('click', () => {
  state.entities = [];
  state.entityCounter = 1;
  state.animation.frames = [];
  state.animation.playing = false;
  drawScene(0);
});

resetAllButton.addEventListener('click', () => {
  state.entities = [];
  state.zones = [];
  state.entityCounter = 1;
  state.zoneCounter = 1;
  state.animation.frames = [];
  state.animation.playing = false;
  renderZoneList();
  drawScene(0);
});

function buildPayload() {
  return {
    vectorField: {
      fx: fxInput.value.trim(),
      fy: fyInput.value.trim(),
    },
    zones: state.zones.map((zone) => {
      if (zone.type === 'circle') {
        return {
          id: zone.id,
          label: zone.label,
          type: 'circle',
          cx: zone.cx,
          cy: zone.cy,
          r: zone.r,
        };
      }
      return {
        id: zone.id,
        label: zone.label,
        type: 'rect',
        cx: zone.cx,
        cy: zone.cy,
        w: zone.w,
        h: zone.h,
      };
    }),
    entities: state.entities.map((entity) => ({
      id: entity.id,
      x: entity.x,
      y: entity.y,
    })),
    settings: {
      duration: parseFloat(durationInput.value),
      dt: parseFloat(dtInput.value),
    },
  };
}

function drawBackground() {
  const width = canvas.width / (window.devicePixelRatio || 1);
  const height = canvas.height / (window.devicePixelRatio || 1);
  ctx.clearRect(0, 0, width, height);
  ctx.fillStyle = '#0f172a';
  ctx.fillRect(0, 0, width, height);

  const centerX = width / 2;
  const centerY = height / 2;
  ctx.strokeStyle = 'rgba(255, 255, 255, 0.08)';
  ctx.lineWidth = 1;
  ctx.beginPath();
  ctx.moveTo(0, centerY);
  ctx.lineTo(width, centerY);
  ctx.moveTo(centerX, 0);
  ctx.lineTo(centerX, height);
  ctx.stroke();
}

function drawZones() {
  state.zones.forEach((zone) => {
    ctx.save();
    ctx.strokeStyle = 'rgba(34, 211, 238, 0.9)';
    ctx.lineWidth = 2;
    if (zone.type === 'circle') {
      const center = worldToCanvas(zone.cx, zone.cy);
      ctx.beginPath();
      ctx.arc(center.x, center.y, zone.r * state.scale, 0, Math.PI * 2);
      ctx.stroke();
    } else {
      const center = worldToCanvas(zone.cx, zone.cy);
      const width = zone.w * state.scale;
      const height = zone.h * state.scale;
      ctx.strokeRect(center.x - width / 2, center.y - height / 2, width, height);
    }
    ctx.restore();
  });
}

function drawEntities(frame) {
  const { entities } = frame;
  entities.forEach((entity) => {
    const pos = worldToCanvas(entity.x, entity.y);
    ctx.save();
    ctx.fillStyle = 'rgba(249, 115, 22, 0.95)';
    ctx.beginPath();
    ctx.arc(pos.x, pos.y, 6, 0, Math.PI * 2);
    ctx.fill();
    ctx.restore();
  });
}

function drawTrajectories(frameIndex) {
  ctx.save();
  ctx.lineWidth = 2;
  ctx.lineJoin = 'round';
  ctx.lineCap = 'round';

  state.animation.trajectories.forEach((points, entityId) => {
    ctx.strokeStyle = 'rgba(129, 140, 248, 0.7)';
    ctx.beginPath();
    for (let i = 0; i <= frameIndex && i < points.length; i += 1) {
      const { x, y } = worldToCanvas(points[i].x, points[i].y);
      if (i === 0) {
        ctx.moveTo(x, y);
      } else {
        ctx.lineTo(x, y);
      }
    }
    ctx.stroke();
  });

  ctx.restore();
}

function drawScene(frameIndex) {
  drawBackground();
  drawZones();
  if (state.animation.frames.length && frameIndex < state.animation.frames.length) {
    drawTrajectories(frameIndex);
    drawEntities(state.animation.frames[frameIndex]);
  } else {
    const frame = { entities: state.entities };
    drawEntities(frame);
  }
}

drawScene(0);
renderZoneList();
renderEvents([]);

function renderEvents(events) {
  eventLog.innerHTML = '';
  if (!events.length) {
    const item = document.createElement('li');
    item.textContent = 'No events recorded.';
    eventLog.appendChild(item);
    return;
  }
  events.forEach((event) => {
    const item = document.createElement('li');
    const time = event.time.toFixed(2);
    const label = `${event.entityId} ${event.type === 'enter' ? 'entered' : 'exited'} ${event.zoneLabel} @ t=${time}`;
    item.textContent = label;
    eventLog.appendChild(item);
  });
}

async function runSimulation() {
  clearError();
  if (!state.entities.length) {
    setError('Place at least one entity on the canvas.');
    return;
  }

  let payload;
  try {
    payload = buildPayload();
    if (!Number.isFinite(payload.settings.duration) || payload.settings.duration <= 0) {
      throw new Error('Duration must be positive.');
    }
    if (!Number.isFinite(payload.settings.dt) || payload.settings.dt <= 0) {
      throw new Error('Time step must be positive.');
    }
  } catch (err) {
    setError(err.message);
    return;
  }

  runButton.disabled = true;
  runButton.textContent = 'Simulating…';

  try {
    const response = await fetch('/simulate', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(payload),
    });
    const data = await response.json();
    if (!response.ok) {
      throw new Error(data.error || 'Simulation failed.');
    }
    state.animation.frames = data.frames;
    state.animation.dt = payload.settings.dt;
    state.animation.trajectories = new Map();
    data.frames[0].entities.forEach((entity) => {
      state.animation.trajectories.set(entity.id, []);
    });
    data.frames.forEach((frame) => {
      frame.entities.forEach((entity) => {
        const track = state.animation.trajectories.get(entity.id);
        if (track) {
          track.push({ x: entity.x, y: entity.y });
        }
      });
    });
    state.animation.playing = true;
    state.animation.startTime = null;
    state.animation.currentFrame = 0;
    renderEvents(data.events || []);
    requestAnimationFrame(stepAnimation);
  } catch (err) {
    setError(err.message);
  } finally {
    runButton.disabled = false;
    runButton.textContent = 'Run Simulation';
  }
}

runButton.addEventListener('click', runSimulation);

function stepAnimation(timestamp) {
  if (!state.animation.playing) {
    drawScene(state.animation.currentFrame || 0);
    return;
  }

  if (state.animation.startTime === null) {
    state.animation.startTime = timestamp;
  }

  const elapsed = (timestamp - state.animation.startTime) / 1000;
  const frameIndex = Math.min(
    Math.floor(elapsed / state.animation.dt),
    state.animation.frames.length - 1,
  );

  state.animation.currentFrame = frameIndex;
  drawScene(frameIndex);

  if (frameIndex >= state.animation.frames.length - 1) {
    state.animation.playing = false;
    return;
  }

  requestAnimationFrame(stepAnimation);
}
