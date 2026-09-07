/**
 * Nagare language subset for the browser playground, extended to three dimensions.
 *
 * Supported forms:
 *   program name { x_expr, y_expr }           -> z is carried over unchanged
 *   program name { x_expr, y_expr, z_expr }   -> full 3D field
 *   program name := <x_expr, y_expr, z_expr>  -> grammar.g4 vector spelling
 *
 *   ZONES {
 *     name { Ellipsoid((cx, cy, cz), a, b, c) }
 *     name { Ellipse((cx, cy), a, b) }          -> lifted to an ellipsoid at cz = 0
 *     name { Sphere((cx, cy, cz), r) }
 *     name { Box((cx, cy, cz), hx, hy, hz) }
 *     name { Sphere((0, 0, 1), 0.75) solid }    -> also a rigid body in the physics world
 *   }
 *
 *   EXECUTE {
 *     name<zone> { display "message" }
 *     name<zone> { finish }
 *     name<zone> { impulse(ix, iy, iz) }        -> playground extension: a physics kick
 *   }
 *
 * Expressions are compiled with a whitelist of identifiers, so nothing but the
 * documented variables, constants and math functions can be reached.
 */

const MATH_FUNCTIONS = {
  sin: Math.sin,
  cos: Math.cos,
  tan: Math.tan,
  asin: Math.asin,
  acos: Math.acos,
  atan: Math.atan,
  atan2: Math.atan2,
  sinh: Math.sinh,
  cosh: Math.cosh,
  tanh: Math.tanh,
  sqrt: Math.sqrt,
  cbrt: Math.cbrt,
  abs: Math.abs,
  sign: Math.sign,
  floor: Math.floor,
  ceil: Math.ceil,
  round: Math.round,
  log: Math.log,
  log2: Math.log2,
  exp: Math.exp,
  pow: Math.pow,
  min: Math.min,
  max: Math.max,
  hypot: Math.hypot,
};

const FUNCTION_NAMES = Object.keys(MATH_FUNCTIONS);
const FUNCTION_VALUES = Object.values(MATH_FUNCTIONS);
const VARIABLE_NAMES = ['x', 'y', 'z', 't'];
const CONSTANT_NAMES = ['PI', 'E', 'TAU'];
const ALLOWED_NAMES = new Set([...VARIABLE_NAMES, ...CONSTANT_NAMES, ...FUNCTION_NAMES]);

const SAFE_CHARACTERS = /^[\s\w.,+\-*/%()^]+$/;
const MAX_EXPRESSION_LENGTH = 400;

export const EXAMPLE_PROGRAM = `!/usr/bin/env nagare

BEGIN {
  program vortex {
    x + 0.05 * (-y - 0.4 * x * (x * x + y * y - 4)),
    y + 0.05 * ( x - 0.4 * y * (x * x + y * y - 4)),
    z + 0.05 * (1.8 + 0.9 * sin(t * 0.017) - z)
  }
}

ZONES {
  pillar { Sphere((0, 0, 1.4), 0.75) solid }
  gate   { Ellipsoid((0, 2, 1.8), 0.45, 0.45, 0.9) }
  ridge  { Box((-2, 0, 1.8), 0.35, 0.6, 0.9) }
  halt   { Ellipsoid((0, -2, 1.8), 0.5, 0.5, 1.2) }
}

EXECUTE {
  vortex<gate>  { display "Passed the gate" }
  vortex<ridge> { display "Crossed the ridge"; impulse(0, 0, 4) }
  vortex<halt>  { finish }
}
`;

/** Compile a Nagare arithmetic expression into `(x, y, z, t) => number`. */
export function compileExpression(expression, label = 'Expression') {
  const source = String(expression ?? '');
  if (!source.trim()) throw new Error(`${label} cannot be empty.`);
  if (source.length > MAX_EXPRESSION_LENGTH) {
    throw new Error(`${label} is longer than ${MAX_EXPRESSION_LENGTH} characters.`);
  }
  if (!SAFE_CHARACTERS.test(source)) {
    throw new Error(`${label} contains unsupported characters: ${source.trim()}`);
  }

  const javascript = source
    .replace(/\btau\b/gi, 'TAU')
    .replace(/\bpi\b/gi, 'PI')
    .replace(/\be\b/g, 'E')
    .replace(/\^/g, '**');

  const identifiers = [...javascript.matchAll(/[A-Za-z_]\w*/g)].map((match) => match[0]);
  const unknown = identifiers.find((name) => !ALLOWED_NAMES.has(name));
  if (unknown) {
    throw new Error(`${label} uses the unknown identifier '${unknown}'.`);
  }

  // eslint-disable-next-line no-new-func -- the input is restricted to the whitelist above.
  const compiled = new Function(
    ...VARIABLE_NAMES,
    ...CONSTANT_NAMES,
    ...FUNCTION_NAMES,
    `"use strict"; return (${javascript});`,
  );

  return (x, y, z, t) => {
    const value = compiled(x, y, z, t, Math.PI, Math.E, Math.PI * 2, ...FUNCTION_VALUES);
    if (!Number.isFinite(value)) {
      throw new Error(`${label} produced a non-finite value at (${format(x)}, ${format(y)}, ${format(z)}).`);
    }
    return Number(value);
  };
}

/** Evaluate a literal expression such as `1.5` or `pi / 2`. */
export function evaluateConstant(expression, label = 'Value') {
  return compileExpression(expression, label)(0, 0, 0, 0);
}

export function format(value) {
  if (!Number.isFinite(value)) return String(value);
  return Number(value.toFixed(3)).toString();
}

function skipString(text, quoteIndex) {
  for (let i = quoteIndex + 1; i < text.length; i += 1) {
    if (text[i] === '\\') {
      i += 1;
      continue;
    }
    if (text[i] === '"') return i;
  }
  return text.length;
}

/** Index of the brace closing the one at `openIndex`, or -1. */
function findClosingBrace(text, openIndex) {
  let depth = 0;
  for (let i = openIndex; i < text.length; i += 1) {
    const character = text[i];
    if (character === '"') {
      i = skipString(text, i);
      continue;
    }
    if (character === '{') depth += 1;
    else if (character === '}') {
      depth -= 1;
      if (depth === 0) return i;
    }
  }
  return -1;
}

function splitTopLevel(text, separator = ',') {
  const parts = [];
  let depth = 0;
  let current = '';
  for (let i = 0; i < text.length; i += 1) {
    const character = text[i];
    if (character === '"') {
      const end = skipString(text, i);
      current += text.slice(i, end + 1);
      i = end;
      continue;
    }
    if (character === '(' || character === '{' || character === '[') depth += 1;
    else if (character === ')' || character === '}' || character === ']') depth -= 1;
    if (character === separator && depth === 0) {
      parts.push(current);
      current = '';
      continue;
    }
    current += character;
  }
  parts.push(current);
  return parts.map((part) => part.trim());
}

/** Body of a `KEYWORD { ... }` block, or null when the keyword is absent. */
function findBlock(source, keyword) {
  const pattern = new RegExp(`\\b${keyword}\\b[^{]*\\{`, 'i');
  const match = pattern.exec(source);
  if (!match) return null;
  const openIndex = match.index + match[0].length - 1;
  const closeIndex = findClosingBrace(source, openIndex);
  if (closeIndex < 0) throw new Error(`The ${keyword} block is missing its closing brace.`);
  return { openIndex, closeIndex, body: source.slice(openIndex + 1, closeIndex) };
}

function findClosingAngle(text, openIndex) {
  let depth = 0;
  for (let i = openIndex + 1; i < text.length; i += 1) {
    const character = text[i];
    if (character === '(') depth += 1;
    else if (character === ')') depth -= 1;
    else if (character === '>' && depth === 0 && text[i - 1] !== '=' && text[i + 1] !== '=') return i;
  }
  return -1;
}

function parseProgramVector(source) {
  const braced = /\bprogram\b(?:\s+([A-Za-z_]\w*))?\s*(?::=)?\s*\{/i.exec(source);
  const angled = /\bprogram\b(?:\s+([A-Za-z_]\w*))?\s*(?::=)?\s*</i.exec(source);
  const match = !braced || (angled && angled.index < braced.index) ? angled : braced;
  if (!match) {
    throw new Error('Could not find a program { x_expr, y_expr, z_expr } block.');
  }

  const openIndex = match.index + match[0].length - 1;
  const closeIndex = source[openIndex] === '{'
    ? findClosingBrace(source, openIndex)
    : findClosingAngle(source, openIndex);
  if (closeIndex < 0) throw new Error('The program block is missing its closing delimiter.');

  const components = splitTopLevel(source.slice(openIndex + 1, closeIndex)).filter(Boolean);
  if (components.length !== 2 && components.length !== 3) {
    throw new Error(`A program block needs 2 or 3 comma separated expressions, found ${components.length}.`);
  }
  if (components.length === 2) components.push('z');

  return { name: match[1] || 'program', components };
}

function parseCenter(text, label) {
  const trimmed = String(text ?? '').trim();
  if (!trimmed.startsWith('(') || !trimmed.endsWith(')')) {
    throw new Error(`${label} needs a parenthesised centre such as (0, 0, 1).`);
  }
  const parts = splitTopLevel(trimmed.slice(1, -1)).filter(Boolean);
  if (parts.length !== 2 && parts.length !== 3) {
    throw new Error(`${label} centre needs 2 or 3 coordinates, found ${parts.length}.`);
  }
  const [x, y, z = '0'] = parts;
  return {
    x: evaluateConstant(x, `${label} centre x`),
    y: evaluateConstant(y, `${label} centre y`),
    z: evaluateConstant(z, `${label} centre z`),
  };
}

const SHAPE_PATTERN = /^([A-Za-z_]\w*)\s*\(([\s\S]*)\)\s*([A-Za-z_]\w*)?$/;

function parseZoneBody(name, body, index) {
  const label = `Zone '${name}'`;
  const match = SHAPE_PATTERN.exec(body.trim());
  if (!match) {
    throw new Error(`${label} must declare a shape, for example Ellipsoid((0, 0, 1), 1, 1, 0.5).`);
  }

  const [, rawShape, rawArguments, modifier] = match;
  if (modifier && modifier.toLowerCase() !== 'solid') {
    throw new Error(`${label} has an unknown modifier '${modifier}'. Only 'solid' is supported.`);
  }

  const args = splitTopLevel(rawArguments).filter(Boolean);
  if (!args.length) throw new Error(`${label} is missing its arguments.`);

  const center = parseCenter(args[0], label);
  const sizes = args.slice(1).map((value, i) => evaluateConstant(value, `${label} size ${i + 1}`));
  const shape = rawShape.toLowerCase();

  let kind = 'ellipsoid';
  let radii;
  if (shape === 'sphere') {
    if (sizes.length !== 1) throw new Error(`${label}: Sphere takes one radius.`);
    radii = { x: sizes[0], y: sizes[0], z: sizes[0] };
  } else if (shape === 'ellipse') {
    if (sizes.length !== 2) throw new Error(`${label}: Ellipse takes two semi-axes.`);
    // A 2D zone becomes an ellipsoid centred on its own plane so legacy programs still trigger.
    radii = { x: sizes[0], y: sizes[1], z: (sizes[0] + sizes[1]) / 2 };
  } else if (shape === 'ellipsoid') {
    if (sizes.length === 1) radii = { x: sizes[0], y: sizes[0], z: sizes[0] };
    else if (sizes.length === 3) radii = { x: sizes[0], y: sizes[1], z: sizes[2] };
    else throw new Error(`${label}: Ellipsoid takes one or three semi-axes.`);
  } else if (shape === 'box') {
    kind = 'box';
    if (sizes.length === 1) radii = { x: sizes[0], y: sizes[0], z: sizes[0] };
    else if (sizes.length === 3) radii = { x: sizes[0], y: sizes[1], z: sizes[2] };
    else throw new Error(`${label}: Box takes one or three half extents.`);
  } else {
    throw new Error(`${label} uses the unknown shape '${rawShape}'. Try Ellipsoid, Ellipse, Sphere or Box.`);
  }

  for (const axis of ['x', 'y', 'z']) {
    if (!Number.isFinite(radii[axis]) || radii[axis] <= 0) {
      throw new Error(`${label} has a non-positive size on the ${axis} axis.`);
    }
    if (!Number.isFinite(center[axis])) {
      throw new Error(`${label} has a non-finite centre on the ${axis} axis.`);
    }
  }

  return {
    id: `program-${name}-${index}`,
    label: name,
    origin: 'program',
    kind,
    center,
    radii,
    solid: Boolean(modifier),
    actions: [],
  };
}

function parseZones(source) {
  const block = findBlock(source, 'ZONES');
  const zones = new Map();
  if (!block) return zones;

  const pattern = /([A-Za-z_]\w*)\s*\{/g;
  let match;
  let index = 0;
  while ((match = pattern.exec(block.body)) !== null) {
    const openIndex = match.index + match[0].length - 1;
    const closeIndex = findClosingBrace(block.body, openIndex);
    if (closeIndex < 0) throw new Error(`Zone '${match[1]}' is missing its closing brace.`);
    zones.set(match[1], parseZoneBody(match[1], block.body.slice(openIndex + 1, closeIndex), index));
    index += 1;
    pattern.lastIndex = closeIndex + 1;
  }
  return zones;
}

const DISPLAY_PATTERN = /^display\s+"((?:[^"\\]|\\.)*)"$/i;
const IMPULSE_PATTERN = /^impulse\s*\(([\s\S]*)\)$/i;

function parseActions(text, zoneName) {
  return splitTopLevel(text.replace(/\r?\n/g, ';'), ';')
    .filter(Boolean)
    .map((entry) => {
      const display = DISPLAY_PATTERN.exec(entry);
      if (display) return { type: 'display', message: display[1].replace(/\\(.)/g, '$1') };
      if (/^finish$/i.test(entry)) return { type: 'finish' };
      const impulse = IMPULSE_PATTERN.exec(entry);
      if (impulse) {
        const parts = splitTopLevel(impulse[1]).filter(Boolean);
        if (parts.length !== 3) throw new Error(`impulse in '${zoneName}' needs three components.`);
        const [x, y, z] = parts.map((value, i) => evaluateConstant(value, `impulse component ${i + 1}`));
        return { type: 'impulse', vector: { x, y, z } };
      }
      throw new Error(`Unsupported action '${entry}' for zone '${zoneName}'. Use display, finish or impulse.`);
    });
}

function applyExecute(source, zones) {
  const block = findBlock(source, 'EXECUTE');
  if (!block) return;

  const pattern = /([A-Za-z_]\w*)\s*<\s*([A-Za-z_]\w*)\s*>\s*\{/g;
  let match;
  while ((match = pattern.exec(block.body)) !== null) {
    const openIndex = match.index + match[0].length - 1;
    const closeIndex = findClosingBrace(block.body, openIndex);
    if (closeIndex < 0) throw new Error(`The EXECUTE entry for '${match[2]}' is missing its closing brace.`);
    const zone = zones.get(match[2]);
    if (!zone) throw new Error(`EXECUTE references the unknown zone '${match[2]}'.`);
    zone.actions = parseActions(block.body.slice(openIndex + 1, closeIndex).trim(), match[2]);
    pattern.lastIndex = closeIndex + 1;
  }
}

/**
 * Parse a Nagare source string into a 3D field plus its zones.
 * @returns {{name: string, expressions: string[], field: Function, zones: object[]}}
 */
export function parseNagare(source) {
  const { name, components } = parseProgramVector(source);
  const fx = compileExpression(components[0], "The program's x expression");
  const fy = compileExpression(components[1], "The program's y expression");
  const fz = compileExpression(components[2], "The program's z expression");

  const zones = parseZones(source);
  applyExecute(source, zones);

  return {
    name,
    expressions: components,
    field: (x, y, z, t) => ({ x: fx(x, y, z, t), y: fy(x, y, z, t), z: fz(x, y, z, t) }),
    zones: [...zones.values()],
  };
}

/** True when `point` (Nagare coordinates) lies inside `zone`. */
export function zoneContains(zone, point) {
  const dx = point.x - zone.center.x;
  const dy = point.y - zone.center.y;
  const dz = point.z - zone.center.z;
  if (zone.kind === 'box') {
    return Math.abs(dx) <= zone.radii.x && Math.abs(dy) <= zone.radii.y && Math.abs(dz) <= zone.radii.z;
  }
  const nx = dx / zone.radii.x;
  const ny = dy / zone.radii.y;
  const nz = dz / zone.radii.z;
  return nx * nx + ny * ny + nz * nz <= 1;
}

/** Render a zone back into the source syntax used by the ZONES block. */
export function zoneToSource(zone) {
  const center = `(${format(zone.center.x)}, ${format(zone.center.y)}, ${format(zone.center.z)})`;
  const sizes = `${format(zone.radii.x)}, ${format(zone.radii.y)}, ${format(zone.radii.z)}`;
  const shape = zone.kind === 'box' ? 'Box' : 'Ellipsoid';
  return `  ${zone.label} { ${shape}(${center}, ${sizes})${zone.solid ? ' solid' : ''} }`;
}

/** Cut `source[start..end]` out along with the blank line it leaves behind. */
function spliceEntry(source, start, end) {
  const before = source.slice(0, start).replace(/[ \t]*$/, '');
  const after = source.slice(end).replace(/^[ \t]*\r?\n/, '');
  return `${before}${before.endsWith('\n') ? '' : '\n'}${after}`;
}

/** Remove every `prog<label> { ... }` entry from the EXECUTE block. */
function removeExecuteEntries(source, label) {
  let result = source;
  for (;;) {
    const block = findBlock(result, 'EXECUTE');
    if (!block) return result;

    const pattern = /([A-Za-z_]\w*)\s*<\s*([A-Za-z_]\w*)\s*>\s*\{/g;
    let match;
    let removed = false;
    while ((match = pattern.exec(block.body)) !== null) {
      const openIndex = match.index + match[0].length - 1;
      const closeIndex = findClosingBrace(block.body, openIndex);
      if (closeIndex < 0) return result;
      if (match[2] === label) {
        const offset = block.openIndex + 1;
        result = spliceEntry(result, offset + match.index, offset + closeIndex + 1);
        removed = true;
        break;
      }
      pattern.lastIndex = closeIndex + 1;
    }
    if (!removed) return result;
  }
}

/**
 * Drop a zone from the ZONES block along with the EXECUTE entries that target it.
 * Returns null when the source has no such zone.
 */
export function removeZoneSource(source, label) {
  const withoutActions = removeExecuteEntries(source, label);
  const block = findBlock(withoutActions, 'ZONES');
  if (!block) return null;

  const pattern = /([A-Za-z_]\w*)\s*\{/g;
  let match;
  while ((match = pattern.exec(block.body)) !== null) {
    const openIndex = match.index + match[0].length - 1;
    const closeIndex = findClosingBrace(block.body, openIndex);
    if (closeIndex < 0) return null;
    if (match[1] === label) {
      const offset = block.openIndex + 1;
      return spliceEntry(withoutActions, offset + match.index, offset + closeIndex + 1);
    }
    pattern.lastIndex = closeIndex + 1;
  }
  return null;
}

/** Insert a zone definition into an existing ZONES block, creating one when needed. */
export function insertZoneSource(source, zone) {
  const line = zoneToSource(zone);
  const block = findBlock(source, 'ZONES');
  if (block) {
    const head = source.slice(0, block.closeIndex).replace(/\s*$/, '');
    return `${head}\n${line}\n${source.slice(block.closeIndex)}`;
  }
  return `${source.replace(/\s*$/, '')}\n\nZONES {\n${line}\n}\n`;
}
