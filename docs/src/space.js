/**
 * Nagare works in a right handed (x, y, z) space where z is altitude, matching the
 * way the 2D playground used to treat the drawing plane. three.js and cannon-es
 * both put "up" on +Y, so every value crossing that boundary goes through here.
 *
 *   Nagare (x, y, z)  <->  world (x, z, y)
 */

/** Copy a Nagare point into any {set(x, y, z)} vector (THREE.Vector3, CANNON.Vec3). */
export function toWorld(point, target) {
  target.set(point.x, point.z, point.y);
  return target;
}

/** Convert a world vector back into plain Nagare coordinates. */
export function fromWorld(vector) {
  return { x: vector.x, y: vector.z, z: vector.y };
}

/** Semi-axes / half extents expressed in world axis order. */
export function sizeToWorld(radii, target) {
  target.set(radii.x, radii.z, radii.y);
  return target;
}

export const AXIS_LABELS = { x: 'x', y: 'y (depth)', z: 'z (up)' };
