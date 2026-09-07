/**
 * three.js stage: renderer, camera, orbit controls, lighting, ground reference
 * grid and the pointer helpers the tools need for placing things in 3D.
 */
import * as THREE from 'three';
import { OrbitControls } from 'three/addons/controls/OrbitControls.js';

const UP = new THREE.Vector3(0, 1, 0);

export function createStage(canvas) {
  const renderer = new THREE.WebGLRenderer({ canvas, antialias: true, alpha: false });
  renderer.setPixelRatio(Math.min(window.devicePixelRatio || 1, 2));
  renderer.outputColorSpace = THREE.SRGBColorSpace;
  renderer.toneMapping = THREE.ACESFilmicToneMapping;
  renderer.toneMappingExposure = 1.05;
  renderer.shadowMap.enabled = true;
  renderer.shadowMap.type = THREE.PCFShadowMap;

  const scene = new THREE.Scene();
  scene.background = new THREE.Color(0x060d19);
  scene.fog = new THREE.Fog(0x060d19, 26, 78);

  const camera = new THREE.PerspectiveCamera(52, 1, 0.05, 500);
  camera.position.set(5.0, 4.2, 6.6);

  const controls = new OrbitControls(camera, canvas);
  controls.enableDamping = true;
  controls.dampingFactor = 0.075;
  controls.minDistance = 1.2;
  controls.maxDistance = 90;
  controls.maxPolarAngle = Math.PI * 0.92;
  controls.target.set(0, 1.6, 0);

  const hemisphere = new THREE.HemisphereLight(0x9ad7ff, 0x0a1526, 1.15);
  scene.add(hemisphere);

  const key = new THREE.DirectionalLight(0xffffff, 2.1);
  key.position.set(9, 14, 7);
  key.castShadow = true;
  key.shadow.mapSize.set(2048, 2048);
  key.shadow.camera.near = 1;
  key.shadow.camera.far = 60;
  key.shadow.camera.left = -18;
  key.shadow.camera.right = 18;
  key.shadow.camera.top = 18;
  key.shadow.camera.bottom = -18;
  key.shadow.bias = -0.0004;
  key.shadow.normalBias = 0.05;
  scene.add(key);
  scene.add(key.target);

  const rimA = new THREE.PointLight(0x22d3ee, 90, 42, 2);
  rimA.position.set(-9, 5, -8);
  scene.add(rimA);

  const rimB = new THREE.PointLight(0xa78bfa, 70, 42, 2);
  rimB.position.set(8, 3, -9);
  scene.add(rimB);

  const ground = new THREE.Mesh(
    new THREE.CircleGeometry(1, 96),
    new THREE.MeshStandardMaterial({
      color: 0x0d1b30,
      roughness: 0.92,
      metalness: 0.05,
      transparent: true,
      opacity: 0.94,
    }),
  );
  ground.rotation.x = -Math.PI / 2;
  // Dropped just below y = 0 so it does not z-fight the grid drawn on the same plane.
  ground.position.y = -0.01;
  ground.receiveShadow = true;
  scene.add(ground);

  const grid = new THREE.GridHelper(1, 20, 0x38bdf8, 0x1e3a5f);
  grid.material.transparent = true;
  grid.material.opacity = 0.42;
  scene.add(grid);

  const axes = new THREE.AxesHelper(1.6);
  axes.material.depthTest = false;
  axes.renderOrder = 2;
  scene.add(axes);

  const arena = new THREE.LineSegments(
    new THREE.EdgesGeometry(new THREE.BoxGeometry(1, 1, 1)),
    new THREE.LineBasicMaterial({ color: 0x2dd4bf, transparent: true, opacity: 0.28 }),
  );
  arena.visible = false;
  scene.add(arena);

  const world = new THREE.Group();
  world.name = 'nagare-world';
  scene.add(world);

  const raycaster = new THREE.Raycaster();
  raycaster.params.Line.threshold = 0.12;
  const pointer = new THREE.Vector2();
  const plane = new THREE.Plane(UP.clone(), 0);
  const hitPoint = new THREE.Vector3();

  function setPointer(event) {
    const rect = canvas.getBoundingClientRect();
    pointer.x = ((event.clientX - rect.left) / rect.width) * 2 - 1;
    pointer.y = -((event.clientY - rect.top) / rect.height) * 2 + 1;
    raycaster.setFromCamera(pointer, camera);
  }

  /** Intersect the pointer with the horizontal plane at Nagare altitude `height`. */
  function pointerOnPlane(event, height = 0) {
    setPointer(event);
    plane.set(UP, -height);
    if (!raycaster.ray.intersectPlane(plane, hitPoint)) return null;
    return { x: hitPoint.x, y: hitPoint.z, z: height };
  }

  /** First object hit by the pointer, walking up to the object carrying `userData.pickId`. */
  function pointerPick(event, objects) {
    setPointer(event);
    const hits = raycaster.intersectObjects(objects, true);
    for (const hit of hits) {
      let node = hit.object;
      while (node) {
        if (node.userData && node.userData.pickId) return { pickId: node.userData.pickId, object: node, hit };
        node = node.parent;
      }
    }
    return null;
  }

  function setArena(size, visible) {
    const half = size / 2;
    arena.scale.set(size, size, size);
    // Lifted a hair so the box's bottom edges do not z-fight the ground plane.
    arena.position.set(0, half + 0.006, 0);
    arena.visible = visible;
    ground.scale.setScalar(Math.max(size, 12) * 0.52);
    grid.scale.setScalar(Math.max(size, 12));
    scene.fog.near = size * 1.1;
    scene.fog.far = size * 4.4;
    key.shadow.camera.left = -size;
    key.shadow.camera.right = size;
    key.shadow.camera.top = size;
    key.shadow.camera.bottom = -size;
    key.shadow.camera.updateProjectionMatrix();
  }

  function setShadows(enabled) {
    renderer.shadowMap.enabled = enabled;
    key.castShadow = enabled;
    renderer.shadowMap.needsUpdate = true;
  }

  function resize() {
    const rect = canvas.getBoundingClientRect();
    const width = Math.max(320, Math.round(rect.width || 900));
    const height = Math.max(360, Math.round(rect.height || width * 0.62));
    renderer.setSize(width, height, false);
    camera.aspect = width / height;
    camera.updateProjectionMatrix();
  }

  function setView(view) {
    if (view === 'top') {
      camera.position.set(0.01, 12, 0.01);
      controls.target.set(0, 0, 0);
    } else if (view === 'front') {
      camera.position.set(0, 2.6, 11);
      controls.target.set(0, 1.8, 0);
    } else {
      camera.position.set(5.0, 4.2, 6.6);
      controls.target.set(0, 1.4, 0);
    }
    controls.update();
  }

  /** Point the camera at a set of world positions without changing its distance. */
  function lookAtPoints(points) {
    if (!points.length) return;
    const box = new THREE.Box3();
    points.forEach((point) => box.expandByPoint(point));
    box.getCenter(controls.target);
    controls.update();
  }

  setArena(14, false);
  resize();

  return {
    renderer,
    scene,
    camera,
    controls,
    world,
    ground,
    grid,
    axes,
    arena,
    resize,
    setArena,
    setShadows,
    setView,
    lookAtPoints,
    pointerOnPlane,
    pointerPick,
    setOrbitDrag(enabled) {
      controls.mouseButtons.LEFT = enabled ? THREE.MOUSE.ROTATE : null;
      controls.touches.ONE = enabled ? THREE.TOUCH.ROTATE : null;
    },
    render() {
      controls.update();
      renderer.render(scene, camera);
    },
  };
}
