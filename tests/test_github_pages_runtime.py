"""Checks on the static GitHub Pages playground under ``docs/``.

The playground is plain ES modules served straight from ``docs/``, so these tests
guard the wiring that has no other coverage: relative asset paths, the import map
that resolves three.js and cannon-es, and the module boundaries the runtime
depends on.
"""

import json
import re
from pathlib import Path

import pytest

DOCS = Path(__file__).resolve().parents[1] / "docs"
SRC = DOCS / "src"


@pytest.fixture(scope="module")
def index() -> str:
    return (DOCS / "index.html").read_text()


@pytest.fixture(scope="module")
def runtime() -> str:
    return (DOCS / "runtime.js").read_text()


def read_module(name: str) -> str:
    return (SRC / name).read_text()


def test_github_pages_runtime_uses_relative_static_assets(index):
    assert 'href="./style.css"' in index
    assert 'src="./runtime.js"' in index
    assert 'type="module"' in index
    assert "/static/" not in index


def test_import_map_pins_three_and_cannon(index):
    match = re.search(
        r'<script type="importmap">\s*(\{.*?\})\s*</script>', index, re.DOTALL
    )
    assert match, "index.html must declare an import map for the 3D libraries"

    imports = json.loads(match.group(1))["imports"]
    assert set(imports) == {"three", "three/addons/", "cannon-es"}

    for specifier, url in imports.items():
        assert url.startswith("https://cdn.jsdelivr.net/npm/"), specifier
        # Every entry is pinned to an exact version so a CDN release cannot break the page.
        assert re.search(r"@\d+\.\d+\.\d+", url), specifier

    assert imports["three/addons/"].endswith("/")


def test_runtime_modules_exist_and_are_wired(runtime):
    for module in ("nagare.js", "physics.js", "scene.js", "space.js", "visuals.js"):
        assert (SRC / module).is_file(), module
        assert f"./src/{module}" in runtime, module

    assert "import * as CANNON from 'cannon-es';" in runtime


def test_scene_module_builds_a_three_js_stage():
    scene = read_module("scene.js")

    assert "import * as THREE from 'three';" in scene
    assert "three/addons/controls/OrbitControls.js" in scene
    assert "new THREE.WebGLRenderer" in scene
    assert "new THREE.PerspectiveCamera" in scene
    assert "new OrbitControls" in scene
    # Placing entities in 3D is a ray/plane intersection, not a 2D canvas offset.
    assert "function pointerOnPlane" in scene


def test_physics_module_builds_a_cannon_es_world():
    physics = read_module("physics.js")

    assert "import * as CANNON from 'cannon-es';" in physics
    assert "new CANNON.World" in physics
    assert "new CANNON.Body" in physics
    assert "new CANNON.Sphere" in physics
    # Solid zones become static colliders: boxes, spheres or a triangle mesh.
    assert "new CANNON.Box" in physics
    assert "new CANNON.Trimesh" in physics
    assert "world.step" in physics


def test_nagare_module_exports_the_language_surface():
    nagare = read_module("nagare.js")

    for export in (
        "export function parseNagare",
        "export function compileExpression",
        "export function zoneContains",
        "export function zoneToSource",
        "export function insertZoneSource",
        "export function removeZoneSource",
    ):
        assert export in nagare, export


def test_nagare_module_is_three_dimensional():
    nagare = read_module("nagare.js")

    # x, y and z all drive the field, and t remains the step counter.
    assert "const VARIABLE_NAMES = ['x', 'y', 'z', 't'];" in nagare
    # 2D programs stay valid: the missing component carries z through unchanged.
    assert "if (components.length === 2) components.push('z');" in nagare

    for shape in ("ellipsoid", "ellipse", "sphere", "box"):
        assert f"shape === '{shape}'" in nagare, shape

    for action in ("display", "finish", "impulse"):
        assert action in nagare, action


def test_example_program_is_a_3d_field_with_zones_and_actions():
    nagare = read_module("nagare.js")
    example = re.search(
        r"export const EXAMPLE_PROGRAM = `(.*?)`;", nagare, re.DOTALL
    )
    assert example, "nagare.js must ship an example program"

    source = example.group(1)
    assert "ZONES {" in source and "EXECUTE {" in source
    assert "Ellipsoid(" in source and "Sphere(" in source and "Box(" in source
    assert "solid" in source
    assert "finish" in source and "display" in source and "impulse" in source


def test_github_pages_runtime_exposes_drawing_and_execution_controls(index, runtime):
    assert 'data-tool="entity"' in index
    assert 'data-tool="ellipsoid"' in index
    assert 'data-tool="erase"' in index
    assert 'data-tool="orbit"' in index
    assert 'id="runProgram"' in index
    assert 'id="runtimeCanvas"' in index

    assert "function addDrawnZone" in runtime
    assert "function advanceField" in runtime
    assert "function applyCoupling" in runtime
    assert "function checkZones" in runtime


def test_physics_controls_are_reachable_from_the_page(index, runtime):
    for element_id in (
        "couplingMode",
        "gravity",
        "fieldResponse",
        "damping",
        "restitution",
        "entityRadius",
        "arenaSize",
        "enableFloor",
        "enableWalls",
    ):
        assert f'id="{element_id}"' in index, element_id
        assert f'getElementById(\'{element_id}\')' in runtime, element_id


def test_field_coupling_runs_inside_the_physics_substep(runtime):
    physics = read_module("physics.js")

    # cannon clears accumulated forces after every world.step, so the steering
    # force has to be re-applied per substep rather than once per Nagare step.
    assert "step(delta, onSubstep)" in physics
    assert "if (onSubstep) onSubstep(FIXED_STEP);" in physics
    assert "physics.step(delta, () => applyCoupling(config));" in runtime
