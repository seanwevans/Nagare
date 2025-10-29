"""Utility functions for simulating Nagare-style vector field programs for the web UI."""
from __future__ import annotations

import ast
import math
from dataclasses import dataclass
from typing import Callable, Dict, Iterable, List, Tuple


class SimulationError(RuntimeError):
    """Raised when the user input cannot be simulated."""


_ALLOWED_NAMES = {
    **{name: getattr(math, name) for name in (
        "sin",
        "cos",
        "tan",
        "asin",
        "acos",
        "atan",
        "atan2",
        "sinh",
        "cosh",
        "tanh",
        "log",
        "log10",
        "exp",
        "sqrt",
        "fabs",
        "floor",
        "ceil",
        "pow",
        "pi",
        "e",
    )},
    "min": min,
    "max": max,
    "abs": abs,
    "hypot": math.hypot,
}

_ALLOWED_NODES = (
    ast.Expression,
    ast.Call,
    ast.BinOp,
    ast.UnaryOp,
    ast.Num,
    ast.Name,
    ast.Load,
    ast.Add,
    ast.Sub,
    ast.Mult,
    ast.Div,
    ast.Mod,
    ast.Pow,
    ast.USub,
    ast.UAdd,
    ast.Constant,
)


def _compile_expression(expr: str) -> Callable[[float, float, float], float]:
    if not expr or not expr.strip():
        raise SimulationError("Expression cannot be empty.")

    try:
        tree = ast.parse(expr, mode="eval")
    except SyntaxError as exc:
        raise SimulationError(f"Invalid expression '{expr}': {exc.msg}") from exc

    for node in ast.walk(tree):
        if not isinstance(node, _ALLOWED_NODES):
            raise SimulationError(f"Unsupported syntax in expression '{expr}'.")
        if isinstance(node, ast.Call):
            if not isinstance(node.func, ast.Name) or node.func.id not in _ALLOWED_NAMES:
                raise SimulationError(f"Use of function '{ast.dump(node.func)}' is not allowed.")

    code = compile(tree, "<expr>", "eval")

    def evaluator(x: float, y: float, t: float) -> float:
        local_vars = {"x": x, "y": y, "t": t}
        return float(eval(code, {"__builtins__": {}, **_ALLOWED_NAMES}, local_vars))

    return evaluator


@dataclass
class Zone:
    zone_id: str
    label: str
    zone_type: str
    params: Dict[str, float]

    def contains(self, x: float, y: float) -> bool:
        if self.zone_type == "circle":
            cx = self.params.get("cx", 0.0)
            cy = self.params.get("cy", 0.0)
            r = self.params.get("r", 0.0)
            return (x - cx) ** 2 + (y - cy) ** 2 <= r ** 2
        if self.zone_type == "rect":
            cx = self.params.get("cx", 0.0)
            cy = self.params.get("cy", 0.0)
            w = self.params.get("w", 0.0)
            h = self.params.get("h", 0.0)
            return (abs(x - cx) <= w / 2) and (abs(y - cy) <= h / 2)
        raise SimulationError(f"Unsupported zone type '{self.zone_type}'.")


@dataclass
class Entity:
    entity_id: str
    x: float
    y: float


@dataclass
class SimulationSettings:
    duration: float
    dt: float

    @classmethod
    def from_dict(cls, raw: Dict[str, float]) -> "SimulationSettings":
        raw_duration = raw.get("duration", 10.0)
        try:
            duration = float(raw_duration)
        except (TypeError, ValueError) as exc:
            raise SimulationError(f"Invalid duration '{raw_duration}'; provide a numeric value.") from exc

        raw_dt = raw.get("dt", 0.05)
        try:
            dt = float(raw_dt)
        except (TypeError, ValueError) as exc:
            raise SimulationError(f"Invalid time step '{raw_dt}'; provide a numeric value.") from exc
        if duration <= 0:
            raise SimulationError("Duration must be positive.")
        if dt <= 0:
            raise SimulationError("Time step must be positive.")
        if duration / dt > 2000:
            raise SimulationError("Too many simulation steps; reduce duration or increase dt.")
        return cls(duration=duration, dt=dt)


@dataclass
class SimulationInput:
    vector_field: Tuple[Callable[[float, float, float], float], Callable[[float, float, float], float]]
    zones: List[Zone]
    entities: List[Entity]
    settings: SimulationSettings


def _parse_zones(raw_zones: Iterable[Dict]) -> List[Zone]:
    zones: List[Zone] = []
    for idx, raw in enumerate(raw_zones):
        zone_id = raw.get("id") or f"zone-{idx}"
        label = raw.get("label") or zone_id
        zone_type = raw.get("type")
        if zone_type not in {"circle", "rect"}:
            raise SimulationError(f"Zone '{label}' has unsupported type '{zone_type}'.")
        params = {}
        for key in ("cx", "cy", "r", "w", "h"):
            if key in raw:
                params[key] = float(raw[key])
        if zone_type == "circle" and params.get("r", 0.0) <= 0:
            raise SimulationError(f"Circle zone '{label}' requires a positive radius.")
        if zone_type == "rect" and (params.get("w", 0.0) <= 0 or params.get("h", 0.0) <= 0):
            raise SimulationError(f"Rectangle zone '{label}' requires positive width and height.")
        zones.append(Zone(zone_id=zone_id, label=label, zone_type=zone_type, params=params))
    return zones


def _parse_entities(raw_entities: Iterable[Dict]) -> List[Entity]:
    entities: List[Entity] = []
    for idx, raw in enumerate(raw_entities):
        entity_id = raw.get("id") or f"entity-{idx}"
        try:
            x = float(raw.get("x", 0.0))
            y = float(raw.get("y", 0.0))
        except (TypeError, ValueError) as exc:
            raise SimulationError(f"Entity '{entity_id}' has invalid coordinates.") from exc
        entities.append(Entity(entity_id=entity_id, x=x, y=y))
    if not entities:
        raise SimulationError("At least one entity must be defined.")
    return entities


def parse_simulation_input(payload: Dict) -> SimulationInput:
    try:
        fx_expr = payload["vectorField"]["fx"]
        fy_expr = payload["vectorField"]["fy"]
    except KeyError as exc:
        raise SimulationError("Vector field requires both 'fx' and 'fy' expressions.") from exc

    fx = _compile_expression(str(fx_expr))
    fy = _compile_expression(str(fy_expr))

    zones = _parse_zones(payload.get("zones", []))
    entities = _parse_entities(payload.get("entities", []))
    settings = SimulationSettings.from_dict(payload.get("settings", {}))

    return SimulationInput(vector_field=(fx, fy), zones=zones, entities=entities, settings=settings)


def run_simulation(payload: Dict) -> Dict:
    sim_input = parse_simulation_input(payload)

    fx, fy = sim_input.vector_field
    zones = sim_input.zones
    entities = [Entity(entity.entity_id, entity.x, entity.y) for entity in sim_input.entities]
    dt = sim_input.settings.dt
    steps = int(sim_input.settings.duration / dt)

    events: List[Dict] = []
    frames: List[Dict] = []

    zone_membership: Dict[Tuple[str, str], bool] = {}

    t = 0.0
    for step in range(steps + 1):
        frame_entities = []
        for entity in entities:
            frame_entities.append({"id": entity.entity_id, "x": entity.x, "y": entity.y})
        frames.append({"time": t, "entities": frame_entities})

        if step == steps:
            break

        for entity in entities:
            for zone in zones:
                inside = zone.contains(entity.x, entity.y)
                key = (entity.entity_id, zone.zone_id)
                previous = zone_membership.get(key, inside)
                if inside != previous:
                    zone_membership[key] = inside
                    events.append({
                        "time": t,
                        "entityId": entity.entity_id,
                        "zoneId": zone.zone_id,
                        "zoneLabel": zone.label,
                        "type": "enter" if inside else "exit",
                    })
                else:
                    zone_membership[key] = inside

        for entity in entities:
            try:
                dx = fx(entity.x, entity.y, t)
                dy = fy(entity.x, entity.y, t)
            except Exception as exc:  # pragma: no cover - propagate evaluation errors
                raise SimulationError(f"Failed to evaluate vector field for entity '{entity.entity_id}': {exc}") from exc

            if not (math.isfinite(dx) and math.isfinite(dy)):
                raise SimulationError("Vector field produced non-finite values.")

            entity.x += dx * dt
            entity.y += dy * dt

        t += dt

    return {"frames": frames, "events": events}
