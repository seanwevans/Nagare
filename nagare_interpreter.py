#!/usr/bin/env python3
"""Simple interpreter for a subset of the Nagare language.

This interpreter supports the minimal features used in the example
scripts in this repository. It parses BEGIN, ZONES and EXECUTE blocks
and runs a very basic simulation where the position is updated by the
"program" vector field on each iteration. When the position enters a
zone, the associated action is executed. Supported actions are
``display "message"`` and ``finish``.
"""

import math
import re
import sys
from dataclasses import dataclass
from typing import Callable, Dict, List, Tuple

@dataclass
class Zone:
    name: str
    cx: float
    cy: float
    a: float
    b: float
    action: Tuple[str, str]

    def contains(self, x: float, y: float) -> bool:
        dx = (x - self.cx) / self.a
        dy = (y - self.cy) / self.b
        return dx * dx + dy * dy <= 1.0

def eval_expr(expr: str, x: float, y: float) -> float:
    """Evaluate a simple arithmetic expression using x and y."""
    allowed: Dict[str, float] = {
        'x': x,
        'y': y,
        'sin': math.sin,
        'cos': math.cos,
        'tan': math.tan,
        'pi': math.pi,
        'e': math.e,
    }
    return eval(expr, {"__builtins__": {}}, allowed)

def parse_program(src: str) -> Tuple[str, str]:
    m = re.search(r'program\s+(?:\w+\s+)?\{\s*([^,]+)\s*,\s*([^}]+)\s*\}', src)
    if not m:
        raise ValueError("Program block not found")
    return m.group(1).strip(), m.group(2).strip()

def parse_zones(src: str) -> List[Zone]:
    zone_pattern = re.compile(
        r'(\w+)\s*\{\s*Ellipse\(\(([^,]+),\s*([^\)]+)\),\s*([^,]+),\s*([^\)]+)\)\s*\}'
    )
    zones: List[Zone] = []
    for name, cx, cy, a, b in zone_pattern.findall(src):
        zones.append(Zone(name, float(cx), float(cy), float(a), float(b), ('', '')))
    return zones

def parse_execute(src: str, zones: Dict[str, Zone]) -> None:
    exec_pattern = re.compile(
        r'\w+<(\w+)>\s*\{\s*(display\s+"([^"]*)"|finish)\s*\}'
    )
    for zone_name, action_block, msg in exec_pattern.findall(src):
        if zone_name not in zones:
            continue
        if action_block.startswith('display'):
            zones[zone_name].action = ('display', msg)
        else:
            zones[zone_name].action = ('finish', '')

def parse_script(path: str) -> Tuple[str, str, List[Zone]]:
    with open(path, 'r') as f:
        content = f.read()

    prog_x, prog_y = parse_program(content)
    zones = parse_zones(content)
    zone_map = {z.name: z for z in zones}
    parse_execute(content, zone_map)
    return prog_x, prog_y, list(zone_map.values())

def run(prog_x: str, prog_y: str, zones: List[Zone]) -> None:
    """Execute the simulation for the given vector field and zones.

    Parameters
    ----------
    prog_x: str
        Expression computing the next ``x`` value.
    prog_y: str
        Expression computing the next ``y`` value.
    zones: List[Zone]
        List of zones with associated actions to trigger.
    """
    x = 0.0
    y = 0.0
    step = 0
    triggered = set()
    while True:
        # update position
        new_x = eval_expr(prog_x, x, y)
        new_y = eval_expr(prog_y, x, y)
        x, y = new_x, new_y
        step += 1
        # check zones
        for z in zones:
            if z.action[0] and z.contains(x, y) and z.name not in triggered:
                triggered.add(z.name)
                if z.action[0] == 'display':
                    print(z.action[1])
                elif z.action[0] == 'finish':
                    print("Finished at step", step)
                    return
        if step > 10000:
            print("Maximum steps reached")
            return

def main(argv: List[str]) -> None:
    """Command line entry point for the interpreter.

    Parameters
    ----------
    argv: List[str]
        Arguments passed from the command line, where the first element is
        the path to the ``.nagare`` script.
    """
    if len(argv) < 1:
        print("Usage: nagare_interpreter.py <file.nagare>")
        return
    prog_x, prog_y, zones = parse_script(argv[0])
    run(prog_x, prog_y, zones)

if __name__ == '__main__':
    main(sys.argv[1:])
