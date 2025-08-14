from pathlib import Path

import pytest

from nagare_interpreter import parse_zones, parse_script, run, parse_execute


def test_parse_zones_multiple_definitions():
    path = Path(__file__).parent / "fixtures" / "multiple_zones.nagare"
    content = path.read_text()
    zones = parse_zones(content)
    assert [z.name for z in zones] == ["zone1", "zone2"]
    z1, z2 = zones
    assert (z1.cx, z1.cy, z1.a, z1.b) == (0.0, 0.0, 1.0, 2.0)
    assert (z2.cx, z2.cy, z2.a, z2.b) == (1.0, 1.0, 3.0, 4.0)


def test_run_triggers_display_and_finish(capsys):
    path = Path(__file__).parent / "fixtures" / "display_and_finish.nagare"
    prog_x, prog_y, zones = parse_script(str(path))
    run(prog_x, prog_y, zones)
    captured = capsys.readouterr().out.strip().splitlines()
    assert captured == ["Hello zone", "Finished at step 2"]


def test_parse_execute_raises_for_unknown_zone():
    src = "EXECUTE { prog<missing> { display \"hello\" } }"
    with pytest.raises(ValueError, match="missing"):
        parse_execute(src, {})
