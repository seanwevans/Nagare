import pytest

from webapp.simulator import run_simulation


def test_run_simulation_aligns_final_time_for_partial_step():
    payload = {
        "vectorField": {"fx": "0", "fy": "0"},
        "zones": [],
        "entities": [{"id": "entity-1", "x": 0.0, "y": 0.0}],
        "settings": {"duration": 1.0, "dt": 0.6},
    }

    result = run_simulation(payload)
    times = [frame["time"] for frame in result["frames"]]

    assert times == pytest.approx([0.0, 0.6, 1.0])
    assert times[-1] == pytest.approx(1.0)
