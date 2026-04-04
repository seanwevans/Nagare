import pytest

from webapp.simulator import SimulationError, _compile_expression, run_simulation


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


def test_compile_expression_rejects_oversized_ast():
    expr = "+".join(["x"] * 260)

    with pytest.raises(SimulationError, match="too complex"):
        _compile_expression(expr)


@pytest.mark.parametrize("expr", ["2**1000", "pow(x, 2, 3, 4, 5)"])
def test_compile_expression_rejects_large_exponent_and_too_many_call_args(expr):
    with pytest.raises(SimulationError):
        _compile_expression(expr)


def test_compile_expression_rejects_large_numeric_literal():
    with pytest.raises(SimulationError, match="Numeric literal is too large"):
        _compile_expression("x + 1000000000000")
