import math
import pytest
from nagare_interpreter import eval_expr


def test_arithmetic_expression():
    assert eval_expr("x + y * 2 - 3", 3, 4) == 3 + 4 * 2 - 3


def test_trigonometric_functions():
    result = eval_expr("sin(x) + cos(y)", math.pi / 2, 0)
    assert result == pytest.approx(2.0)


def test_rejects_unsafe_constructs():
    with pytest.raises(ValueError):
        eval_expr("__import__('os').system('echo unsafe')", 0, 0)
