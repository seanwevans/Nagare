import pytest
from pathlib import Path

from nagare_interpreter import main, run


def test_main_requires_script_argument(capsys):
    with pytest.raises(SystemExit):
        main([])
    err = capsys.readouterr().err
    assert "usage:" in err


def test_main_respects_step_limit(capsys):
    path = Path(__file__).parent / "fixtures" / "display_and_finish.nagare"
    main([str(path), "--step-limit", "1"])
    captured = capsys.readouterr().out.strip().splitlines()
    assert captured == ["Hello zone", "Maximum steps reached"]


def test_run_handles_expression_error(capsys):
    success = run("x / 0", "y", [], step_limit=1)
    captured = capsys.readouterr()
    assert not success
    assert "Error evaluating x-expression 'x / 0': float division by zero" in captured.err


def test_main_exits_nonzero_on_expression_error(capsys):
    path = Path(__file__).parent / "fixtures" / "division_by_zero.nagare"
    with pytest.raises(SystemExit) as excinfo:
        main([str(path)])
    assert excinfo.value.code == 1
    captured = capsys.readouterr()
    assert "Error evaluating x-expression 'x / 0': float division by zero" in captured.err
