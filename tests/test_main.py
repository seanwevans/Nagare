import pytest
from pathlib import Path

from nagare_interpreter import main


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
