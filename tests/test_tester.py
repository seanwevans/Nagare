import subprocess
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[1]


def build_tester(tmp_path):
    exe_path = tmp_path / "tester"
    subprocess.run(
        ["gcc", str(ROOT / "tester.c"), "-o", str(exe_path), "-lpthread", "-lm"],
        check=True,
    )
    return exe_path


def test_simulation_stops_on_nan(tmp_path):
    tester_exe = build_tester(tmp_path)
    input_file = tmp_path / "nan_input.txt"
    input_file.write_text("nan 0.0\n")

    try:
        subprocess.run(
            [str(tester_exe), input_file.name],
            check=True,
            cwd=tmp_path,
            timeout=2,
        )
    except subprocess.TimeoutExpired as exc:
        pytest.fail(f"tester did not terminate on NaN input: {exc}")

    output_file = tmp_path / "nan_0.000000.txt"
    assert output_file.exists(), "simulation output file was not created"
    assert (
        output_file.read_text() == ""
    ), "simulation should stop before writing any steps for NaN input"
