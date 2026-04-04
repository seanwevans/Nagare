import subprocess
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parent.parent


@pytest.fixture(scope="module", autouse=True)
def build_tester():
    subprocess.run(["make", "tester"], cwd=ROOT, check=True)


def run_tester(*args: str, cwd: Path = ROOT) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [str(ROOT / "tester"), *args],
        cwd=cwd,
        text=True,
        capture_output=True,
    )


def test_tester_handles_empty_input(tmp_path):
    input_file = tmp_path / "empty_input.txt"
    input_file.write_text("")

    result = run_tester(str(input_file))

    assert result.returncode == 0
    assert result.stderr == ""


def test_tester_reports_missing_file(tmp_path):
    missing_file = tmp_path / "missing_input.txt"

    result = run_tester(str(missing_file))

    assert result.returncode != 0
    assert (
        f"Failed to load thread data from '{missing_file}':" in result.stderr
    )


def test_tester_overwrites_output_by_default(tmp_path):
    input_file = tmp_path / "single_point.txt"
    input_file.write_text("0.0 3.4e38\n")

    first_run = run_tester(str(input_file), cwd=tmp_path)
    assert first_run.returncode == 0
    output_file = next(path for path in tmp_path.glob("*.txt") if path != input_file)
    first_lines = output_file.read_text().splitlines()
    assert first_lines

    second_run = run_tester(str(input_file), cwd=tmp_path)
    assert second_run.returncode == 0
    second_lines = output_file.read_text().splitlines()

    assert second_lines == first_lines


def test_tester_append_flag_accumulates_output(tmp_path):
    input_file = tmp_path / "single_point.txt"
    input_file.write_text("0.0 3.4e38\n")

    first_run = run_tester("--append", str(input_file), cwd=tmp_path)
    assert first_run.returncode == 0
    output_file = next(path for path in tmp_path.glob("*.txt") if path != input_file)
    first_lines = output_file.read_text().splitlines()
    assert first_lines

    second_run = run_tester("--append", str(input_file), cwd=tmp_path)
    assert second_run.returncode == 0
    second_lines = output_file.read_text().splitlines()

    assert len(second_lines) == len(first_lines) * 2
