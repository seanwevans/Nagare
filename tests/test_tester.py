import subprocess
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parent.parent


@pytest.fixture(scope="module", autouse=True)
def build_tester():
    subprocess.run(["make", "tester"], cwd=ROOT, check=True)


def run_tester(*args: str) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [str(ROOT / "tester"), *args],
        cwd=ROOT,
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
