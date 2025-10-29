import re
import subprocess
from pathlib import Path


def test_ring_buffer_thread_synchronization(tmp_path):
    repo_root = Path(__file__).resolve().parents[1]
    source = (repo_root / "ring.c").read_text()

    writer_iters = int(re.search(r"#define\s+WRITER_ITERS\s+(\d+)", source).group(1))
    reader_iters = int(re.search(r"#define\s+READER_ITERS\s+(\d+)", source).group(1))
    capacity = int(
        re.search(r"create_buffer\(&cache,\s*(\d+)\s*,", source).group(1)
    )

    executable = tmp_path / "ring"

    compile_cmd = [
        "gcc",
        "-O2",
        "-Wall",
        "ring.c",
        "-o",
        str(executable),
        "-lpthread",
    ]
    subprocess.run(compile_cmd, cwd=repo_root, check=True)

    run = subprocess.run(
        [str(executable)], cwd=repo_root, check=True, capture_output=True, text=True
    )

    tokens = run.stdout.strip().split()
    assert tokens, "ring program should emit buffer contents"

    valid_tokens = {"NULL"}
    valid_tokens.update(str(i) for i in range(writer_iters))
    assert all(token in valid_tokens for token in tokens)
    assert any(token != "NULL" for token in tokens)

    assert len(tokens) == capacity * reader_iters
