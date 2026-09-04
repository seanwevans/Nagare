from pathlib import Path

DOCS = Path(__file__).resolve().parents[1] / "docs"


def test_github_pages_runtime_uses_relative_static_assets():
    index = (DOCS / "index.html").read_text()

    assert 'href="./style.css"' in index
    assert 'src="./runtime.js"' in index
    assert '/static/' not in index


def test_github_pages_runtime_exposes_drawing_and_execution_controls():
    index = (DOCS / "index.html").read_text()
    runtime = (DOCS / "runtime.js").read_text()

    assert 'data-tool="ellipse"' in index
    assert 'id="runProgram"' in index
    assert 'function parseNagare' in runtime
    assert 'function runNagare' in runtime
    assert 'function addDrawnZone' in runtime
