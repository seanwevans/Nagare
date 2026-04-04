"""Flask application serving the Nagare web-based simulation player."""
from __future__ import annotations

import os
from pathlib import Path
from typing import Any, Dict, Tuple

from flask import Flask, jsonify, request, send_from_directory

from .simulator import SimulationError, run_simulation

APP_ROOT = Path(__file__).resolve().parent

app = Flask(__name__, static_folder=str(APP_ROOT / "static"), static_url_path="/static")


def _parse_bool_env(value: str | None, *, default: bool = False) -> bool:
    """Parse a boolean environment variable value with a safe fallback."""
    if value is None:
        return default
    normalized = value.strip().lower()
    if normalized in {"1", "true", "t", "yes", "y", "on"}:
        return True
    if normalized in {"0", "false", "f", "no", "n", "off"}:
        return False
    return default


def _parse_int_env(value: str | None, *, default: int) -> int:
    """Parse an integer environment variable value with a safe fallback."""
    if value is None:
        return default
    try:
        return int(value.strip())
    except (TypeError, ValueError):
        return default


def get_server_config() -> Tuple[bool, str, int]:
    """Read Flask server settings from environment variables."""
    debug = _parse_bool_env(os.getenv("FLASK_DEBUG"), default=False)
    host = os.getenv("HOST", "127.0.0.1")
    port = _parse_int_env(os.getenv("PORT"), default=5000)
    return debug, host, port


@app.route("/")
def index() -> Any:
    return send_from_directory(app.static_folder, "index.html")


@app.post("/simulate")
def simulate() -> Any:
    if not request.is_json:
        return jsonify({"error": "Request must be JSON."}), 400
    payload: Dict[str, Any] = request.get_json(force=True)
    try:
        result = run_simulation(payload)
    except SimulationError as exc:
        return jsonify({"error": str(exc)}), 400
    return jsonify(result)


@app.errorhandler(404)
def not_found(_: Exception) -> Any:
    return send_from_directory(app.static_folder, "index.html")


if __name__ == "__main__":
    debug, host, port = get_server_config()
    app.run(debug=debug, host=host, port=port)
