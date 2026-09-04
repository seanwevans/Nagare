"""Flask application serving the Nagare web-based simulation player."""
from __future__ import annotations

import logging
import os
from pathlib import Path
from typing import Any, Tuple

from flask import Flask, jsonify, request, send_from_directory

from .simulator import SimulationError, run_simulation

APP_ROOT = Path(__file__).resolve().parent

#: Largest accepted /simulate request body. Payloads are small JSON documents;
#: anything larger is rejected before Flask buffers it into memory.
MAX_CONTENT_LENGTH = 256 * 1024

app = Flask(__name__, static_folder=str(APP_ROOT / "static"), static_url_path="/static")
app.config["MAX_CONTENT_LENGTH"] = MAX_CONTENT_LENGTH

logging.basicConfig(
    level=os.getenv("LOG_LEVEL", "INFO").upper(),
    format="%(asctime)s %(levelname)s %(name)s %(message)s",
)


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


@app.get("/healthz")
def healthz() -> Any:
    """Liveness/readiness probe for load balancers and orchestrators."""
    return jsonify({"status": "ok"})


@app.post("/simulate")
def simulate() -> Any:
    if not request.is_json:
        return jsonify({"error": "Request must be JSON."}), 400
    payload = request.get_json(silent=True)
    if not isinstance(payload, dict):
        return jsonify({"error": "Request body must be a JSON object."}), 400
    try:
        result = run_simulation(payload)
    except SimulationError as exc:
        return jsonify({"error": str(exc)}), 400
    return jsonify(result)


@app.errorhandler(413)
def payload_too_large(_: Exception) -> Any:
    return jsonify({"error": "Request body is too large."}), 413


@app.errorhandler(404)
def not_found(_: Exception) -> Any:
    return send_from_directory(app.static_folder, "index.html")


@app.errorhandler(500)
def internal_error(exc: Exception) -> Any:
    app.logger.exception("Unhandled error while serving request", exc_info=exc)
    return jsonify({"error": "Internal server error."}), 500


if __name__ == "__main__":
    debug, host, port = get_server_config()
    app.run(debug=debug, host=host, port=port)
