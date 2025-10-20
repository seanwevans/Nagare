"""Flask application serving the Nagare web-based simulation player."""
from __future__ import annotations

from pathlib import Path
from typing import Any, Dict

from flask import Flask, jsonify, request, send_from_directory

from .simulator import SimulationError, run_simulation

APP_ROOT = Path(__file__).resolve().parent

app = Flask(__name__, static_folder=str(APP_ROOT / "static"), static_url_path="/static")


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
    app.run(debug=True, host="0.0.0.0", port=5000)
