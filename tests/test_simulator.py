from webapp.app import app


def test_simulate_rejects_non_numeric_duration():
    client = app.test_client()
    payload = {
        "vectorField": {"fx": "0", "fy": "0"},
        "entities": [{"id": "entity-1", "x": 0, "y": 0}],
        "settings": {"duration": "not-a-number", "dt": 0.1},
    }

    response = client.post("/simulate", json=payload)

    assert response.status_code == 400
    data = response.get_json()
    assert data == {"error": "Invalid duration 'not-a-number'; provide a numeric value."}
