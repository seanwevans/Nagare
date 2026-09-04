import pytest

from webapp.app import app


@pytest.fixture
def client():
    app.config.update(TESTING=True)
    with app.test_client() as client:
        yield client


def _base_payload():
    return {
        "vectorField": {"fx": "0", "fy": "0"},
        "entities": [{"id": "entity-0", "x": 0, "y": 0}],
        "settings": {"duration": 1, "dt": 1},
    }


@pytest.mark.parametrize(
    "zone_payload, expected_fragment",
    [
        ({"type": "circle", "r": "not-a-number"}, "invalid value for 'r'"),
        ({"type": "rect", "w": "oops", "h": 1}, "invalid value for 'w'"),
    ],
)
def test_simulate_rejects_non_numeric_zone_parameters(client, zone_payload, expected_fragment):
    payload = _base_payload()
    payload["zones"] = [zone_payload]

    response = client.post("/simulate", json=payload)

    assert response.status_code == 400
    data = response.get_json()
    assert expected_fragment in data["error"]


def test_healthz_reports_ok():
    client = app.test_client()
    response = client.get("/healthz")
    assert response.status_code == 200
    assert response.get_json() == {"status": "ok"}


def test_simulate_rejects_non_object_payload():
    client = app.test_client()
    response = client.post("/simulate", json=[1, 2, 3])
    assert response.status_code == 400
    assert "JSON object" in response.get_json()["error"]


def test_simulate_rejects_oversized_payload():
    client = app.test_client()
    response = client.post(
        "/simulate",
        data=b"{" + b" " * (app.config["MAX_CONTENT_LENGTH"] + 1) + b"}",
        content_type="application/json",
    )
    assert response.status_code == 413


def test_wsgi_entrypoint_exposes_application():
    from webapp.wsgi import application

    assert application is app
