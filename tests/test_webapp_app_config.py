from webapp.app import get_server_config


def test_get_server_config_defaults(monkeypatch):
    monkeypatch.delenv("FLASK_DEBUG", raising=False)
    monkeypatch.delenv("HOST", raising=False)
    monkeypatch.delenv("PORT", raising=False)

    debug, host, port = get_server_config()

    assert debug is False
    assert host == "127.0.0.1"
    assert port == 5000


def test_get_server_config_reads_env(monkeypatch):
    monkeypatch.setenv("FLASK_DEBUG", "true")
    monkeypatch.setenv("HOST", "0.0.0.0")
    monkeypatch.setenv("PORT", "8001")

    debug, host, port = get_server_config()

    assert debug is True
    assert host == "0.0.0.0"
    assert port == 8001


def test_get_server_config_handles_invalid_port(monkeypatch):
    monkeypatch.setenv("PORT", "not-a-port")

    _, _, port = get_server_config()

    assert port == 5000
