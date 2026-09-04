"""WSGI entrypoint for production servers.

Run with a real WSGI server rather than Flask's development server::

    gunicorn --bind 0.0.0.0:8000 webapp.wsgi:application
"""
from __future__ import annotations

from .app import app as application

__all__ = ["application"]
