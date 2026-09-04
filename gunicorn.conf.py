"""Gunicorn settings for the Nagare web player."""
import multiprocessing
import os

bind = f"0.0.0.0:{os.getenv('PORT', '8000')}"
workers = int(os.getenv("WEB_CONCURRENCY", str(min(4, (multiprocessing.cpu_count() * 2) + 1))))
threads = int(os.getenv("WEB_THREADS", "2"))

# Simulations are bounded to 2000 steps, so requests are short-lived.
timeout = int(os.getenv("WEB_TIMEOUT", "30"))
graceful_timeout = 20
keepalive = 5

# Recycle workers periodically to bound memory growth.
max_requests = 1000
max_requests_jitter = 100

accesslog = "-"
errorlog = "-"
loglevel = os.getenv("LOG_LEVEL", "info").lower()
