# Production image for the Nagare web simulation player.
FROM python:3.12-slim AS runtime

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1 \
    PORT=8000

WORKDIR /app

COPY requirements.txt ./
RUN pip install --no-cache-dir -r requirements.txt

COPY webapp ./webapp
COPY gunicorn.conf.py ./

# Run as an unprivileged user.
RUN useradd --create-home --uid 10001 nagare \
    && chown -R nagare:nagare /app
USER nagare

EXPOSE 8000

HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
    CMD python -c "import urllib.request,sys; sys.exit(0 if urllib.request.urlopen('http://127.0.0.1:8000/healthz', timeout=2).status == 200 else 1)"

CMD ["gunicorn", "--config", "gunicorn.conf.py", "webapp.wsgi:application"]
