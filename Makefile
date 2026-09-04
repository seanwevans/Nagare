CC ?= gcc
CFLAGS ?= -O2 -Wall
PYTHON ?= python3

.PHONY: all
all: nagare tester ring

nagare: nagare.c
	$(CC) $(CFLAGS) nagare.c -o nagare

tester: tester.c
	$(CC) $(CFLAGS) tester.c -o tester -lpthread -lm

ring: ring.c
	$(CC) $(CFLAGS) ring.c -o ring -lpthread

.PHONY: install-dev
install-dev:
	$(PYTHON) -m pip install -r requirements-dev.txt

.PHONY: test
test:
	$(PYTHON) -m pytest

.PHONY: lint
lint:
	$(PYTHON) -m ruff check .

.PHONY: format
format:
	$(PYTHON) -m ruff format .

.PHONY: serve
serve:
	$(PYTHON) -m gunicorn --config gunicorn.conf.py webapp.wsgi:application

.PHONY: docker
docker:
	docker build -t nagare-webapp:local .

.PHONY: clean
clean:
	rm -f nagare tester ring
