# Deploying Nagare

This document describes how Nagare goes from a repository of scripts to something
that is built, tested and shipped automatically.

There are two independently deployable artifacts:

| Artifact | What it is | Where it goes |
| --- | --- | --- |
| `docs/` playground | Static, browser-only 3D Nagare runtime (three.js + cannon-es) | GitHub Pages |
| `webapp/` player | Flask + gunicorn service exposing `POST /simulate` | Container image on GHCR |

The C sources (`nagare.c`, `ring.c`, `tester.c`) are built and exercised in CI but
are not yet released as binaries — see [Later stages](#later-stages).

---

## What runs on every change

`.github/workflows/ci.yml` runs on every pull request and every push to `main`:

- **Lint** — `ruff check` over the Python sources.
- **Python tests** — `pytest` with coverage on 3.10, 3.11, 3.12 and 3.13. This
  includes `tests/test_tester.py` and `tests/test_ring_buffer.py`, which compile
  and run the C programs.
- **C build** — `make all` with `-Wall -Wextra -Werror`, a rebuild under
  AddressSanitizer + UndefinedBehaviorSanitizer, and a `cppcheck` pass.
- **Container** — builds the image, boots it, and smoke-tests `/healthz` and a
  real `POST /simulate` round trip.

`.github/workflows/codeql.yml` runs CodeQL over both Python and C, on pull
requests and weekly.

`.github/dependabot.yml` opens weekly update PRs for pip, GitHub Actions and the
Docker base image. Those PRs go through the same CI, so an upgrade that breaks
the build is visible before it merges.

## What runs on release

- **Pages** — `.github/workflows/pages.yml` publishes `docs/` via the Pages
  deployment action on any push to `main` that touches `docs/`. This replaces
  the manual "Deploy from a branch → /docs" setting described in the README; in
  repository **Settings → Pages**, set the source to **GitHub Actions**.
  The playground loads three.js and cannon-es from `cdn.jsdelivr.net` through an
  import map pinned to exact versions, so the published page needs outbound
  access to that CDN; nothing is bundled or built ahead of time.
- **Container image** — `.github/workflows/release.yml` builds and pushes
  `ghcr.io/<owner>/<repo>` on any `v*` tag, tagged with the full version, the
  `major.minor` line and the commit SHA. Cutting a release is `git tag v0.1.0 &&
  git push origin v0.1.0`.

## Running the service

Never run `python -m webapp.app` in production — that is Flask's development
server. Use the WSGI entrypoint:

```bash
gunicorn --config gunicorn.conf.py webapp.wsgi:application
```

or the container:

```bash
docker build -t nagare-webapp .
docker run -p 8000:8000 nagare-webapp
```

The image runs as an unprivileged user (uid 10001) and declares a `HEALTHCHECK`.

### Configuration

| Variable | Default | Purpose |
| --- | --- | --- |
| `PORT` | `8000` | Listen port |
| `WEB_CONCURRENCY` | `min(4, 2*cpu+1)` | gunicorn worker processes |
| `WEB_THREADS` | `2` | Threads per worker |
| `WEB_TIMEOUT` | `30` | Worker timeout, seconds |
| `LOG_LEVEL` | `info` | Application and gunicorn log level |

`FLASK_DEBUG` and `HOST` remain for local development only.

### Operational endpoints

- `GET /healthz` → `{"status": "ok"}`. Point liveness and readiness probes here.
- `POST /simulate` accepts at most 256 KiB of JSON and returns `413` beyond that.
  Simulations are bounded to 2000 steps by `SimulationSettings.from_dict`, so a
  single request cannot run unbounded.

## Local development

```bash
make install-dev   # pip install -r requirements-dev.txt
make test          # pytest
make lint          # ruff check
make all           # build the C programs
```

`requirements.txt` holds runtime dependencies; `requirements-dev.txt` adds the
test and lint tooling.

---

## Security notes

`webapp/simulator.py` evaluates user-supplied vector-field expressions with
`eval`. It is guarded by an AST allow-list (permitted node types, an allow-list
of math functions, and caps on node count, literal magnitude, exponent and
argument count) and runs with `__builtins__` emptied. That is a meaningful
defence, but it is still an evaluator reachable from an unauthenticated public
endpoint. Before exposing the service on the open internet, add:

- rate limiting per client (e.g. `flask-limiter` behind the proxy's real-IP header),
- a CPU/wall-clock budget per request, since the step cap bounds iterations but
  not the cost of each one,
- and ideally a move from `eval` to a small explicit interpreter over the parsed
  AST, which removes the class of risk instead of fencing it.

## Later stages

Deliberately not in this change:

1. **Branch protection.** CI is only a gate once `main` requires these checks to
   pass. That is a repository setting, not a file in the tree - configure it in
   **Settings → Branches** once the first green run lands.
2. **Deployment target.** The release workflow publishes an image but does not
   deploy it anywhere. Wiring it to a host (Fly, Cloud Run, ECS, a VM) with a
   staging step and a documented rollback is the next infrastructure decision,
   and it depends on where you want to run it.
3. **Packaging.** Publishing `nagare-interpreter` as an installable distribution
   with a console script requires moving the flat top-level modules into a `src/`
   package layout, which touches every import and test path.
4. **Formatter and `pyupgrade` adoption.** `make format` runs `ruff format`, but
   CI does not enforce it, and the `UP` lint rules are off. Both reformat every
   file, so each belongs in its own commit.
5. **C artifact releases.** The release workflow ships the web player only;
   publishing built `nagare`/`tester` binaries per platform is a separate matrix.
