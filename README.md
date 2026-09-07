# Nagare Simulation Framework
<img width="256" alt="Swirling Blues and Teals" src="https://github.com/user-attachments/assets/e85267ab-0624-4410-bc11-3f763721dfb2" />

Nagare is a continuous, Befunge-like programming language designed for simulating dynamic systems involving vector fields, zones, and time-evolving entities. This project consists of a custom interpreter, simulation scripts, and visualization tools that facilitate the creation of field-based simulations with minimal effort.

## Project Structure

### Source Files
- **nagare.c** – Core implementation of the Nagare runtime. Defines structures for fields, zones, and execution flow. Handles boundary conditions and program evolution over time.
- **tester.c** – A multi-threaded simulation program that applies differential equations to evolve points, writes results to files, and simulates movement within zones. The simulator halts a trajectory as soon as any coordinate becomes non-finite (NaN or infinity) to prevent runaway outputs.
- **ring.c** – Thread-safe ring buffer utility for managing circular data storage.
- **pp.py** – A Python script that visualizes simulation output by generating an animated GIF from result files.
- **nagare_interpreter.py** – Minimal Python interpreter for running Nagare scripts without compiling the C code.

### Grammar Files
- **grammar.g4** – ANTLR grammar defining the Nagare DSL syntax, covering arithmetic expressions, boolean logic, zone definitions, and execution blocks.

### Simulation Files
- **sean.nagare** – Example simulation script written in Nagare. Defines zones and executes commands when points enter specific regions.

---

## How It Works
Nagare operates similarly to Befunge but extends the concept into continuous spaces rather than grid-based environments. The simulation steps through field programs that modify entity states. Key features include:
- **Zones** – Areas defined by shapes (e.g., ellipses) where special rules apply.
- **Program Flow** – The evolution of points follows vector fields and triggers events when entering/exiting zones.
- **Execution** – Commands like `printf` or `exit` are triggered when entities interact with zones.
- **Visualization** – Simulated data can be converted into animations for graphical analysis.

---

## Example Workflow
### 1. Write a Simulation (Example: `sean.nagare`)
```nagare
!/usr/bin/env nagare

BEGIN {
  program { x+1, y }
}

ZONES { 
  print { Ellipse((1.5, 0), 1, 1) }
  done  { Ellipse((2.5, 0), 1, 1) }
}

EXECUTE { 
  out<print> { display "Hello World!" }
  end<done>  { finish } 
}
```

### 2. Run the Simulation
You can either compile the C implementation or use the lightweight
Python interpreter:
```bash
python3 nagare_interpreter.py hello.nagare
```
If an expression in the program cannot be evaluated (for example, due to a
division by zero), the interpreter reports the error and exits with a non-zero
status code so scripts can detect failures.
The original C workflow is still available and shown below.
Compile and execute the core C programs:
```bash
# Compile the simulation programs
gcc nagare.c -o nagare
gcc tester.c -o tester -lpthread -lm

# Run the simulation script
./nagare sean.nagare
```

### 3. Visualize the Results
```bash
python3 pp.py out.txt
```
This generates an animated GIF of the simulation process.

---

## Web-Based Simulation Player

An interactive web application is included in `webapp/` for experimenting with Nagare
vector fields in real time. The player lets you define zones, author vector fields, place
entities, and watch their trajectories evolve alongside zone entry/exit events.

### Launching the App

1. Install the dependencies:
   ```bash
   pip install -r requirements.txt      # runtime only
   pip install -r requirements-dev.txt  # plus the test tooling
   ```
2. Start the development server from the project root:
   ```bash
   python -m webapp.app
   ```
   The server reads runtime settings from environment variables:
   - `FLASK_DEBUG` (default: `false`) — set to `true` for auto-reload and debug mode locally.
   - `HOST` (default: `127.0.0.1`) — set to `0.0.0.0` when you need LAN/container access.
   - `PORT` (default: `5000`) — choose a different port if `5000` is already in use.

### Running in Production

`python -m webapp.app` starts Flask's development server and is not suitable for
anything but local work. Serve the app through the WSGI entrypoint instead:

```bash
make serve   # gunicorn --config gunicorn.conf.py webapp.wsgi:application
```

or build and run the container, which does the same thing as an unprivileged
user with a health check wired to `/healthz`:

```bash
make docker
docker run -p 8000:8000 nagare-webapp:local
```

Both read `PORT`, `WEB_CONCURRENCY`, `WEB_THREADS`, `WEB_TIMEOUT` and `LOG_LEVEL`
from the environment; see `gunicorn.conf.py` for the defaults.

   Recommended local development command:
   ```bash
   FLASK_DEBUG=true HOST=127.0.0.1 PORT=5000 python -m webapp.app
   ```
3. Open your browser to <http://localhost:5000> to configure and run simulations.

Zones can be added via the sidebar, and entities are placed by clicking on the canvas.
Simulation results animate directly in the browser and log zone events in the UI.

---

## Installation and Dependencies
**Requirements:**
- C compiler (e.g., `gcc`)
- Python 3 with `matplotlib` and `imageio`
- ANTLR (if modifying the grammar)

**Install Python dependencies:**
```bash
pip install matplotlib imageio
```

---

## Contributing
Feel free to submit pull requests for bug fixes, new features, or additional simulation examples.

---

## License
This project is licensed under the MIT License. See `LICENSE` for details.


## GitHub Pages 3D Runtime Playground

A serverless Nagare runtime is available in `docs/` for deployment with GitHub Pages.
It renders with [three.js](https://threejs.org) and simulates with
[cannon-es](https://github.com/pmndrs/cannon-es), and runs entirely in the browser, so no
Flask server or Python process is required after publishing the static files.

### The 3D environment
- Entities are rigid bodies in a `cannon-es` world: they carry mass and momentum, bounce
  off `solid` zones and off each other, and can fall under gravity inside an optional
  walled arena.
- The Nagare program is a map on `(x, y, z)`. Each simulation step the map is evaluated
  at every entity's position and the difference becomes the field velocity, coupled into
  the bodies in one of two ways:
  - **Steer** — a force pushes each body toward the field velocity, so collisions,
    gravity and impulses can fight the field.
  - **Flow** — the field velocity is written straight onto the body, so trajectories
    follow the program exactly.
- Nagare's `z` axis is the vertical one; the three.js scene is `y`-up, and the conversion
  lives in `docs/src/space.js`. A 2D program therefore runs flat on the ground plane.

### Authoring
- `program name { x_expr, y_expr, z_expr }` defines the field. Two expressions still work
  and carry `z` through unchanged, so existing 2D programs keep running.
- Zones can be `Ellipsoid((cx, cy, cz), a, b, c)`, `Ellipse((cx, cy), a, b)`,
  `Sphere((cx, cy, cz), r)` or `Box((cx, cy, cz), hx, hy, hz)`. Adding `solid` also gives
  the zone a static collider.
- Actions are `display "message"`, `finish`, and `impulse(ix, iy, iz)` — a playground
  extension that kicks the entity that entered the zone.
- Expressions may use `x`, `y`, `z`, `t`, the constants `pi`, `e`, `tau`, and the usual
  math functions (`sin`, `cos`, `sqrt`, `hypot`, `atan2`, ...). Anything else is rejected
  before the expression is compiled.

### Interacting
- **Orbit** to move the camera; **Entity** to drop a body onto the highlighted build
  plane; **Zone** to drag out an ellipsoid, which is written into the `ZONES` block;
  **Erase** to remove a body, or a zone along with its `ZONES` and `EXECUTE` lines.
- The build plane's altitude, the height of new zones, gravity, field response, damping,
  restitution, entity radius and arena size are all live controls.
- Space toggles the run, `R` restarts it from the entities' start positions.

### Layout
```
docs/
  index.html      page shell and the import map for three.js / cannon-es
  style.css
  runtime.js      app: tools, simulation loop, field/physics coupling, UI
  src/nagare.js   the Nagare subset: expressions, zones, actions
  src/scene.js    three.js renderer, camera, orbit controls, lighting, picking
  src/physics.js  cannon-es world, entity bodies, solid zone colliders
  src/visuals.js  entity meshes and trails, zone volumes, labels
  src/space.js    Nagare (x, y, z) <-> world (x, z, y)
```
three.js and cannon-es are loaded from `cdn.jsdelivr.net` via an import map pinned to
exact versions, so there is no build step and nothing is vendored into the repository.

### Deploying on GitHub Pages
1. Commit the repository with the `docs/` directory present.
2. In the GitHub repository settings, choose **Pages**.
3. Set the source to **GitHub Actions**.
4. Save the configuration. The `Deploy Pages playground` workflow publishes
   `docs/` on every push to `main` that touches it.

You can also preview it locally without any build step:
```bash
python3 -m http.server 8000 --directory docs
```
Then open <http://localhost:8000>. The page needs outbound access to
`cdn.jsdelivr.net` for three.js and cannon-es; if that fails it says so instead of
rendering a blank scene.

---

## Continuous Integration and Delivery

Every pull request runs lint, the test suite on Python 3.10-3.13, a C build under
`-Werror` and ASan/UBSan, CodeQL, and a container build with a live smoke test.
Pushes to `main` that touch `docs/` deploy the playground to GitHub Pages, and
`v*` tags publish a container image to GHCR.

See [DEPLOYMENT.md](DEPLOYMENT.md) for the full pipeline, configuration
variables, operational endpoints and security notes.
