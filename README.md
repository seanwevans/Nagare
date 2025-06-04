# Nagare Simulation Framework

Nagare is a continuous, Befunge-like programming language designed for simulating dynamic systems involving vector fields, zones, and time-evolving entities. This project consists of a custom interpreter, simulation scripts, and visualization tools that facilitate the creation of field-based simulations with minimal effort.

## Project Structure

### Source Files
- **nagare.c** – Core implementation of the Nagare runtime. Defines structures for fields, zones, and execution flow. Handles boundary conditions and program evolution over time.
- **tester.c** – A multi-threaded simulation program that applies differential equations to evolve points, writes results to files, and simulates movement within zones.
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

