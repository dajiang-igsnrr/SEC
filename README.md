# MESC

*A Fortran-based microbial-explicit soil carbon cycle model.*

---

## 1. Overview

The **MESC model** is a process-oriented soil carbon model that explicitly represents microbial decomposition processes (using Michaelis-Menten kinetics) implemented in **Fortran**, designed to simulate soil and ecosystem carbon dynamics and their controlling mechanisms.

The model adopts a **modular architecture**, clearly separating core process representation, input/output handling, and model control logic.  
It makes use of **netCDF (C + Fortran)**, enabling efficient handling of structured scientific data and deployment on **high-performance computing (HPC) systems**.

---

## 2. Code Structure and Design

```text
MESC/
├── src/                   # Core Fortran source code
│   ├── main.f90           # Main program for running MESC as an executable
│   ├── mod_calcost.f90    # Module for computing cost functions
│   ├── mod_constants.f90  # Module defining all constants
│   ├── mod_functions.f90  # Module defining orchestrator functions
│   ├── mod_inout.f90      # Module for handling input and output
│   ├── mod_interface.f90  # Module containing high-level driver interface
│   ├── mod_model_core.f90 # Module containing core model physics
│   └── mod_variables.f90  # Module defining all model variables
│
├── auxil/                 # Auxiliary code - TODO
│
├── cmake/                 # CMake helper modules
│   ├── FindNetCDF.cmake
│   └── FindNetCDFFortran.cmake
│
├── test/                  # Test and example runs
│   ├── benchmark          # 
│   ├── input              # Input data for the tests goes here
│   ├── output             # Output data from the tests goes here
│   ├── run_main.sh        # One-command run test script
│   └── README.md          # Documentation for test suite
│
├── pre-processing/        # Pre-processing code
│   ├── cable              # Code for handling outputs from CABLE model
│   ├── ORCHIDEE           # Code for handling outputs from ORCHIDEE model
│   ├── available_USDA_SoilSuborder_mask.py # 
│   ├── convert_scale.bash                  # 
│   ├── resample_USDA_SoilSuborder.py       # 
│   └── README.md          # Documentation for pre-processing code
│
├── post-processing/       # Post-processing code - TODO
│
├── CMakeLists.txt         # Build configuration
├── build.sh               # One-command build script
├── README.md              # High-level documentation for repository
├── mesc.md                # Configuration for FORD API documentation
├── fortitude.toml         # Configuration for Fortitude Fortran linter
├── requirements-dev.txt   # Python developer dependencies
└── LICENSE                # MESC license
```

---

## 3. Software Requirements

The MESC model has been developed and tested in the following software environments:

- **Fortran compiler**:
   - Legacy Intel compiler (`ifort`) version **2021.9.0**
   - Intel compiler (`ifx`) version **2025.3.0**
- **netCDF-Fortran**: versions **4.6.1**, **4.6.2**
- **netCDF-C**: versions **4.9.2**, **4.9.3**

On HPC systems, the required environment is typically provided via modules, for example:

```bash
module load oneapi23u1
module load netcdf_intel
```

The exact modules used will be specific to the HPC system.

### Python requirements

Python is used in MESC's pre-processing steps. If you intend to run scripts in
the [pre-processing](pre-processing) subdirectory then you will need to install
the relevant Python modules. To do this, create and activate a
[Python virtual environment](https://www.datasciencebase.com/fundamentals/python/environment-setup/)
and install via
```sh
pip install -r requirements.txt
```

---

## 4. Building the Model

Recommended: One-command build

```bash
./build.sh
```

This script automatically:

1. Loads the required compiler and libraries
2. Creates an out-of-source build directory
3. Configures and builds the model
4. Copies the executable to `build/` or `test/`

The final executable is located at: `build/main` or `test/main`

Manual build (for development)
```bash
module load oneapi23u1 
module load netcdf_intel
mkdir build
cd build
cmake .. -DCMAKE_Fortran_COMPILER=ifort
cmake --build . -j
```

---

## 5. Running the Model

Input files (e.g., parameter files or netCDF datasets) should be placed in the same directory to ensure consistent input–output management.

```bash
cd test
./run_main.sh
```

---

## 6. Generating API Documentation

API documentation is generated with [FORD](https://forddocs.readthedocs.io/), a Fortran documentation generator.

To install FORD, create and activate a
[Python virtual environment](https://www.datasciencebase.com/fundamentals/python/environment-setup/)
and install via
```bash
pip install ford
```

Optionally, install `graphviz` to generate dependency graphs with FORD. The
installation method will differ depending on your operating system. On Ubuntu,
for example, run
```sh
sudo apt update
sudo apt install graphviz
```

Once you have installed FORD, generate the API documentation with
```bash
ford mesc.md
```

This reads `!>` and `!!` comment blocks from `src/*.f90` and produces static
HTML output in `doc/`.

The documentation will be generated in the `doc/` subdirectory and can be viewed
using
```bash
open doc/index.html
```
(or as appropriate for your operating system).

To regenerate after updating doc comments, simply re-run `ford mesc.md`.

---

## Developer setup

We make use of [Fortitude](https://fortitude.readthedocs.io/en/stable/) for
Fortran linting. To install Fortitude, either create and activate a
[Python virtual environment](https://www.datasciencebase.com/fundamentals/python/environment-setup/)
and install via
```sh
pip install -r requirements-dev.txt
```
or follow the instructions
[on the Fortitude website](https://fortitude.readthedocs.io/en/stable/installation/).

To apply Fortitude for linting the MESC code base, run
```sh
fortitude check
```
from the command line. This will report any issues, given the configuration in
[`fortitude.toml`](fortitude.toml).
