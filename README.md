# SEC 
*A Fortran-based pmicrobial-explicit soil carbon cycle model.*

---

## 1. Overview

The **SEC model** is a process-oriented soil carbon model that explicitly represents microbial decomposition processes (using Michaelis-Menten kinetics) implemented in **Fortran**, designed to simulate soil and ecosystem carbon dynamics and their controlling mechanisms.

The model adopts a **modular architecture**, clearly separating core process representation, input/output handling, and model control logic.  
It is compiled using **NVHPC (`nvfortran`)** and linked against **netCDF (C + Fortran)**, enabling efficient handling of structured scientific data and deployment on **high-performance computing (HPC) systems**.

---

## 2. Code Structure and Design
```text
SEC/
├── src/                  # Core Fortran source code
│   ├── main.f90          # main program or test program
│   ├── mesc_cost.f90
│   ├── mesc_function.f90
│   ├── mesc_input.f90
│   ├── mesc_interface.f90
│   ├── mesc_model.f90
│   └── mesc_variable.f90
│
├── cmake/                # CMake helper modules
│   └── FindNetCDFFortran.cmake
│
├── doc/                  # Documentation
│   ├── html              # html
│   └── readme            
│
├── test/                 # Test and example runs
│
├── CMakeLists.txt        # Build configuration
├── build.sh              # One-command build script
├── README.md
└── LICENSE
```

---

## 3. Software Requirements

The SEC model has been developed and tested in the following software environment:

- **Fortran compiler**: NVHPC (`nvfortran`) version **25.11**
- **netCDF-Fortran**: version **4.6.1**
- **netCDF-C**: version **4.9.2**

On HPC systems, the required environment is typically provided via modules, for example:

```bash
module load nvhpc 
module load netcdf_hpc
```

---

## 4. Building the Model

Recommended: One-command build

```bash
chmod +x build
./build.sh
```

This script automatically:
	1.	Loads the required compiler and libraries
	2.	Creates an out-of-source build directory
	3.	Configures and builds the model
	4.	Copies the executable to **SEC/build/** or **SEC/test/**

The final executable is located at: **SEC/build/main** or **SEC/test/main**

Manual build (for development)
```bash
module load nvhpc 
module load netcdf_hpc
mkdir build
cd build
cmake .. -DCMAKE_Fortran_COMPILER=nvfortran
cmake --build . -j
```

---

## 5. Running the Model

Input files (e.g., parameter files or netCDF datasets) should be placed in the same directory to ensure consistent input–output management.

```bash
cd /SEC/test
./main
```
