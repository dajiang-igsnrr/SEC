#!/bin/bash
set -e

# --------------------------------------------------
# Load environment (users can edit if needed)
# --------------------------------------------------
module purge
module load nvhpc netcdf_hpc

# --------------------------------------------------
# Build directory
# --------------------------------------------------
BUILD_DIR=build

if [ ! -d "$BUILD_DIR" ]; then
  mkdir "$BUILD_DIR"
fi

cd "$BUILD_DIR"

# --------------------------------------------------
# Configure & build
# --------------------------------------------------
cmake .. -DCMAKE_Fortran_COMPILER=nvfortran
cmake --build . -j

echo "Build finished."
echo "Executable is available in: SEC/test/main or SEC/build"
