#!/bin/bash
set -e

# --------------------------------------------------
# Load environment (users can edit if needed)
# --------------------------------------------------
module purge
module load oneapi23u1   # load intel compiler
module load netcdf_intel # load netcdf library

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
cmake .. -DCMAKE_Fortran_COMPILER=ifort
cmake --build . -j

echo "Build finished."
echo "Executable is available in: SEC/test/main or SEC/build"
