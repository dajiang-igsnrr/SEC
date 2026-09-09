#!/bin/bash
# Shell script for building MESC.

# --------------------------------------------------
# Load environment
#
# This will be specific to the machine you are working on and will need to be
# completed by the user. For example:
# ```
# module purge
# module load oneapi23u1   # load intel compiler
# module load netcdf_intel # load netcdf library
# ```
# --------------------------------------------------

# Ensure the build fails on hitting an error
set -e

# --------------------------------------------------
# Setup build directory
# --------------------------------------------------

BUILD_DIR=build
if [ ! -d "${BUILD_DIR}" ]; then
  mkdir "${BUILD_DIR}"
fi
cd "${BUILD_DIR}"

# --------------------------------------------------
# Fortran compiler
# --------------------------------------------------
if [ -z "${FC}" ]; then
  FC=ifort
fi

# --------------------------------------------------
# Configure & build
# --------------------------------------------------
cmake .. -DCMAKE_Fortran_COMPILER=${FC}
cmake --build . -j

echo "Build successful."
echo "The \$(main) executable is available in both test/ and build/"
