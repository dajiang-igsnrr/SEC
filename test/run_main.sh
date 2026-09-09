#!/bin/bash
# Shell script for running the MESC test suite.

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

# Ensure test suite fails on hitting an error
set -e

# --------------------------------------------------
# Set environment variables
# --------------------------------------------------
export OMP_NUM_THREADS=8

# --------------------------------------------------
# Remove old files
# --------------------------------------------------
rm -f fort.*
rm -f val*.txt
rm -f params1.txt
rm -f params_val.txt
rm -f case.txt

# --------------------------------------------------
# Configure test cases to be run
# --------------------------------------------------
cases=("f3" "cable3")
runs=("frc" "hwsd")

# --------------------------------------------------
# Loop over test cases
# --------------------------------------------------
rm -rf output
mkdir -p output
for i in 0 1; do
  case="${cases[${i}]}"
  run="${runs[${i}]}"
  echo "Running test case '${case}', run '${run}'"

  # --------------------------------------------------
  # Copy parameter files
  # --------------------------------------------------
  cp ./input/mesc_${run}_${case}.nml mesc.nml
  cp ./input/parameters_${run}_${case}.txt parameters.txt
  cp ./input/params_val_${run}_${case}.txt params_val.txt

  # --------------------------------------------------
  # Run the test case
  # --------------------------------------------------
  START="$(date +%s)"
  ./main >output/outval_${case}_${run}.txt
  DURATION=$(($(date +%s) - ${START}))
  echo "Time taken: ${DURATION} seconds"
  if [ -e fort.91 ]; then
    mv fort.91 output/valsoc_91_${case}_${run}.txt
    diff benchmark/valsoc_91_${case}_${run}.txt output/valsoc_91_${case}_${run}.txt >output/diff_valsoc_91_${case}_${run}.txt
  fi
  if [ -e fort.92 ]; then
    mv fort.92 output/valsoc_92_${case}_${run}.txt
    diff benchmark/valsoc_92_${case}_${run}.txt output/valsoc_92_${case}_${run}.txt >output/diff_valsoc_92_${case}_${run}.txt
  fi
done

# --------------------------------------------------
# Report test statuses
# --------------------------------------------------
for i in {0..1}; do
  case="${cases[${i}]}"
  run="${runs[${i}]}"
  pass=true
  for id in 91 92; do
    # Check the output file exists and is not empty
    if [ ! -e "output/valsoc_${id}_${case}_${run}.txt" ]; then
      pass=false
      break
    fi
    if [ -z "output/valsoc_${id}_${case}_${run}.txt" ]; then
      pass=false
      break
    fi
    # Check the diff file exists and is empty
    if [ -s "output/diff_valsoc_${id}_${case}_${run}.txt" ]; then
      pass=false
      break
    fi
  done
  if [ ${pass} == false ]; then
    echo "FAIL: test case '${case}', run '${run}'"
  else
    echo "PASS: test case '${case}', run '${run}'"
  fi
done
rm -f output/diff_*.txt
echo "===== Job finished: $(date) ====="
