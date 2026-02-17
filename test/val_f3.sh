#!/bin/bash
#SBATCH -A OD-230931
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=4gb
#SBATCH --time=0:20:00
#SBATCH --output="test_f3.out"
#SBATCH --error="test_f3.err"

cd /home/wan028/mes-c/SEC/test
##################################
# user settings
# # ################################
#
case="f3"
code_dir="/home/wan028/mes-c/SEC/src"
param_dir="/home/wan028/mes-c/SEC/test/input"
work_dir="/home/wan028/mes-c/SEC/test"

cd ${work_dir}

###############################################
# Environment setup
# ###############################################
 echo "===== Job started: $(date) ====="
 echo "Case: ${case}, opt: ${opt}"
 echo "Working directory: ${work_dir}"

# Load modules (modify if using different compilers)
 module unload netcdf
 module load netcdf/4.8.1-intel20

 ulimit -s unlimited

# clean up
 rm runfrac
 rm fort.*
 rm *.f90
 rm val*.txt 
 rm params1.txt
 rm params_val.txt
 rm case.txt

# copy codes across
 cp ${code_dir}/mesc_*.f90 .
 cp ${code_dir}/main.f90 .
# copy the parameter file
 cp ${param_dir}/case_f3.txt case.txt
 cp ${param_dir}/params1_frc2_${case}.txt params1.txt
 cp ${param_dir}/params_val_f3.txt params_val.txt

cat main.f90 mesc_variable.f90 mesc_function.f90 mesc_inout.f90 mesc_interface.f90 mesc_model.f90 mesc_cost.f90 >model5.f90
ifort -o runfrac model5.f90 -L/apps/netcdf/4.8.1-intel20/lib/Intel -lnetcdff -lnetcdf -qopenmp
export OMP_NUM_THREADS=8

./runfrac >output/outval_${case}.txt
mv fort.91 output/valfrc2_91_${case}.txt
mv fort.92 output/valfrc2_92_${case}.txt

echo "===== Job finished: $(date) ====="
