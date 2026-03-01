# --------------------------------------------------
# Load environment (users can edit if needed)
# --------------------------------------------------
module purge
module load oneapi23u1 netcdf_intel

# --------------------------------------------------
# remove old files 
# --------------------------------------------------
rm -rf fort.*
rm -rf val*.txt 
rm -rf params1.txt
rm -rf params_val.txt
rm -rf case.txt

case="c7"
# --------------------------------------------------
# copy parameter files 
# --------------------------------------------------
cp ./input/case_${case}.txt case.txt
cp ./input/params1_hwsd_${case}.txt params1.txt
cp ./input/params_val_${case}.txt params_val.txt

# --------------------------------------------------
# run 
# --------------------------------------------------
export OMP_NUM_THREADS=8
./main > output/outval_${case}.txt
mv fort.91 output/valsoc_91_${case}.txt
mv fort.92 output/valsoc_92_${case}.txt

echo "===== Job finished: $(date) ====="
