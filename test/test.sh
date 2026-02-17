#!/bin/bash
#SBATCH -A OD-230931
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=4gb
#SBATCH --time=0:20:00
#SBATCH --output="test.out"
#SBATCH --error="test.err"

cd /home/wan028/mes-c/SEC/test

jid1=$(sbatch val_f3.sh | awk '{print $4}')
sbatch --dependency=afterok:$jid1 valc_c3.sh


