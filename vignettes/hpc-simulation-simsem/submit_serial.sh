#!/bin/bash -l
#SBATCH -p compute
#SBATCH -A pv911001
#SBATCH -N 1
#SBATCH --cpus-per-task=1
#SBATCH --time=24:00:00
#SBATCH --job-name=simsem_serial
#SBATCH --array=1-320
#SBATCH --output=logs/simsem_serial_%A_%a.out

module load cray-R/4.4.0

Rscript --vanilla splitsimresult_hpc_serial.R
