#!/bin/bash -l
#SBATCH -p compute
#SBATCH -A pv911001
#SBATCH -N 1
#SBATCH --cpus-per-task=16
#SBATCH --time=24:00:00
#SBATCH --job-name=simsem_16
#SBATCH --array=1-16
#SBATCH --output=logs/simsem_16_%A_%a.out

module load cray-R/4.4.0

Rscript --vanilla splitsimresult_hpc_parallel16.R
