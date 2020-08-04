#!/bin/bash  --login

#SBATCH --job-name=evipro
#SBATCH --time=02:00:00
#SBATCH --nodes=1
#SBATCH --partition=short
#SBATCH --tasks-per-node=1
#SBATCH --cpus-per-task=36
#SBATCH --mem=160G
#SBATCH --account=aes4t

# MODIFY HERE according to your environment setup
module purge
. activate ~/.conda-envs/r-env

Rscript preprocess.r
