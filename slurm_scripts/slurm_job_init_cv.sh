#!/bin/bash
#SBATCH--time=78:00:00
#SBATCH--partition="modi_long"
#SBATCH--cpus-per-task=50
#SBATCH--mem-per-cpu=5G   # memory per cpu-core
#SBATCH--nodelist="modi001"
#SBATCH--ntasks=1
#SBATCH--ntasks-per-node=1

$srun singularity exec ~/modi_images/hpc-notebook-latest.sif \
    ~/cross_validation_scripts/simulation_0/slurm_job_cv.sh