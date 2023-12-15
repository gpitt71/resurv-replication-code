#!/bin/bash
#SBATCH--time=78:00:00
#SBATCH--partition="modi_long"
#SBATCH--cpus-per-task=6
#SBATCH--mem-per-cpu=20G   # memory per cpu-core
#SBATCH--nodelist="n006"
#SBATCH--ntasks=1
#SBATCH--ntasks-per-node=1

$srun singularity exec ~/modi_images/hpc-notebook-22.05.9.sif \
    /home/gabriele_pittarello_uniroma1_it/modi_mount/Scoring/Simulation_scripts/simulation_fitting_scoring.sh
