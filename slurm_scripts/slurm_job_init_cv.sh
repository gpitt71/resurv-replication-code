#!/bin/bash
#SBATCH --time=78:00:00
#SBATCH --partition="modi_long"
#SBATCH --cpus-per-task=50
#SBATCH --mem-per-cpu=5G   # memory per cpu-core
#SBATCH --nodelist="modi001"
#SBATCH --ntasks=1
#SBATCH --ntasks-per-node=1

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="${RESURV_PROJECT_ROOT:-$(cd "${SCRIPT_DIR}/.." && pwd)}"
SINGULARITY_IMAGE="${RESURV_SINGULARITY_IMAGE:-${HOME}/modi_images/hpc-notebook-latest.sif}"

$srun singularity exec "${SINGULARITY_IMAGE}" \
    "${PROJECT_ROOT}/slurm_scripts/slurm_job_cv.sh"
