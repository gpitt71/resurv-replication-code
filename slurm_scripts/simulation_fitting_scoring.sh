#!/bin/bash

set -euo pipefail

PROJECT_ROOT="${RESURV_PROJECT_ROOT:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"

if [[ -n "${CONDA_DIR:-}" && -f "${CONDA_DIR}/etc/profile.d/conda.sh" ]]; then
  # shellcheck disable=SC1090
  source "${CONDA_DIR}/etc/profile.d/conda.sh"
fi

if [[ -n "${RESURV_CONDA_ENV:-}" ]]; then
  conda activate "${RESURV_CONDA_ENV}"
fi

Rscript --vanilla "${PROJECT_ROOT}/Fitting_Scoring/Simulation_scripts/simulation_cl_scoring1.R"
Rscript --vanilla "${PROJECT_ROOT}/Fitting_Scoring/Simulation_scripts/simulation_cl_scoring2.R"
Rscript --vanilla "${PROJECT_ROOT}/Fitting_Scoring/Simulation_scripts/simulation_fitting.R" cox
Rscript --vanilla "${PROJECT_ROOT}/Fitting_Scoring/Simulation_scripts/simulation_fitting.R" deepsurv
Rscript --vanilla "${PROJECT_ROOT}/Fitting_Scoring/Simulation_scripts/simulation_fitting.R" xgboost
Rscript --vanilla "${PROJECT_ROOT}/Fitting_Scoring/Simulation_scripts/simulation_scoring.R" cox
Rscript --vanilla "${PROJECT_ROOT}/Fitting_Scoring/Simulation_scripts/simulation_scoring.R" deepsurv
Rscript --vanilla "${PROJECT_ROOT}/Fitting_Scoring/Simulation_scripts/simulation_scoring.R" xgboost
