#!/bin/bash

set -euo pipefail

if [[ -n "${CONDA_DIR:-}" && -f "${CONDA_DIR}/etc/profile.d/conda.sh" ]]; then
  # shellcheck disable=SC1090
  source "${CONDA_DIR}/etc/profile.d/conda.sh"
fi

if [[ -n "${RESURV_CONDA_ENV:-}" ]]; then
  conda activate "${RESURV_CONDA_ENV}"
fi

PROJECT_ROOT="${RESURV_PROJECT_ROOT:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"

Rscript --vanilla "${PROJECT_ROOT}/cross_validation_scripts/simulation_0/bayes_xgboost.R"
Rscript --vanilla "${PROJECT_ROOT}/cross_validation_scripts/simulation_0/bayes_deepsurv.R"
