#!/bin/bash

source $CONDA_DIR/etc/profile.d/conda.sh
conda activate /home/xkc626_ku_dk/modi_mount/r_env

Rscript ~/modi_mount/cross_validation_scripts/simulation_0/bayes_xgboost.R
Rscript ~/modi_mount/cross_validation_scripts/simulation_0/bayes_deepsurv.R