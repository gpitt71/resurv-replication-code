#!/bin/bash

source $CONDA_DIR/etc/profile.d/conda.sh
conda activate /home/gabriele_pittarello_uniroma1_it/modi_mount/r_environ

Rscript --vanilla /home/gabriele_pittarello_uniroma1_it/modi_mount/Scoring/Simulation_scripts/simulation_cl_scoring1.R
Rscript --vanilla /home/gabriele_pittarello_uniroma1_it/modi_mount/Scoring/Simulation_scripts/simulation_cl_scoring2.R 
Rscript --vanilla /home/gabriele_pittarello_uniroma1_it/modi_mount/Scoring/Simulation_scripts/simulation_fitting.R cox
Rscript --vanilla /home/gabriele_pittarello_uniroma1_it/modi_mount/Scoring/Simulation_scripts/simulation_fitting.R deepsurv
Rscript --vanilla /home/gabriele_pittarello_uniroma1_it/modi_mount/Scoring/Simulation_scripts/simulation_fitting.R xgboost
Rscript --vanilla /home/gabriele_pittarello_uniroma1_it/modi_mount/Scoring/Simulation_scripts/simulation_scoring.R cox
Rscript --vanilla /home/gabriele_pittarello_uniroma1_it/modi_mount/Scoring/Simulation_scripts/simulation_scoring.R deepsurv
Rscript --vanilla /home/gabriele_pittarello_uniroma1_it/modi_mount/Scoring/Simulation_scripts/simulation_scoring.R xgboost
