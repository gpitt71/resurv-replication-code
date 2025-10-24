[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10419198.svg)](https://doi.org/10.5281/zenodo.10419198)

# resurv-replication-code

This repository contains the code to replicate the simulated case study of the manuscript *A MACHINE LEARNING APPROACH BASED ON SURVIVAL ANALYSIS FOR IBNR FREQUENCIES IN NON-LIFE RESERVING*.

The computations for obtaining the results in the manuscript were performed on the ERDA cloud (see Appendix *Computational Details* of the manuscript).

> We do not share the private data on which we performed the real data case study.

The repository is organised as follows:


```
| resurv-replication-code
   |_ Fitting_Scoring
   |    |_ Scoring_results
   |    |_ Simulation_scripts
   |    |_ summarize_simulation_results.R
   |_ ReSurv_cv_results
   |_ cross_validation_scripts
   |    |_ simulation_0 ... simulation_5
   |         |_ bayes_deepsurv.R
   |         |_ bayes_w18.R
   |         |_ bayes_xgboost.R
   |_ slurm_scripts
        |_ simulation_fitting_scoring.sh
        |_ simulation_fitting_scoring_slurm.sh
        |_ slurm_job_cv.sh
        |_ slurm_job_init_cv.sh
```


## Folders

| Folder / File                     | Description |
| :-------------------------------: | :---------- |
| `Fitting_Scoring`                 | Orchestrates model fitting, scoring and post-processing. |
| `Fitting_Scoring/Scoring_results` | Stores intermediate scoring outputs generated during the simulations. |
| `Fitting_Scoring/Simulation_scripts` | Simulation drivers that fit and score chain ladder, Cox, XGBoost and neural network models for each scenario family. |
| `Fitting_Scoring/summarize_simulation_results.R` | Collects the simulation outputs and prepares the summary tables reported in the manuscript. |
| `ReSurv_cv_results`               | Cross-validation results for every simulation scenario and model. |
| `cross_validation_scripts`        | Hyper-parameter tuning scripts grouped per simulation (`simulation_Alpha`–`simulation_Epsilon`). Each folder contains the R scripts listed below. |
| `slurm_scripts`                   | Submission helpers for running the workflow on a Slurm cluster (examples listed below). |

## Scripts

| Script                                                      | Location                              | Description                                                                 |
|-------------------------------------------------------------|---------------------------------------|------------------------------------------------------------------------------|
| `bayes_deepsurv.R`                                          | `cross_validation_scripts/simulation_*` | Tunes DeepSurv neural network hyperparameters for each simulation setting.   |
| `bayes_w18.R`                                               | `cross_validation_scripts/simulation_*` | Tunes W18 Cox model hyperparameters for each simulation setting.             |
| `bayes_xgboost.R`                                           | `cross_validation_scripts/simulation_*` | Tunes XGBoost hyperparameters for each simulation setting.                   |
| `simulation_fitting_and_scoring_scenarios04_cox_xgboost_nn.R` | `Fitting_Scoring/Simulation_scripts`   | Fits and scores Cox, XGBoost, and neural network models for scenarios Alpha to Epsilon. |
| `simulation_fitting_and_scoring_cl_methods_scenarios04.R`   | `Fitting_Scoring/Simulation_scripts`   | Fits and scores the chain ladder benchmarks for scenarios Alpha to Epsilon.  |
| `simulation_fitting_and_scoring_zeta_cox_xgboost_nn.R`      | `Fitting_Scoring/Simulation_scripts`   | Fits and scores Cox, XGBoost, and neural network models for scenario Zeta.   |
| `simulation_zeta_fitting_and_scoring_cl_methods.R`           | `Fitting_Scoring/Simulation_scripts`   | Fits and scores the chain ladder benchmarks for scenario Zeta.               |
| `summarize_simulation_results.R`                            | `Fitting_Scoring`                      | Aggregates scoring outputs and creates manuscript-ready summary tables.      |

## Running on Slurm

The jobs were executed with the Slurm scheduler. To replicate our workflow:

1. **Perform Bayesian hyper-parameter tuning for each dataset in each scenario.** Example `.sh` files for simulation Alpha can be found in `slurm_scripts`. Submit with `sbatch ~/slurm_job_init_cv.sh`. The companion script `slurm_job_cv.sh` shows how to launch the subsequent cross-validation jobs. Both serve as templates for creating similar submission scripts for other simulations.
2. **Fit and score the models.** Example `.sh` files that fit and score chain ladder and ReSurv-based models are provided in `slurm_scripts`. Execute `sbatch ~/simulation_fitting_scoring_slurm.sh` (or adapt `simulation_fitting_scoring.sh` for interactive runs).
3. **Save the final results in a LaTeX-friendly format.** Run `Fitting_Scoring/summarize_simulation_results.R` after the simulations complete.

## R Session Information

We refer to version `1.0.1` of `ReSurv`, which was used to obtain the paper's results. The initial release of `ReSurv` can be downloaded [here](https://github.com/edhofman/ReSurv/releases/tag/v1.0.1).

```r
# System info (private fields redacted)
Sys.info()
#>  sysname        "Linux"
#>  release        "5.14.0"
#>  version        "(kernel info omitted)"
#>  nodename       "(redacted)"
#>  machine        "x86_64"
#>  login          "(redacted)"
#>  user           "(redacted)"
#>  effective_user "(redacted)"

# R version and session info
sessionInfo()
#> R version 4.4.0 (2024-04-24)
#> Platform: x86_64-conda-linux-gnu
#> Running under: Ubuntu 24.04.2 LTS
#>
#> Matrix products: default
#> BLAS/LAPACK: (path omitted)
#>
#> locale:
#>  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C
#>  [3] LC_TIME=C.UTF-8        LC_COLLATE=C.UTF-8
#>  [5] LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8
#>  [7] LC_PAPER=C.UTF-8       LC_MEASUREMENT=C.UTF-8
#>
#> time zone: Europe/Copenhagen
#>
#> attached base packages:
#> [1] stats graphics grDevices utils datasets methods base
#>
#> loaded via a namespace (and not attached):
#> [1] compiler_4.4.0
