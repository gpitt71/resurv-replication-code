[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10419198.svg)](https://doi.org/10.5281/zenodo.10419198)

# resurv-replication-code

This repository contains the code to replicate the simulated case study of the manuscript A MACHINE LEARNING APPROACH BASED ON SURVIVAL ANALYSIS FOR IBNR FREQUENCIES IN NON-LIFE RESERVING.

The computations for obtaining the results in the manuscript were performed on the ERDA cloud, see Appendix `Computational Details` of the manuscript. 

We will not share the private data on which we performed the real data case study.

This repository has the same structure as the repository that we used to obtain the results in the manuscript.

```
| resurv-replication-code
   |_ ReSurv_cv_results
   |_ cross_validation_scripts 
      |_ simulation_0
         |_ bayes_deepsurv.R 
         |_ bayes_xgboost.R 
      |_ simulation_1
         |_ bayes_deepsurv.R 
         |_ bayes_xgboost.R 
      |_ simulation_2
         |_ bayes_deepsurv.R 
         |_ bayes_xgboost.R 
      |_ simulation_3
         |_ bayes_deepsurv.R 
         |_ bayes_xgboost.R 
      |_ simulation_4
         |_ bayes_deepsurv.R 
         |_ bayes_xgboost.R 
   |_ Scoring
      |_ Fitting_results
      |_ latex_tables
      |_ Scoring_results
      |_ Scoring_datasets
      |_ Simulation_scripts
         |_ simulation_cl_scoring1.R
         |_ simulation_cl_scoring2.R
         |_ simulation_fitting.R
         |_ simulation_scoring.R
         |_ latex_tables.R
   |_ slurm_scripts
      |_ slurm_job_init_cv.sh
      |_ slurm_job_cv.sh
      |_ simulation_fitting_scoring.sh
      |_ simulation_fitting_scoring_slurm.sh
   |_ heatmap_data

      
```

We describe the folders below:

|      Folder             |           Description                                                      |
| :---------------------: |:--------------------------------------------------------------------------:|
| `ReSurv_cv_results`     | Folder that contains the cross-validation output.                          |
| `Fitting_results`       | Folder that containts fitted models for each data set in each scenario.    |
| `Scoring_results`       | Folder that containts temporary files for scoring.                         |
| `Scoring_datasets`      | Folder that containts temporary files for scoring.                         |
| `Simulation_scripts`    | Folder that containts scripts for scoring chain ladder and ReSurv models.  |
| `slurm_scripts`         | Folder that containts scripts for slurm.                                   |
| `latex_tables`          | Folder that containts the paper's output.                                  |
| `heatmap_data`          | Folder that containts the data to plot the heatmap.                        |

We describe the scripts below:

|      Script                |    Description                                                  |
| :------------------------: |:---------------------------------------------------------------:|
| `simulation_cl_scoring1.R` | Script to fit and score CL.                                     |
| `simulation_cl_scoring2.R` | Script to create CL scoring results comparable to other models. |
| `simulation_fitting.R`     | Script to fit the models.                                       |
| `simulation_scoring.R`     | Script to score the models.                                     |
| `bayes_deepsurv.R`         | Script to tune NN hyperparameters.                              |
| `bayes_xgboost.R`          | Script to tune XGB hyperparameters.                             |
| `plot_heat_map.R`          | Script to plot the heatmap or $RE^{k,j}$                        |


The jobs were executed with the job scheduler Slurm. In order to replicate our results the user should:

1. **Perform bayesian hyperparameters tuning for each dataset in each scenario**. Example `.sh` files for tuning the hyperparameters of simulation Alpha can be found in the folder `slurm_scripts`. Execute `sbatch ~ /slurm_job_init_cv.sh`. 

2. **Fit and score the models**. An example `.sh` file to fit and score CL and our models can also be found in the folder `slurm_scripts`. Execute `sbatch ~ /simulation_fitting_scoring_slurm.sh`.

3. **Save the final results in a Latex friendly format**. This can be done using the `.R` file `latex_tables.R`


Details on the R session, printed with the function `SessionInfo()`. 
We refer to the version `0.0.1` of `ReSurv` that we used for the obtaining the paper's results. The initial release of `ReSurv` can be obtained at [this link.](https://github.com/edhofman/ReSurv/releases/tag/ReSurv_initial_release).

```
R version 4.2.3 (2023-03-15)
Platform: x86_64-conda-linux-gnu (64-bit)
Running under: Ubuntu 22.04.3 LTS

Matrix products: default
BLAS/LAPACK: <path to the BLAS library within the active conda environment>

locale:
 [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
 [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
 [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
 [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
 [9] LC_ADDRESS=C               LC_TELEPHONE=C            
[11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       

attached base packages:
[1] splines   stats     graphics  grDevices utils     datasets  methods  
[8] base     

other attached packages:
 [1] lubridate_1.9.3   forcats_1.0.0     stringr_1.5.1     readr_2.1.4      
 [5] tidyr_1.3.0       tibble_3.2.1      ggplot2_3.4.4     tidyverse_2.0.0  
 [9] dplyr_1.1.4       dtplyr_1.3.1      fastDummies_1.7.3 forecast_8.21.1  
[13] reshape_0.8.9     purrr_1.0.2       reshape2_1.4.4    bshazard_1.1     
[17] Epi_2.47.1        survival_3.5-7    SynthETIC_1.0.5   rpart_4.1.21     
[21] data.table_1.14.8

loaded via a namespace (and not attached):
 [1] Rcpp_1.0.11         lattice_0.22-5      zoo_1.8-12         
 [4] lmtest_0.9-40       utf8_1.2.4          R6_2.5.1           
 [7] plyr_1.8.9          etm_1.1.1           pillar_1.9.0       
[10] rlang_1.1.2         curl_5.0.1          fracdiff_1.5-2     
[13] TTR_0.24.4          Matrix_1.6-4        munsell_0.5.0      
[16] compiler_4.2.3      numDeriv_2016.8-1.1 pkgconfig_2.0.3    
[19] urca_1.3-3          mgcv_1.9-0          nnet_7.3-19        
[22] tidyselect_1.2.0    quadprog_1.5-8      fansi_1.0.5        
[25] tzdb_0.4.0          withr_2.5.2         MASS_7.3-60        
[28] grid_4.2.3          nlme_3.1-164        gtable_0.3.4       
[31] lifecycle_1.0.4     magrittr_2.0.3      scales_1.3.0       
[34] quantmod_0.4.25     cli_3.6.1           stringi_1.7.12     
[37] tseries_0.10-54     timeDate_4022.108   xts_0.13.1         
[40] generics_0.1.3      vctrs_0.6.5         tools_4.2.3        
[43] cmprsk_2.2-11       glue_1.6.2          hms_1.1.3          
[46] parallel_4.2.3      timechange_0.2.0    colorspace_2.1-0 
```

