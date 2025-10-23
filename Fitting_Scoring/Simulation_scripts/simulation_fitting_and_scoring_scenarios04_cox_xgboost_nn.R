resurv_conda_env <- Sys.getenv("RESURV_CONDA_ENV", unset = NA)
if (!is.na(resurv_conda_env) && nzchar(resurv_conda_env)) {
  reticulate::use_condaenv(resurv_conda_env)
}

get_script_dir <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- cmd_args[grepl("^--file=", cmd_args)]
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg),
                                  winslash = "/",
                                  mustWork = TRUE)))
  }
  getwd()
}

script_dir <- get_script_dir()
project_root <- Sys.getenv(
  "RESURV_PROJECT_ROOT",
  unset = normalizePath(file.path(script_dir, "..", ".."),
                        winslash = "/",
                        mustWork = FALSE)
)
results_dir <- Sys.getenv(
  "RESURV_SCORING_RESULTS",
  unset = file.path(project_root, "Fitting_Scoring", "Scoring_results")
)
if (!dir.exists(results_dir)) {
  dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
}
fitting_results_dir <- Sys.getenv(
  "RESURV_FITTING_RESULTS",
  unset = file.path(project_root, "Fitting_Scoring", "Fitting_results")
)
if (!dir.exists(fitting_results_dir)) {
  dir.create(fitting_results_dir, recursive = TRUE, showWarnings = FALSE)
}
cv_results_dir <- Sys.getenv(
  "RESURV_CV_RESULTS",
  unset = file.path(project_root, "ReSurv_cv_results")
)

torch <- reticulate::import("torch", delay_load = TRUE)
torchtuples <- reticulate::import('torchtuples', delay_load = TRUE)
shap <- reticulate::import('shap', delay_load = TRUE)

library(ReSurv)
library(data.table)
library(dplyr)
require(parallel)

sim_fitting <- function(seed, names_table, location,scenario, model,save=FALSE){

  input_data <- data_generator(random_seed = seed,
                               scenario=scenario,
                               time_unit =  1 / 360,
                               years = 4,
                               period_exposure  = 200)


  categorical_features = c("claim_type")
  continuous_features = c("AP")

  individual_data <- IndividualDataPP(
    input_data,
    categorical_features = categorical_features,
    continuous_features = continuous_features,
    accident_period = "AP",
    calendar_period = "RP",
    input_time_granularity = "days",
    output_time_granularity = "quarters",
    years = 4
  )

  individual_data_y <- IndividualDataPP(
    input_data,
    categorical_features = categorical_features,
    continuous_features = continuous_features,
    accident_period = "AP",
    calendar_period = "RP",
    input_time_granularity = "days",
    output_time_granularity = "years",
    years = 4
  )



scenario_num=which(c('alpha','beta','gamma','delta','epsilon','zeta','eta')==scenario)-1


  file_name <-names_table[
    names_table$sim==scenario_num &
      names_table$seed == seed &
      names_table$model==model,
  ]$org

  if(!(model %in% c("COX", "ltrc"))){
    hparameters <- fread(paste0(location,"/",file_name)) %>%
      filter(Score==max(Score,na.rm=T))
    hparameters<-hparameters[1,]
  }


  if(model=="xgboost"){
    start<-Sys.time()
    resurv.fit <- ReSurv(individual_data,
                         hazard_model = "XGB",
                         hparameters = list(params=list(booster="gbtree",
                                                        eta=hparameters$eta,
                                                        subsample=hparameters$subsample,
                                                        alpha=hparameters$alpha,
                                                        lambda=hparameters$lambda,
                                                        min_child_weight=hparameters$min_child_weight,
                                                        max_depth = hparameters$max_depth),
                                            print_every_n = 0,
                                            nrounds=3000,
                                            verbose=F,
                                            early_stopping_rounds = 500),
                         grouping_method="probability",
                         simplifier=TRUE)


    resurv.fit.predict <- predict(resurv.fit,
                                  grouping_method = "probability")

    time <- Sys.time() - start
  }
  if(model=="COX"){
    if(scenario %in% c(2,3,4)){
      cfs <- "AP_i"
    }else{
      cfs <- NULL
    }



    start<-Sys.time()
    resurv.fit <- ReSurv(individual_data,
                         hazard_model = "COX",
                         grouping_method="probability",
                         simplifier=TRUE)

    resurv.fit.predict <- predict(resurv.fit)

    time <- Sys.time() - start

  }


  if(model=="deepsurv"){
    optim = switch(hparameters$optim, "Adam", "SGD")
    activation = switch(hparameters$activation, "LeakyReLU","SELU")
    batch_size=as.integer(5000)
    num_layers=as.integer(hparameters$num_layers)
    num_nodes=as.integer(hparameters$num_nodes)
    lr =hparameters$lr
    eps =hparameters$eps
    xi =hparameters$xi

    start<-Sys.time()
    resurv.fit <- ReSurv(individual_data,
                         hazard_model = "NN",
                         hparameters = list(num_layers=num_layers,
                                            early_stopping = TRUE,
                                            patience = 350,
                                            verbose=F,
                                            network_structure = NULL,
                                            num_nodes = num_nodes,
                                            activation = activation,
                                            optim= optim,
                                            lr = lr,
                                            xi=xi,
                                            epsilon = eps,
                                            batch_size=as.integer(5000),
                                            epochs=as.integer(5500),
                                            num_workers=0,
                                            tie='Efron'),
                         grouping_method="probability",
                         simplifier=TRUE)


    resurv.fit.predict <- predict(resurv.fit,
                                  grouping_method = "probability")

    time <- Sys.time() - start
  }


  resurv.fit.predict$fitting_time <- time

  if(save){
    name <- file.path(
      fitting_results_dir,
      sprintf("sim%s_%s_%s_%s.RData",
              scenario,
              seed,
              model,
              format(Sys.time(), "%Y_%m_%d_%H_%M"))
    )
    saveRDS(resurv.fit.predict, file=name)}


  # Scoring ----
  conversion_factor <- individual_data$conversion_factor

  max_dp_i <- 1440

  true_output <- resurv.fit$IndividualDataPP$full.data %>%
    mutate(
      DP_rev_o = floor(max_dp_i * conversion_factor) -
        ceiling(
          DP_i * conversion_factor +
            ((AP_i - 1) %% (
              1 / conversion_factor
            )) * conversion_factor) + 1,
      AP_o = ceiling(AP_i * conversion_factor),
      TR_o = AP_o - 1
    ) %>%
    filter(DP_rev_o <= TR_o) %>%
    group_by(claim_type, AP_o, DP_rev_o) %>%
    mutate(claim_type = as.character(claim_type)) %>%
    summarize(I = sum(I), .groups = "drop") %>%
    filter(DP_rev_o > 0)

  out_list <- resurv.fit.predict$long_triangle_format_out
  out <- out_list$output_granularity %>%
    mutate(DP_rev_o = 16 - DP_o + 1)

  #Total output
  score_total <- out[, c(categorical_features, setdiff(continuous_features,"AP"), "AP_o", "DP_o", "expected_counts")] %>%
    mutate(DP_rev_o = 16 - DP_o + 1) %>%
    inner_join(true_output, by = c(categorical_features, setdiff(continuous_features,"AP"), "AP_o", "DP_rev_o")) %>%
    mutate(ave = I - expected_counts, abs_ave = abs(ave)) %>%
    # from here it is reformulated for the are tot
    ungroup() %>%
    group_by(AP_o, DP_rev_o) %>%
    reframe(abs_ave = abs(sum(ave)), I = sum(I))

  are_tot <- sum(score_total$abs_ave) / sum(score_total$I)
  ei_r <- sum(resurv.fit.predict$predicted_counts) / sum(score_total$I)


  # dfs_output <- out %>%
  #   mutate(DP_rev_o = 16 - DP_o + 1) %>%
  #   select(AP_o, claim_type, DP_rev_o, f_o) %>%
  #   mutate(DP_rev_o = DP_rev_o) %>%
  #   distinct()

  #Cashflow on output scale.Etc quarterly cashflow development
  # are cal quarterly ----

  score_total_diagonal<- out%>%
    group_by(claim_type,AP_o,DP_rev_o) %>%
    # mutate(DP_rev_o = 16 - DP_o + 1) %>%
    inner_join(true_output, by =c("claim_type", "AP_o", "DP_rev_o")) %>%
    ungroup()%>%
    mutate(RP_o=DP_o+AP_o-1) %>%
    filter(RP_o>16) %>%
    group_by(RP_o)%>%
    reframe(I=sum(I),
            expected_counts=sum(expected_counts)) %>%
    mutate(absolute_difference=abs(I-expected_counts))

  are_cal_q=sum(score_total_diagonal$absolute_difference)/sum(score_total_diagonal$I)

  # score_diagonal <- resurv.fit$IndividualDataPP$full.data  %>%
  #   mutate(
  #     DP_rev_o = floor(max_dp_i * conversion_factor) -
  #       ceiling(
  #         DP_i * conversion_factor +
  #           ((AP_i - 1) %% (
  #             1 / conversion_factor
  #           )) * conversion_factor) + 1,
  #     AP_o = ceiling(AP_i * conversion_factor)
  #   ) %>%
  #   group_by(claim_type, AP_o, DP_rev_o) %>%
  #   mutate(claim_type = as.character(claim_type)) %>%
  #   summarize(I = sum(I), .groups = "drop") %>%
  #   group_by(claim_type, AP_o) %>%
  #   arrange(desc(DP_rev_o)) %>%
  #   mutate(I_cum = cumsum(I)) %>%
  #   mutate(I_cum_lag = lag(I_cum, default = 0)) %>%
  #   left_join(dfs_output, by = c("AP_o", "claim_type", "DP_rev_o")) %>%
  #   mutate(I_cum_hat =  I_cum_lag * f_o,
  #          RP_o = max(DP_rev_o) - DP_rev_o + AP_o) %>%
  #   inner_join(true_output[, c("AP_o", "DP_rev_o")] %>%  distinct()
  #              , by = c("AP_o", "DP_rev_o")) %>%
  #   group_by(AP_o, DP_rev_o) %>%
  #   reframe(abs_ave2_diag = abs(sum(I_cum_hat) - sum(I_cum)), I = sum(I))
  #
  # are_cal_q <- sum(score_diagonal$abs_ave2_diag) / sum(score_diagonal$I)



  # are cal yearly ----

  # conversion_factor <- individual_data_y$conversion_factor

  max_dp_i <- 1440

  # true_output <- individual_data_y$full.data %>%
  #   mutate(
  #     DP_rev_o = floor(max_dp_i * conversion_factor) -
  #       ceiling(
  #         DP_i * conversion_factor +
  #           ((AP_i - 1) %% (
  #             1 / conversion_factor
  #           )) * conversion_factor) + 1,
  #     AP_o = ceiling(AP_i * conversion_factor),
  #     TR_o = AP_o - 1
  #   ) %>%
  #   filter(DP_rev_o <= TR_o) %>%
  #   group_by(claim_type, AP_o, DP_rev_o) %>%
  #   mutate(claim_type = as.character(claim_type)) %>%
  #   summarize(I = sum(I), .groups = "drop") %>%
  #   filter(DP_rev_o > 0)

  conversion_factor <- individual_data_y$conversion_factor

  true_output <-individual_data_y$full.data %>%
    mutate(
      DP_rev_o = floor(max_dp_i*conversion_factor)-ceiling(DP_i*conversion_factor+((AP_i-1)%%(1/conversion_factor))*conversion_factor) +1,
      AP_o = ceiling(AP_i*conversion_factor),
      TR_o= AP_o-1
    ) %>%
    filter(DP_rev_o <=TR_o) %>%
    group_by(claim_type,  AP_o, DP_rev_o) %>%
    mutate(claim_type = as.character(claim_type)
    ) %>%
    summarize(I=sum(I), .groups = "drop") %>%
    filter(DP_rev_o>0)


  resurv.fit.predict.y <- predict(resurv.fit,
                                  newdata = individual_data_y,
                                  grouping_method = "probability")

  out_list <- resurv.fit.predict.y$long_triangle_format_out
  out <- out_list$output_granularity


  setDT(out)

  out<-out[,DP_rev_o := 4 - DP_o + 1]

  score_total_diagonal<- out%>%
    group_by(claim_type,AP_o,DP_rev_o) %>%
    # mutate(DP_rev_o = 16 - DP_o + 1) %>%
    inner_join(true_output, by =c("claim_type", "AP_o", "DP_rev_o")) %>%
    ungroup()%>%
    mutate(RP_o=DP_o+AP_o-1) %>%
    filter(RP_o>4) %>%
    group_by(RP_o)%>%
    reframe(I=sum(I),
            expected_counts=sum(expected_counts)) %>%
    mutate(absolute_difference=abs(I-expected_counts))


  are_cal_y=sum(score_total_diagonal$absolute_difference)/sum(score_total_diagonal$I)



#
#   score_diagonal <- resurv.fit$IndividualDataPP$full.data  %>%
#     mutate(
#       DP_rev_o = floor(max_dp_i * conversion_factor) -
#         ceiling(
#           DP_i * conversion_factor +
#             ((AP_i - 1) %% (
#               1 / conversion_factor
#             )) * conversion_factor) + 1,
#       AP_o = ceiling(AP_i * conversion_factor)
#     ) %>%
#     group_by(claim_type, AP_o, DP_rev_o) %>%
#     mutate(claim_type = as.character(claim_type)) %>%
#     summarize(I = sum(I), .groups = "drop") %>%
#     group_by(claim_type, AP_o) %>%
#     arrange(desc(DP_rev_o)) %>%
#     mutate(I_cum = cumsum(I)) %>%
#     mutate(I_cum_lag = lag(I_cum, default = 0)) %>%
#     left_join(dfs_output, by = c("AP_o", "claim_type", "DP_rev_o")) %>%
#     mutate(I_cum_hat =  I_cum_lag * f_o,
#            RP_o = max(DP_rev_o) - DP_rev_o + AP_o) %>%
#     inner_join(true_output[, c("AP_o", "DP_rev_o")] %>%  distinct()
#                , by = c("AP_o", "DP_rev_o")) %>%
#     group_by(AP_o, DP_rev_o) %>%
#     reframe(abs_ave2_diag = abs(sum(I_cum_hat) - sum(I_cum)), I = sum(I))
#
#   are_cal_y <- sum(score_diagonal$abs_ave2_diag) / sum(score_diagonal$I)



crps <- survival_crps(resurv.fit)
m_crps <- mean(crps$crps)



  name <- file.path(
    results_dir,
    sprintf(
      "sim_in_chunks_%s_seed_%s_%s.csv",
      scenario,
      seed,
      format(Sys.time(), "%Y_%m_%d_%H:%M")
    )
  )


  fwrite(data.table(are_tot=are_tot,
             are_cal_q=are_cal_q,
             are_cal_y=are_cal_y,
             ei_r=ei_r,
             m_crps=m_crps,
             model=model,
             scenario=scenario,
             seed=seed),
         name)

}


library(data.table)


set.seed(1964)
seeds<-1:5
args = commandArgs(trailingOnly=TRUE)
print(args)
location <- cv_results_dir

names <- list.files(location)
names_relevant <- names[substr(names,1,4) %in% c("sim0","sim1","sim2","sim3","sim4","sim5")]
names_table <- tibble(org=names_relevant) %>%
  rowwise() %>%
  mutate( sim = substr(org,
                       4,
                       4),
          seed=substr(org,
                      unlist(gregexpr('_', org))[1] +1,
                      unlist(gregexpr('_', org))[2]-1 ),
          model = substr(org,
                         unlist(gregexpr('_', org))[2] +1,
                         unlist(gregexpr('_', org))[3]-1),
          date = as.POSIXct(substr(org,
                                   unlist(gregexpr('_', org))[4] +1,
                                   unlist(gregexpr('\\.', org))[1]-1),
                            format = "%Y_%m_%d_%H_%M")
  )%>%
  ungroup() %>%
  group_by(sim,seed, model) %>%
  filter(date==max(date)) %>%
  filter(model==args[1])

cl <- makeCluster(2)
random_seed=1964
objects_export <- list(
  "random_seed",
  "sim_fitting",
  "shap",
  "torchtuples",
  "torch"
)
clusterExport(cl, objects_export, envir = environment())

clusterEvalQ(cl, {library("fastDummies")
  library("reticulate")
  library("ReSurv")
  library("dplyr")
  library("data.table")
  set.seed(random_seed)} )


parallel::parSapply(cl,
                    seeds, FUN=sim_fitting,
                    names_table = names_table,
                    location=location,
                    model=args[1],
                   scenario=args[2])



