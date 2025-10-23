library(ParBayesianOptimization)
library(ReSurv)
library(data.table)
library(doParallel)

resurv_conda_env <- Sys.getenv("RESURV_CONDA_ENV", unset = NA)

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

cv_results_dir <- Sys.getenv(
  "RESURV_CV_RESULTS",
  unset = file.path(project_root, "ReSurv_cv_results")
)
if (!dir.exists(cv_results_dir)) {
  dir.create(cv_results_dir, recursive = TRUE, showWarnings = FALSE)
}

cox_loss_objective2 <- function(preds,dtrain){
  
  Ti <- attr(dtrain, 'truncation')
  Ei <- attr(dtrain, 'claim_arrival')
  
  risk_sets <- attr(dtrain, 'risk_sets')
  event_sets <- attr(dtrain, 'event_sets')
  
  efron_c<-attr(dtrain, 'efron_c')
  tieid<- attr(dtrain, 'tieid')
  # obs_tie<- attr(dtrain, 'groups')
  
  exp_p_sum <- sapply(risk_sets,
                      FUN=exp_sum_computer,
                      ypred=preds)
  
  exp_p_tie <- sapply(event_sets,
                      FUN=exp_sum_computer,
                      ypred=preds)
  
  tmp1 <- data.table(risks_s = rep(exp_p_sum, tieid),
                     events_s = rep(exp_p_tie, tieid),
                     efron_c=efron_c,
                     ties = Ei)
  tmp_alpha_i=tmp1[, .(alpha_i = sum(1/(risks_s-efron_c*events_s))), by = ties]$alpha_i
  alpha_i = vector("numeric",length = max(Ei))
  alpha_i[unique(Ei)] = tmp_alpha_i
  
  tmp_beta_i=tmp1[, .(beta_i = sum(efron_c/(risks_s-efron_c*events_s))), by = ties]$beta_i
  beta_i = vector("numeric",length = max(Ei))
  beta_i[unique(Ei)] = tmp_beta_i
  # browser()
  # alpha_i <- 1/(exp_p_sum-efron_c*exp_p_tie)
  # beta_i <- efron_c/(tmp1$risks_s-efron_c*tmp1$events_s)
  
  tmp_gamma_i=tmp1[, .(gamma_i = sum(1/(risks_s-efron_c*events_s)^2)), by = ties]$gamma_i
  gamma_i = vector("numeric",length = max(Ei))
  gamma_i[unique(Ei)] = tmp_gamma_i
  
  tmp_omega_i=tmp1[, .(omega_i = sum((1-(1-efron_c)^2)/(risks_s-efron_c*events_s)^2)), by = ties]$omega_i
  omega_i = vector("numeric",length = max(Ei))
  omega_i[unique(Ei)] = tmp_omega_i
  # gamma_i <- (1/(tmp1$risks_s-efron_c*tmp1$events_s))^2
  # omega_i <- (1-(1-efron_c)^2)/((tmp1$risks_s-efron_c*tmp1$events_s)^2)
  
  
  # beta_i=rep(beta_i,tieid)
  # omega_i=rep(omega_i,tieid)
  
  exp_p <- exp(preds)
  n <- length(exp_p)
  J <- length(alpha_i)
  alpha_i_lt = vector("numeric",n)
  gamma_i_lt = vector("numeric",n)
  beta_i_lt = vector("numeric",n)
  omega_i_lt = vector("numeric",n)
  
  for(i in 1:n){
    
    alpha_i_lt[i]= sum(alpha_i[(Ti[i]+1):Ei[i]])
    beta_i_lt[i] = beta_i[Ei[i]]
    gamma_i_lt[i]= sum(gamma_i[(Ti[i]+1):Ei[i]])
    omega_i_lt[i] = omega_i[Ei[i]]
  }
  
  # browser()
  #we consider the nll
  grad <- exp_p*(alpha_i_lt-beta_i_lt)-1
  
  hess <- grad-(exp_p^2)*(gamma_i_lt-omega_i_lt)+1
  # browser()
  return(list(grad=grad,hess=hess))
}


cox_evaluation_metrix <- function(preds,
                                  dtrain){
  
  risk_sets <- attr(dtrain, 'risk_sets')
  event_sets <- attr(dtrain, 'event_sets')
  efron_c<- attr(dtrain, 'efron_c')
  tieid<- attr(dtrain, 'tieid')
  
  exp_p_sum <- rep(sapply(risk_sets,FUN=exp_sum_computer, ypred=preds),tieid)
  exp_p_tie <- rep(sapply(event_sets,FUN=exp_sum_computer, ypred=preds),tieid)
  exp_p <- exp(preds)
  
  r_k <- exp_p_sum-efron_c*exp_p_tie
  
  lkh<-(exp_p/r_k)
  
  value <- -sum(log(lkh))
  # browser()
  return(list(metric = "log-partial likelihood", value = value/length(preds) ))
}


# Function must take the hyper-parameters as inputs
obj_func <- function(eta, max_depth, min_child_weight, subsample, lambda, alpha) {
  
  # param <- list(
  #
  #   # Hyter parameters
  #   eta = eta,
  #   max_depth = max_depth,
  #   min_child_weight = min_child_weight,
  #   subsample = subsample,
  #   lambda = lambda,
  #   alpha = alpha,
  #
  #   # Tree model
  #   booster = "gbtree",
  #
  #   # Regression problem
  #   objective = cox_loss_objective2,
  #
  #   # Use the Mean Absolute Percentage Error
  #   eval_metric = cox_evaluation_metrix)
  
  xgbcv <- ReSurvCV(
    individual_data,
    model = "XGBoost",
    hparameters_grid = list(
      booster = "gbtree",
      eta = eta,
      max_depth = max_depth,
      subsample = subsample,
      alpha = lambda,
      lambda = alpha,
      min_child_weight = min_child_weight
    ),
    print_every_n = 1L,
    nrounds = 500,
    verbose = FALSE,
    verbose.cv = TRUE,
    early_stopping_rounds = 30,
    folds = 3,
    parallel = FALSE,
    random_seed = as.integer(Sys.time())
  )
  
  lst <- list(
    
    # First argument must be named as "Score"
    # Function finds maxima so inverting the output
    Score = -xgbcv$out.cv.best.oos$test.lkh,
    
    # Get number of trees for the best performing model
    train.lkh = xgbcv$out.cv.best.oos$train.lkh
  )
  
  return(lst)
}


bounds <- list(eta = c(0, 1),
               max_depth = c(1L, 25L),
               min_child_weight = c(0, 50),
               subsample = c(0.51, 1),
               lambda = c(0, 15),
               alpha = c(0, 15))

set.seed(1964)
seeds <- sample.int(20)

cl <- makeCluster(50)
registerDoParallel(cl)


clusterEvalQ(cl, {library("ReSurv"); library("data.table")} )


for (i in seeds){


    input_data <- data_generator(random_seed = i,
                              scenario=0,
                              time_unit = 1/360,
                              years = 4,
                              period_exposure = 200)

  individual_data <- IndividualDataPP(input_data,
                                    id=NULL,
                                    categorical_features = c("claim_type"),
                                    continuous_features = "AP_i",
                                    accident_period="AP",
                                    calendar_period="RP",
                                    input_time_granularity = "days",
                                    output_time_granularity = "quarters",
                                    years=4 #check this. I see strange development periods
)

objects_export <- list(
  "individual_data"
)
clusterExport(cl, objects_export, envir = environment())


bayes_out <- bayesOpt(
  FUN = obj_func
  , bounds = bounds
  , initPoints = length(bounds) + 20
  , iters.n = 1000
  , iters.k = 50
  , otherHalting = list(timeLimit = 18000)
  , parallel = TRUE
)


  name <- file.path(
    cv_results_dir,
    sprintf("sim0_%s_xgboost_cv_%s.csv", i, format(Sys.time(), "%Y_%m_%d_%H:%M"))
  )

write.csv(bayes_out$scoreSummary, file = name)
}
parallel::stopCluster(cl)