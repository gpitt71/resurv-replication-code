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

# Function must take the hyper-parameters as inputs
obj_func <- function(num_layers, num_nodes, optim, activation,lr, xi, eps ) {
  
  optim_name <- switch(as.integer(optim), "Adam", "SGD")
  activation_name <- switch(as.integer(activation), "LeakyReLU", "SELU")
  batch_size <- as.integer(5000)
  num_layers_int <- as.integer(num_layers)
  num_nodes_int <- as.integer(num_nodes)

  deepsurv_cv <- ReSurvCV(
    individual_data,
    model = "NN",
    hparameters_grid = list(
      num_layers = num_layers_int,
      num_nodes = num_nodes_int,
      optim = optim_name,
      activation = activation_name,
      lr = lr,
      xi = xi,
      eps = eps,
      tie = "Efron",
      batch_size = batch_size,
      early_stopping = TRUE,
      patience = 50
    ),
    epochs = as.integer(300),
    num_workers = as.integer(2),
    verbose = FALSE,
    verbose.cv = TRUE,
    folds = 3,
    parallel = FALSE,
    random_seed = as.integer(Sys.time())
  )
  

  lst <- list(
    
    # First argument must be named as "Score"
    # Function finds maxima so inverting the output
    Score = -deepsurv_cv$out.cv.best.oos$test.lkh,
    
    # Get number of trees for the best performing model
    train.lkh = deepsurv_cv$out.cv.best.oos$train.lkh
  )
  
  return(lst)
}


bounds <- list(num_layers = c(2L,10L),
               num_nodes = c(2L,10L),
               optim=c(1L,2L),
               activation = c(1L,2L),
               lr=c(.005,0.5),
               xi=c(0,0.5),
               eps = c(0,0.5)
               )

set.seed(1964)
seeds <- sample.int(20)
#seeds <- seeds[11:20]

cl <- makeCluster(32)
registerDoParallel(cl)


clusterEvalQ(cl, {library("ReSurv"); library("data.table")} )


for (i in seeds){
  
  
    input_data <- data_generator(random_seed = i,
                              scenario=4,
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
    , initPoints = 32
    , iters.n = 1000
    , iters.k = 32
    , otherHalting = list(timeLimit = 18000)
    , parallel = TRUE
  )
  
  
  name <- file.path(
    cv_results_dir,
    sprintf("sim4_%s_deepsurv_cv_%s.csv", i, format(Sys.time(), "%Y_%m_%d_%H:%M"))
  )
  
  write.csv(bayes_out$scoreSummary, file = name)
}
parallel::stopCluster(cl)