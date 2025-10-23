library(ParBayesianOptimization)
library(ReSurv)
library(doParallel)
library(data.table)

obj_func <- function(
    eta,
    max_depth,
    min_child_weight,
    subsample,
    lambda,
    alpha) {

  xgbcv <- ReSurvCV(
    IndividualDataPP = individual_data,
    model = "XGB",
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
    Score = -xgbcv$out.cv.best.oos$test.lkh,
    train.lkh = xgbcv$out.cv.best.oos$train.lkh
  )

  return(lst)
}

bounds <- list(
  eta = c(0, 1),
  max_depth = c(1L, 25L),
  min_child_weight = c(0, 50),
  subsample = c(0.51, 1),
  lambda = c(0, 15),
  alpha = c(0, 15)
)
set.seed(1964)
seeds <- sample.int(20)

cl <- makeCluster(50)
registerDoParallel(cl)


clusterEvalQ(cl, {
  library("ReSurv")
})


for (i in seeds) {

  input_data <- data_generator(
    random_seed = i,
    scenario = 5,
    time_unit = 1 / 360,
    years = 4,
    period_exposure = 200
  )

  individual_data <- IndividualDataPP(
    input_data,
    categorical_features = c("business_use"),
    continuous_features = c("property_value", "AP", "age"),
    accident_period = "AP",
    calendar_period = "RP",
    input_time_granularity = "days",
    output_time_granularity = "years",
    years = 4
  )

  objects_export <- list(
    "individual_data"
  )
  clusterExport(cl, objects_export, envir = environment())


  bayes_out <- bayesOpt(
    FUN = obj_func,
    bounds = bounds,
    initPoints = length(bounds) + 20,
    iters.n = 10,
    iters.k = 2,
    otherHalting = list(timeLimit = 18000),
    parallel = TRUE
  )


  name = paste0(
    "~/modi_mount/ReSurv_cv_results",
    "/sim5_",
    i,
    "_xgboost_cv_",
    format(Sys.time(), "%Y_%m_%d_%H:%M"),
    ".csv"
  )

  write.csv(bayes_out$scoreSummary, file = name)
}
parallel::stopCluster(cl)
