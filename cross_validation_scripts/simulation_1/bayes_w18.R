library(ParBayesianOptimization)
library(ReSurv)
library(dplyr)
library(tidyverse)
library(data.table)
library(doParallel)
library(fastDummies)
library(reticulate)
library(keras)

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

# Preprocess covariates
## utils
MinMaxScaler <- function(x, na.rm = TRUE) {
  "MinMax Scaler"
  return(2*(x- min(x)) /(max(x)-min(x))-1)
}
## Aggregate in triangles

data_wrapper <- function(data,
                         categorical_features=NULL,
                         continuous_features=NULL,
                         development_period,
                         accident_period){

  out <- data[,.(dt_count = .N), by = c(categorical_features,
                                        continuous_features,
                                        development_period,
                                        accident_period)]

  return(out)


}


## actual preprocessing
mw18_datapp_x <- function(data,
                          continuous_features=NULL,
                          categorical_features=NULL) {

  # browser()

  out <- data

  if (!is.null(categorical_features)) {
    # out_cat=apply(eval(parse(text=paste0("data[,.(",categorical_features,")]"))),2,as.factor)

    out[, (categorical_features) := lapply(.SD, as.factor), .SDcols = categorical_features]

    # out_cat=apply(eval(parse(text=paste0("data[,.(",categorical_features,")]"))),2,as.factor)

  }

  if (!is.null(continuous_features)) {
    # out_cont=apply(eval(parse(text=paste0("data[,.(",continuous_features,")]"))),2,MinMaxScaler)

    out[, (continuous_features) := lapply(.SD, MinMaxScaler), .SDcols = continuous_features]
  }

  to_keep <- c(categorical_features,
               continuous_features)

  return(out[,..to_keep])

}

# Preprocess targets
mw18_datapp_y <- function(data,
                          development_time,
                          dt_counts,
                          max_dp = 40){

  out<-  pivot_wider(data[order(DP),],
                     names_from = development_time,
                     values_from = dt_counts,
                     names_prefix = "DP")

  setDT(out)

  out[is.na(out)] <- 0

  dp_columns = paste0("DP",1:max_dp)

  return(out)

}

nn_model_cv <- function(data,
                        number_of_neurons,
                        number_of_layers,
                        categorical_features,
                        max_dp,
                        continuous_features) {


    if (!is.na(resurv_conda_env) && nzchar(resurv_conda_env)) {
      reticulate::use_condaenv(resurv_conda_env)
    }
  y_data_columns <- paste0("DP", 1:max_dp)
  x_cat <- fastDummies::dummy_cols(data[,..categorical_features],
                      select_columns=categorical_features,
                      remove_first_dummy=F)[,-1]


  x_data <- cbind(x_cat,
                  data[,..continuous_features])

  y_data <- data[,..y_data_columns]

  loss<-val_loss <-NULL


  sequence_trials <- sample(1:(max_dp-1), 10, replace = FALSE)

  for(j in sequence_trials){

    train_set <- (data[['AP']] < (max_dp - j + 1))

    tmp <- y_data[train_set,][[paste0("DP", j+1)]]

    tmp_1 <- y_data[train_set,][[paste0("DP", j)]]

    cond_positive_denominator <- tmp_1 >0

    if (sum(cond_positive_denominator) > 0) {
      ytgt = tmp / sqrt(tmp_1)
      wtrain = sqrt(tmp_1)

      features <- layer_input(shape = dim(x_data)[2], name = "main_model_il")
      net <- features

      if (number_of_layers > 0) {
        net <- net %>%
          layer_dense(
            units = number_of_neurons,
              use_bias = FALSE,
            activation = 'tanh',
            name = "main_model_hl"
          )
      }

      if (number_of_layers > 1) {
        net <- net %>%
          layer_dense(
            units = number_of_neurons,
            activation = 'tanh',
            use_bias = FALSE,
            name = "main_model_hl_2"
          )
      }

      net <- net %>%
        layer_dense (units = 1,
                     activation = k_exp,
                     name = "main_model_ol")


      volumes <- layer_input(shape =c(1),
                           name = "weights_il")
    
    offset <- volumes %>%
      layer_dense( units = 1, activation = 'linear', 
                   use_bias =FALSE , 
                   trainable =FALSE ,
                   kernel_initializer = initializer_ones(),
                   name = "weights_ol")

      merged <- list(net, offset) %>%
        layer_multiply()
     model <- keras_model(inputs = list (features , 
                                         volumes), outputs = merged)
     #model %>% compile (loss = 'mse', 
     #                   optimizer = 'rmsprop')

     model$compile(loss = "mse", optimizer = "rmsprop")
     
     dat.X <- as.matrix.data.frame(x_data[train_set,][cond_positive_denominator,])
     dat.W <- as.matrix(wtrain[cond_positive_denominator])
     dat.Y <- as.matrix(ytgt[cond_positive_denominator])


my_batch_size = eval(parse(text=(paste0(as.integer(max(floor(length(ytgt)/10),2)),"L"))))
     
     if (j == (max_dp - 1)) {
       my_val_split = 0L
     } else{
       my_val_split = 0.2
     }
     
      
      model_fit <- model$fit(
       list(dat.X, dat.W),
       dat.Y,
       epochs = 80L,
       verbose = 0,
       batch_size = my_batch_size,
       validation_split = my_val_split
     )


      loss <- c(loss,last(model_fit$history$loss))
      val_loss <- c(val_loss,last(model_fit$history$val_loss))

    }




  }


  out.cv = data.frame(
    number_of_neurons=number_of_neurons,
    number_of_layers = number_of_layers,
    loss=mean(loss),
    val_loss=mean(val_loss)
  )


  best.out.cv = out.cv%>%
    filter(val_loss==min(val_loss))

  out <- list(
    out.cv=out.cv,
    out.cv.best=best.out.cv)

  return(out)

}




# Function must take the hyper-parameters as inputs
scenario_number <- 1
categorical_features <- c("claim_type")
continuous_features <- character(0)
development_period <- "DP"
accident_period <- "AP"
id_col <- "claim_number"

individual_continuous_features <- unique(c(continuous_features, accident_period))
columns_to_keep <- unique(c(
  id_col,
  categorical_features,
  continuous_features,
  paste0(accident_period, "_o"),
  paste0(development_period, "_o")
))

scoringFunction <- function(number_of_neurons,
                            number_of_layers){


  cv_out <- nn_model_cv(data=dt_target,
              number_of_neurons=number_of_neurons,
              number_of_layers=number_of_layers,
              categorical_features=categorical_features,
              continuous_features=continuous_features,
              max_dp=max_dp)


  lst <- list(
    Score = cv_out$out.cv.best$val_loss,
    train_loss = cv_out$out.cv.best$loss
  )

  return(lst)


}


set.seed(1964)
seeds <- sample.int(20)
#seeds <- seeds[9:20]


cl <- makeCluster(2)
registerDoParallel(cl)


clusterEvalQ(cl, {library("ParBayesianOptimization")
library("ReSurv")
library("dplyr")
library("tidyverse")
library("data.table")
library("doParallel")
library("fastDummies")
library("reticulate")
library("keras")
library("tensorflow")} )


for (i in seeds){

  input_data <- data_generator(
    random_seed = i,
    scenario = scenario_number,
    time_unit =  1 / 360,
    years = 4,
    period_exposure  = 200
  )
max_dp = max(input_data$AP)

  individual_data_y <- IndividualDataPP(input_data,
                                        id = id_col,
                                        categorical_features = categorical_features,
                                        continuous_features = individual_continuous_features,
                                        accident_period = accident_period,
                                        calendar_period = "RP",
                                        input_time_granularity = "days",
                                        output_time_granularity = "months",
                                        years = 4,
                                        continuous_features_spline = NULL,
                                        calendar_period_extrapolation = FALSE)



  input_data <- individual_data_y$training.data
  setDT(input_data)

  input_data[,DP_o := max(DP_rev_o)-DP_rev_o +1]

  input_data <- input_data[,.SD, .SDcols = columns_to_keep]

  setnames(input_data, c("AP_o", "DP_o"),c("AP","DP"))

  input_data[,RP:=(DP+AP-1)]

  max_dp = max(input_data$AP)

      setDT(input_data)
  # input_data[,DP:=pmin(RP-AP+1,max_dp)]

  vec1 <- sort(unique(input_data$DP))
  vec2 <- 1:max_dp

  v3 <- setdiff(union(vec1, vec2), intersect(vec1, vec2))

  wrapped_data <- data_wrapper(data=input_data,
                               categorical_features = categorical_features,
                               continuous_features=continuous_features,
                               development_period = development_period,
                               accident_period=accident_period)
  
  if(length(v3)!=0){
    
    extra_data <- tail(wrapped_data, 1)
    
    # Duplicate the last row length(v3) times
    extra_data <- extra_data[rep(1, length(v3))]
    
    # Assign the DP column
    extra_data[, DP := v3]
    
    extra_data[, dt_count := 0]
    wrapped_data <- rbind(wrapped_data,
                          extra_data)
    
  }

 setDT(wrapped_data)
  wrapped_data<-wrapped_data[order(DP),.SD,by=c(categorical_features,
                               continuous_features,
                               accident_period)]

    dt_target <- mw18_datapp_y(wrapped_data,
              development_time = development_period,
              dt_counts = "dt_count"
              )

    setDT(dt_target)
columns<-  c(paste0("DP",1:max_dp))


tmp = as.data.frame(t(apply(dt_target[,..columns],1,cumsum)))

setDT(tmp)

dt_target[,(columns):=tmp]


dt_covariates <- mw18_datapp_x(dt_target,
              categorical_features = categorical_features,
               continuous_features=continuous_features)


common_cols <- intersect(colnames(dt_covariates),
                         colnames(dt_target))


setDT(dt_covariates)

dt_target[, (common_cols):=dt_covariates[,..common_cols]]    


  objects_export <- list(
    "nn_model_cv",
    "MinMaxScaler",
    "data_wrapper",
    "mw18_datapp_x",
    "mw18_datapp_y",
    "dt_target",
    "max_dp"

  )
  clusterExport(cl, objects_export, envir = environment())

    bounds <- list(number_of_neurons = c(1L, 10L),
               number_of_layers = c(1L, 2L))

    
  bayes_out <- bayesOpt(
    FUN = scoringFunction
    , bounds = bounds
    , initPoints = 3
    , iters.n = 50
    , iters.k = 2
    , otherHalting = list(timeLimit = 5000)
    , parallel = TRUE
  )


  name <- file.path(
    cv_results_dir,
    sprintf("sim%s_%s_marios_cv_%s.csv", scenario_number, i, format(Sys.time(), "%Y_%m_%d_%H:%M"))
  )

  write.csv(bayes_out$scoreSummary, file = name)
}
parallel::stopCluster(cl)
