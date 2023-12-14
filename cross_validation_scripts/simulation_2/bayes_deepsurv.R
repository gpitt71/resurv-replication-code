library(ParBayesianOptimization)
library(ReSurv)
library(doParallel)

# Function must take the hyper-parameters as inputs
obj_func <- function(num_layers, num_nodes, optim, activation,lr, xi, eps ) {
  
  optim = switch(optim, "Adam", "SGD")
  activation = switch(activation, "LeakyReLU","SELU")
  batch_size=as.integer(5000)
  number_layers=as.integer(num_layers)
  num_nodes=as.integer(num_nodes)
  
  deepsurv_cv <- ReSurvCV(IndividualData=individual_data,
                    model="deepsurv",
                    hparameters_grid=list(num_layers = num_layers,
                                         num_nodes = num_nodes,
                                         optim=optim,
                                         activation = activation,
                                         lr=lr,
                                         xi=xi,
                                         eps = eps,
                                         tie = "Efron",
                                         batch_size = batch_size,
                                         early_stopping = 'TRUE',
                                         patience  = 30
                    ),
                    epochs=as.integer(300),
                    num_workers = as.integer(2),      
                    verbose=F,
                    verbose.cv=T,
                    folds=3,
                    parallel=F,
                    random_seed = as.integer(Sys.time()))
  

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
#seeds<-seeds[13:20]

cl <- makeCluster(32)
registerDoParallel(cl)


clusterEvalQ(cl, {library("ReSurv")} )


for (i in seeds){
  
  
    input_data <- data_generator(random_seed = i,
                              scenario=2,
                              time_unit = 1/360,
                              years = 4,
                              yearly_exposure = 200)
    
  individual_data <- IndividualData(data = input_data,
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
  
  
  name = paste0("~/modi_mount/ReSurv_cv_results",  "/sim2_",i,"_deepsurv_cv_",format(Sys.time(), "%Y_%m_%d_%H:%M"),".csv")
  
  write.csv(bayes_out$scoreSummary, file = name)
}
parallel::stopCluster(cl)