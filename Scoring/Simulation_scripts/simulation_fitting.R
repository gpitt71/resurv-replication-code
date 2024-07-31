reticulate::use_virtualenv("pyresurv")
torch <- reticulate::import("torch", delay_load = TRUE)
torchtuples <- reticulate::import('torchtuples', delay_load = TRUE)
shap <- reticulate::import('shap', delay_load = TRUE)

library(ReSurv)
require(parallel)

sim_fitting <- function(seed, names_table, location, model){
    
    
for(scenario in 0:4){
input_data <- data_generator(random_seed = seed,
                              scenario=scenario,
                              time_unit = 1/360,
                              years = 4,
                              yearly_exposure = 200)

individual_data <- IndividualData(input_data,
                                  id="claim_number",
                                  continuous_features="AP_i",
                                  categorical_features="claim_type",
                                  accident_period="AP",
                                  calendar_period="RP",
                                  input_time_granularity = "days",
                                  output_time_granularity = "quarters",
                                  years=4,
                                  continuous_features_spline=NULL,
                                  calendar_period_extrapolation=F)

file_name <-names_table[
  names_table$sim==scenario &
  names_table$seed == seed &
  names_table$model==model,
]$org

if(!(model %in% c("cox", "ltrc"))){
hparameters <- fread(paste0(location,"/",file_name)) %>%
  filter(Score==max(Score,na.rm=T))
hparameters<-hparameters[1,]
}

    
if(model=="xgboost"){   
    start<-Sys.time()
resurv.fit <- ReSurv(individual_data,
                             hazard_model = "xgboost",
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
                                                early_stopping_rounds = 500))
    
resurv.fit.predict <- predict(resurv.fit,
                    grouping_method = "probability")

time <- Sys.time() - start
}
if(model=="cox"){
    if(scenario %in% c(2,3,4)){
        cfs <- "AP_i"
     }
else{
    cfs <- NULL
}
individual_data <- IndividualData(input_data,
                                  id="claim_number",
                                  continuous_features="AP_i",
                                  categorical_features="claim_type",
                                  accident_period="AP",
                                  calendar_period="RP",
                                  input_time_granularity = "days",
                                  output_time_granularity = "quarters",
                                  years=4,
                                  continuous_features_spline=cfs,
                                  calendar_period_extrapolation=F)


start<-Sys.time()
resurv.fit <- ReSurv(individual_data,
                             hazard_model = "cox")
    
resurv.fit.predict <- predict(resurv.fit,
                    grouping_method = "probability")

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
                             hazard_model = "deepsurv",
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
                                              tie='Efron'))
    
    
resurv.fit.predict <- predict(resurv.fit,
                    grouping_method = "probability")

time <- Sys.time() - start
} 
if(model=="ltrc"){
hparameters <- fread(paste0(location,"/",file_name)) %>%
  filter(xerror==min(xerror,na.rm=T))  %>%
    filter(is_lkh==min(is_lkh))
    
hparameters<-hparameters[1,]           
  
    cp =hparameters$CP
  minsplit =hparameters$minsplit
    
start<-Sys.time()
resurv.fit <- ReSurv(individual_data,
                             hazard_model = "LTRCtrees",
                             hparameters = list(cp=cp,
                                               minsplit = minsplit))
    
    
    
resurv.fit.predict <- predict(resurv.fit,
                    grouping_method = "probability")

time <- Sys.time() - start
}    
    
    
resurv.fit.predict$fitting_time <- time

name = paste0("~/modi_mount/Scoring/Fitting_results",  "/sim",scenario,"_",seed,"_",model,"_",format(Sys.time(), "%Y_%m_%d_%H_%M"),".RData")

saveRDS(resurv.fit.predict, file=name)
    }
}


library(data.table)


set.seed(1964)
seeds<-1:20
#seeds<-c(15)
args = commandArgs(trailingOnly=TRUE)
print(args)
location <- "~/modi_mount/ReSurv_cv_results"
   
names <- list.files(location)
names_relevant <- names[substr(names,1,4) %in% c("sim0","sim1","sim2","sim3","sim4")]
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
    
cl <- makeCluster(20)
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
                      library("data.table")
                      set.seed(random_seed)} )


parallel::parSapply(cl, 
                    seeds, FUN=sim_fitting,
                    names_table = names_table,
                    location=location,
                    model=args[1])



