library(ReSurv)
require(parallel)

scoring_function <-function(resurv.predict_table){

resurv.predict <- readRDS(paste0( "~/modi_mount/Scoring/Fitting_results/",resurv.predict_table))
    
conversion_factor <- resurv.predict$ReSurvFit$IndividualData$conversion_factor


max_dp_i <-1440

    
  crps_dt <- ReSurv::survival_crps(resurv.predict$ReSurvFit)
  crps_result <- mean(crps_dt$crps)
    
  true_output <- resurv.predict$ReSurvFit$IndividualData$full.data %>%
    mutate(
      DP_rev_o = floor(max_dp_i*conversion_factor)-ceiling(DP_i*conversion_factor+((AP_i-1)%%(1/conversion_factor))*conversion_factor) +1,
      AP_o = ceiling(AP_i*conversion_factor),
      TR_o= AP_o-1
    ) %>%
    filter(DP_rev_o <=TR_o) %>%
    group_by(claim_type, AP_o, DP_rev_o) %>%
    mutate(claim_type = as.character(claim_type)) %>%
    summarize(I=sum(I), .groups = "drop") %>%
    filter(DP_rev_o>0) #we cant have =0, because corresponds to half a parallelogram.

  #Total output
  score_total<- resurv.predict$hazard_frame_output[,c("claim_type","AP_o", "DP_rev_o", "I_expected")] %>%
    inner_join(true_output, by =c("claim_type","AP_o", "DP_rev_o")) %>%
    mutate(ave = I-I_expected,
           abs_ave = abs(ave)) %>%
    # from here it is reformulated for the are tot
    ungroup()%>%
    group_by(AP_o, DP_rev_o) %>%
    reframe(abs_ave=abs(sum(ave)),
           I=sum(I))
    

  dfs_output <- resurv.predict$hazard_frame_output %>%
    select(AP_o, claim_type, DP_rev_o, df_o) %>%
    mutate(DP_rev_o = DP_rev_o) %>% 
    distinct()

  #Cashflow on output scale.Etc quarterly cashflow development
  score_diagonal <- resurv.predict$ReSurvFit$IndividualData$full.data  %>%
    mutate(
      DP_rev_o = floor(max_dp_i*conversion_factor)-ceiling(DP_i*conversion_factor+((AP_i-1)%%(1/conversion_factor))*conversion_factor) +1,
      AP_o = ceiling(AP_i*conversion_factor)
    ) %>%
    group_by(claim_type, AP_o, DP_rev_o) %>%
    mutate(claim_type = as.character(claim_type)) %>%
    summarize(I=sum(I), .groups = "drop") %>%
    group_by(claim_type, AP_o) %>%
    arrange(desc(DP_rev_o)) %>%
    mutate(I_cum=cumsum(I)) %>%
    mutate(I_cum_lag = lag(I_cum, default = 0)) %>%
    left_join(dfs_output, by = c("AP_o", "claim_type", "DP_rev_o")) %>%
    mutate(I_cum_hat =  I_cum_lag * df_o,
           RP_o = max(DP_rev_o)-DP_rev_o + AP_o) %>%
    inner_join(true_output[,c("AP_o", "DP_rev_o")] %>%  distinct()
               , by =c("AP_o", "DP_rev_o")) %>%
    group_by(AP_o,DP_rev_o) %>%
    reframe(abs_ave2_diag = abs(sum(I_cum_hat)-sum(I_cum)),
          I=sum(I)) 
    
    are_cal_q=sum(score_diagonal$abs_ave2_diag)/sum(score_diagonal$I)
    
    #old code for score_diagonal ----
#     mutate(ave_2 =  I_cum-I_cum_lag * df_o,
#            abs_ave_2 = abs(ave_2),
#            RP_o = max(DP_rev_o)-DP_rev_o + AP_o) %>%
#     inner_join(true_output[,c("AP_o", "DP_rev_o")] %>%  distinct()
#                , by =c("AP_o", "DP_rev_o"))
    
#       relative_diagonal = score_diagonal %>% group_by(RP_o) %>%
#     summarize(relative_diagonal = sum(ave_2,na.rm=T)/sum(I),
#               I=sum(I))

#   weighted_relative_diagonal = sum(relative_diagonal$relative_diagonal*relative_diagonal$I)/(sum(relative_diagonal$I))
    
    
    
    # Old measure
    # relative_total = sum(score_total$ave)/sum(score_total$I)
    # ARE TOT
    are_tot=sum(score_total$abs_ave)/sum(score_total$I)
    # RMSE_total = sqrt(mean(score_total$ave^2)) 


    
# Now do yearly
    scenario = substr(resurv.predict_table,
                       4,
                       4)
    seed=substr(resurv.predict_table,
                     unlist(gregexpr('_', resurv.predict_table))[1] +1,
                     unlist(gregexpr('_', resurv.predict_table))[2]-1 )
    
    input_data <- data_generator(random_seed = seed,
                              scenario=scenario,
                              time_unit = 1/360,
                              years = 4,
                              yearly_exposure = 200)

    individual_data2 <- IndividualData(input_data,
                                  id="claim_number",
                                  continuous_features="AP_i",
                                  categorical_features="claim_type",
                                  accident_period="AP",
                                  calendar_period="RP",
                                  input_time_granularity = "days",
                                  output_time_granularity = "years",
                                  years=4,
                                  continuous_features_spline=NULL,
                                  calendar_period_extrapolation=F)
    
    resurv.predict <- predict(resurv.predict$ReSurvFit,
                                  newdata=individual_data2,
                             grouping_method = "probability")
    
    conversion_factor <- individual_data2$conversion_factor


  max_dp_i <-1440


  true_output <- individual_data2$full.data %>%
    mutate(
      DP_rev_o = floor(max_dp_i*conversion_factor)-ceiling(DP_i*conversion_factor+((AP_i-1)%%(1/conversion_factor))*conversion_factor) +1,
      AP_o = ceiling(AP_i*conversion_factor),
      TR_o= AP_o-1
    ) %>%
    filter(DP_rev_o <=TR_o) %>%
    group_by(claim_type, AP_o, DP_rev_o) %>%
    mutate(claim_type = as.character(claim_type)) %>%
    summarize(I=sum(I), .groups = "drop") %>%
    filter(DP_rev_o>0) #we cant have =0, because corresponds to half a parallelogram.

  dfs_output <- resurv.predict$hazard_frame_output %>%
    select(AP_o, claim_type, DP_rev_o, df_o) %>%
    mutate(DP_rev_o = DP_rev_o) %>% 
    distinct()

  #Cashflow on output scale.Etc quarterly cashflow development
  score_diagonal_yearly <- individual_data2$full.data  %>%
    mutate(
      DP_rev_o = floor(max_dp_i*conversion_factor)-ceiling(DP_i*conversion_factor+((AP_i-1)%%(1/conversion_factor))*conversion_factor) +1,
      AP_o = ceiling(AP_i*conversion_factor)
    ) %>%
    group_by(claim_type, AP_o, DP_rev_o) %>%
    mutate(claim_type = as.character(claim_type)) %>%
    summarize(I=sum(I), .groups = "drop") %>%
    group_by(claim_type, AP_o) %>%
    arrange(desc(DP_rev_o)) %>%
    mutate(I_cum=cumsum(I)) %>%
    mutate(I_cum_lag = lag(I_cum, default = 0)) %>%
    left_join(dfs_output, by = c("AP_o", "claim_type", "DP_rev_o")) %>%
    mutate(I_cum_hat =  I_cum_lag * df_o,
           RP_o = max(DP_rev_o)-DP_rev_o + AP_o) %>%
    inner_join(true_output[,c("AP_o", "DP_rev_o")] %>%  distinct()
               , by =c("AP_o", "DP_rev_o")) %>%
    group_by(AP_o,DP_rev_o) %>%
    reframe(abs_ave2_diag = abs(sum(I_cum_hat)-sum(I_cum)),
          I=sum(I)) 
    
    are_cal_y=sum(score_diagonal_yearly$abs_ave2_diag)/sum(score_diagonal_yearly$I)
    
    # old code for the yearly aggregation
    # mutate(ave_2 =  I_cum-I_cum_lag * df_o,
    #        abs_ave_2 = abs(ave_2),
    #        RP_o = max(DP_rev_o)-DP_rev_o + AP_o) %>%
    # inner_join(true_output[,c("AP_o", "DP_rev_o")] %>%  distinct()
    #            , by =c("AP_o", "DP_rev_o"))   
    # relative_diagonal_yearly = score_diagonal_yearly %>% group_by(RP_o) %>%
    # summarize(relative_diagonal = sum(ave_2,na.rm=T)/sum(I),
    #           I=sum(I))

  # weighted_relative_diagonal_yearly = sum(relative_diagonal_yearly$relative_diagonal*relative_diagonal_yearly$I)/(sum(relative_diagonal_yearly$I))

  scoring_result<- list(
    score_total = score_total,
    score_diagonal = score_diagonal,
    score_diagonal_yearly = score_diagonal_yearly,
    # old output----
    # relative_diagonal = relative_diagonal,
    # relative_diagonal_yearly = relative_diagonal_yearly,   
    # metrics =list(relative_total = relative_total,
    #               RMSE_total = RMSE_total,
    #               weighted_relative_diagonal = weighted_relative_diagonal,
    #               weighted_relative_diagonal_yearly = weighted_relative_diagonal_yearly
    # )
    metrics=list(are_tot=are_tot,
                are_cal_q=are_cal_q,
                are_cal_y=are_cal_y,
                crps_result=crps_result)
  
  )
    
    
    
 sim = substr(resurv.predict_table,
                       4,
                       4)
          seed=substr(resurv.predict_table,
                     unlist(gregexpr('_', resurv.predict_table))[1] +1,
                     unlist(gregexpr('_', resurv.predict_table))[2]-1 )
         model = substr(resurv.predict_table,
                        unlist(gregexpr('_', resurv.predict_table))[2] +1,
                        unlist(gregexpr('_', resurv.predict_table))[3]-1)

    
  name = paste0("~/modi_mount/Scoring/Scoring_datasets", "/sim",sim,"_",seed,"_",model,"_",format(Sys.time(), "%Y_%m_%d_%H:%M"),".RData")
  saveRDS(scoring_result, file=name) 
    

  return(
  #    list(
  #  score_total = score_total,
  #  score_diagonal = score_diagonal,
  #  relative_diagonal = relative_diagonal,
  #  metrics =list(relative_total = relative_total,
  #                RMSE_total = RMSE_total,
  #                weighted_relative_diagonal = weighted_relative_diagonal
  #  ))
      # c(relative_total,
      # RMSE_total,
        # crps_result,
        # weighted_relative_diagonal,
        # weighted_relative_diagonal_yearly
      c(are_tot=are_tot,
        are_cal_q=are_cal_q,
        are_cal_y=are_cal_y,
        crps_result=crps_result)
  )
}


f_score_function <- function(seeds,model){
   
if(model!= "CL"){
location <- "~/modi_mount/Scoring/Fitting_results"
   
names <- list.files(location)
names_relevant <- names[substr(names,1,4) %in% c("sim0","sim1","sim2","sim3","sim4")]
names_table <- tibble(org=names_relevant) %>% rowwise() %>%
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
                unlist(gregexpr('_', org))[3] +1,
                unlist(gregexpr('\\.', org))[1]-1),
                format = "%Y_%m_%d_%H_%M")
         )%>%
  ungroup() %>%
  group_by(sim,seed, model) %>%
  filter(date==max(date))
 
names_table <- names_table[names_table$model == model,]
    

     
    names_table <- names_table %>% filter(seed==seeds)  %>%
  arrange(as.numeric(sim))
     
    result <- sapply(names_table$org,scoring_function)
    tmp <- data.frame(t(result))    
    tmp$seed <-seeds
    tmp$scenario <- sapply(0:(nrow(tmp)-1), function(x){paste0(as.character(x)) })     
  
  return(tmp)
}
    
}

args = commandArgs(trailingOnly=TRUE)
print(args[1])
seeds = 1:20

cl <- makeCluster(20)
random_seed=1964
objects_export <- list(
       "random_seed",
        "scoring_function"
     )
clusterExport(cl, objects_export, envir = environment())

clusterEvalQ(cl, {library("ReSurv")
                      library("fastDummies")
                      library("reticulate")
                      set.seed(random_seed)} )

scores<-parallel::parSapply(cl, seeds, FUN=f_score_function,
            model = args[1] )

score_unnested<-tibble(
  # relative_total = unlist(scores[1,]),
  # RMSE_total = unlist(scores[2,]),
    # crps = unlist(scores[2,]),
  # cash_flow_quarterly = unlist(scores[3,]),
  # cash_flow_yearly = unlist(scores[4,]),
    are_tot = unlist(scores[1,]),
    are_cal_q=unlist(scores[2,]),
    are_cal_y=unlist(scores[3,]),
    crps_result=unlist(scores[4,]),
    seed = unlist(scores[5,]),
    scenario = unlist(scores[6,])
)

name = paste0("~/modi_mount/Scoring/Scoring_results",  "/sim_",args[1],"_",format(Sys.time(), "%Y_%m_%d_%H:%M"),".csv")

write.csv(score_unnested, file = name)
