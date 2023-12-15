library(ReSurv)
require(parallel)

sim_fitting <- function(seed){
    

results=matrix(ncol=5)
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
    
    
 CL = individual_data$training.data %>%
    mutate(DP_o = max(individual_data$training.data$DP_rev_o)-DP_rev_o + 1) %>%
    group_by(AP_o, DP_o) %>%
    summarize(I=sum(I), .groups="drop") %>%
    group_by(AP_o) %>%
    arrange(DP_o) %>%
    mutate(I_cum = cumsum(I),
           I_cum_lag = lag(I_cum, default=0)) %>%
    ungroup() %>%
    group_by(DP_o) %>%
    reframe(df = sum(I_cum*(AP_o<=max(individual_data$training.data$AP_o)-DP_o+1)) /
              sum(I_cum_lag*(AP_o<=max(individual_data$training.data$AP_o)-DP_o+1)),
            I=sum(I*(AP_o<=max(individual_data$training.data$AP_o)-DP_o))) %>%
    mutate(DP_o_join = DP_o) %>%
    mutate(DP_rev_o = max(DP_o) - DP_o +1 )

  latest_observed <- individual_data$training.data %>%
    filter(DP_rev_o >=TR_o) %>%
    mutate(DP_o = max(individual_data$training.data$DP_rev_o)-DP_rev_o + 1) %>%
    group_by(AP_o) %>%
    mutate(DP_max =max(DP_o)) %>%
    group_by(AP_o, DP_max) %>%
    summarize(I=sum(I),.groups="drop")

  predictions <- expand.grid(AP_o = latest_observed$AP_o, DP_o = CL$DP_o_join) %>%
    left_join(CL[,c("DP_o_join", "df")], by=c("DP_o"="DP_o_join")) %>%
    left_join(latest_observed, by="AP_o") %>%
    rowwise() %>% 
    filter(DP_o>DP_max) %>%
    ungroup() %>% 
    group_by(AP_o) %>%
    arrange(DP_o) %>%
    mutate(df_cum = cumprod(df) ) %>%
    mutate(I_expected= I*df_cum-I*lag(df_cum,default=1)) %>%
    select(DP_o, AP_o, I_expected)

  conversion_factor = individual_data$conversion_factor
  max_dp_i <- 1440

  true_output_cl <- individual_data$full.data %>%
    mutate(
      DP_rev_o = floor(max_dp_i*conversion_factor)-ceiling(DP_i*conversion_factor+((AP_i-1)%%(1/conversion_factor))*conversion_factor) +1,
      AP_o = ceiling(AP_i*conversion_factor),
      TR_o= AP_o-1
    ) %>%
    filter(DP_rev_o <=TR_o) %>%
    mutate(DP_o = max(individual_data$training.data$DP_rev_o)-DP_rev_o + 1) %>%
    group_by(AP_o, DP_o, DP_rev_o) %>%
    summarize(I=sum(I), .groups="drop") %>%
    filter(DP_rev_o>0)

  score_total <- predictions  %>%
    inner_join(true_output_cl,
               by =c("AP_o", "DP_o")) %>%
    mutate(ave = I-I_expected,
           abs_ave = abs(ave)) %>%
    # from here it is reformulated for the are tot
    ungroup()%>%
    group_by(AP_o, DP_rev_o) %>%
    reframe(abs_ave=abs(sum(ave)),
           I=sum(I))
    
    are_tot=sum(score_total$abs_ave)/sum(score_total$I)
    
     score_diagonal <- individual_data$full.data  %>%
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
     mutate(DP_o = max(individual_data$training.data$DP_rev_o)-DP_rev_o + 1) %>%
    left_join(CL[,c("DP_o","df")], by = c("DP_o")) %>%
    mutate(I_cum_hat =  I_cum_lag * df,
           RP_o = max(DP_rev_o)-DP_rev_o + AP_o) %>%
    inner_join(true_output_cl[,c("AP_o", "DP_rev_o")] %>%  distinct()
               , by =c("AP_o", "DP_rev_o")) %>%
    group_by(AP_o,DP_rev_o) %>%
    reframe(abs_ave2_diag = abs(sum(I_cum_hat)-sum(I_cum)),
          I=sum(I))  
    
    are_cal_q=sum(score_diagonal$abs_ave2_diag)/sum(score_diagonal$I)
    
    
    # yearly 
    
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
                                  output_time_granularity = "years",
                                  years=4,
                                  continuous_features_spline=NULL,
                                  calendar_period_extrapolation=F)
    
CL = individual_data$training.data %>%
    mutate(DP_o = max(individual_data$training.data$DP_rev_o)-DP_rev_o + 1) %>%
    group_by(AP_o, DP_o) %>%
    summarize(I=sum(I), .groups="drop") %>%
    group_by(AP_o) %>%
    arrange(DP_o) %>%
    mutate(I_cum = cumsum(I),
           I_cum_lag = lag(I_cum, default=0)) %>%
    ungroup() %>%
    group_by(DP_o) %>%
    reframe(df = sum(I_cum*(AP_o<=max(individual_data$training.data$AP_o)-DP_o+1)) /
              sum(I_cum_lag*(AP_o<=max(individual_data$training.data$AP_o)-DP_o+1)),
            I=sum(I*(AP_o<=max(individual_data$training.data$AP_o)-DP_o))) %>%
    mutate(DP_o_join = DP_o) %>%
    mutate(DP_rev_o = max(DP_o) - DP_o +1 )

  latest_observed <- individual_data$training.data %>%
    filter(DP_rev_o >=TR_o) %>%
    mutate(DP_o = max(individual_data$training.data$DP_rev_o)-DP_rev_o + 1) %>%
    group_by(AP_o) %>%
    mutate(DP_max =max(DP_o)) %>%
    group_by(AP_o, DP_max) %>%
    summarize(I=sum(I),.groups="drop")

  predictions <- expand.grid(AP_o = latest_observed$AP_o, DP_o = CL$DP_o_join) %>%
    left_join(CL[,c("DP_o_join", "df")], by=c("DP_o"="DP_o_join")) %>%
    left_join(latest_observed, by="AP_o") %>%
    rowwise() %>% 
    filter(DP_o>DP_max) %>%
    ungroup() %>% 
    group_by(AP_o) %>%
    arrange(DP_o) %>%
    mutate(df_cum = cumprod(df) ) %>%
    mutate(I_expected= I*df_cum-I*lag(df_cum,default=1)) %>%
    select(DP_o, AP_o, I_expected)

  conversion_factor = individual_data$conversion_factor
  max_dp_i <- 1440

  true_output_cl <- individual_data$full.data %>%
    mutate(
      DP_rev_o = floor(max_dp_i*conversion_factor)-ceiling(DP_i*conversion_factor+((AP_i-1)%%(1/conversion_factor))*conversion_factor) +1,
      AP_o = ceiling(AP_i*conversion_factor),
      TR_o= AP_o-1
    ) %>%
    filter(DP_rev_o <=TR_o) %>%
    mutate(DP_o = max(individual_data$training.data$DP_rev_o)-DP_rev_o + 1) %>%
    group_by(AP_o, DP_o, DP_rev_o) %>%
    summarize(I=sum(I), .groups="drop") %>%
    filter(DP_rev_o>0)

  score_total <- predictions  %>%
    inner_join(true_output_cl,
               by =c("AP_o", "DP_o")) %>%
    mutate(ave = I-I_expected,
           abs_ave = abs(ave)) %>%
    # from here it is reformulated for the are tot
    ungroup()%>%
    group_by(AP_o, DP_rev_o) %>%
    reframe(abs_ave=abs(sum(ave)),
           I=sum(I))
    
    
     score_diagonal <- individual_data$full.data  %>%
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
     mutate(DP_o = max(individual_data$training.data$DP_rev_o)-DP_rev_o + 1) %>%
    left_join(CL[,c("DP_o","df")], by = c("DP_o")) %>%
    mutate(I_cum_hat =  I_cum_lag * df,
           RP_o = max(DP_rev_o)-DP_rev_o + AP_o) %>%
    inner_join(true_output_cl[,c("AP_o", "DP_rev_o")] %>%  distinct()
               , by =c("AP_o", "DP_rev_o")) %>%
    group_by(AP_o,DP_rev_o) %>%
    reframe(abs_ave2_diag = abs(sum(I_cum_hat)-sum(I_cum)),
          I=sum(I))
    
    are_cal_y=sum(score_diagonal$abs_ave2_diag)/sum(score_diagonal$I)
    
    results=rbind(results,c(are_tot,are_cal_q,are_cal_y,scenario,seed))
    
 }
    
    results <- results[-1,]
    
    name = paste0("~/modi_mount/Scoring/Fitting_results",  "/seed","_",seed,"_","CL","_",format(Sys.time(), "%Y_%m_%d_%H_%M"),".csv")
    
    colnames(results) <- c("are_tot",
                          "are_cal_q",
                          "are_cal_y",
                          "scenario",
                          "seed")
    
    fwrite(results,name)
}


library(data.table)


set.seed(1964)
seeds<-1:20       
cl <- makeCluster(20)
random_seed=1964
objects_export <- list(
       "random_seed",
        "sim_fitting"
     )
clusterExport(cl, objects_export, envir = environment())

clusterEvalQ(cl, {library("fastDummies")
                      library("ReSurv")
                      library("data.table")
                      set.seed(random_seed)} )


parallel::parSapply(cl, 
                    seeds, 
                    FUN=sim_fitting)



