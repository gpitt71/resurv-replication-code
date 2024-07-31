library(data.table)
library(dplyr)
library(ReSurv)
##

maximum.time <- function(years,
                         input_time_granularity){
  
  "
  This function returns the triangle width.

  years: numeric, number of years in the triangle.
  input_time_granularity: numeric, input data granularity with respect to the one year reference. E.g., 1/12 for months.

  "
  
  time_unit_string <- c('days','months','quarters', 'semesters', 'years')
  time_unit_numeric <- c(1/360, 1/12, 1/4, 1/2, 1)
  
  input.pos <- which(time_unit_string%in%intersect(input_time_granularity,time_unit_string))
  
  years/time_unit_numeric[input.pos]
  
}



construct_actual_expected <-function(resurv.predict_table){
  
  resurv.predict <- readRDS(paste0( "~/Fitting_results/",resurv.predict_table))
  
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
    ungroup()
  
  
  dfs_output <- resurv.predict$hazard_frame_output %>%
    select(AP_o, claim_type, DP_rev_o, df_o) %>%
    mutate(DP_rev_o = DP_rev_o) %>% 
    distinct()
  
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
    inner_join(dfs_output, by = c("AP_o", "claim_type", "DP_rev_o")) %>%
    mutate(ave_2 =  I_cum-I_cum_lag * df_o,
           abs_ave_2 = abs(ave_2),
           RP_o = max(DP_rev_o)-DP_rev_o + AP_o) %>%
    inner_join(true_output[,c("AP_o", "DP_rev_o")] %>%  distinct()
               , by =c("AP_o", "DP_rev_o"))
  
  dtb <- as.data.table(score_diagonal)
  
  dtb<- dtb[,DP_o:=maximum.time(4,'quarters')-DP_rev_o+1]
  dtb <- dtb[,I_expected:=(I-ave_2)]
  dtb <- dtb[,list(expected=sum(I_expected),
                   actual=sum(I)),
             by=list(AP_o,DP_o)]
  
  
  scenario = substr(resurv.predict_table,
                    4,
                    4)
  seed=substr(resurv.predict_table,
              unlist(gregexpr('_', resurv.predict_table))[1] +1,
              unlist(gregexpr('_', resurv.predict_table))[2]-1 )
  
  model=substr(resurv.predict_table,
              unlist(gregexpr('_', resurv.predict_table))[2] +1,
              unlist(gregexpr('_', resurv.predict_table))[3]-1 )
  
  
  dtb[,scenario:=scenario]
  dtb[,seed:=seed]
  dtb[,model:=model]
  
  tname<- paste0('~/heatmap_data','\\hmp','_',model,'_',seed,'_',scenario,'_',format(Sys.time(), "%Y_%m_%d_%H_%M"),".csv")
  fwrite(dtb,tname)
  
  }



##

location <- "~/Fitted_models/"

names <- list.files(location)
names_relevant <- names[substr(names,1,3) %in% 'sim']
names_table <- tibble(org=names_relevant) %>% rowwise() %>%
  mutate( scenario = substr(org,
                            4,
                            4),
          seed = substr(org,
                        unlist(gregexpr('_', org))[1] +1,
                        unlist(gregexpr('_', org))[2]-1),
          model = substr(org,
                         unlist(gregexpr('_', org))[2] +1,
                         unlist(gregexpr('_', org))[3]-1),
          date = as.POSIXct(substr(org,
                                   unlist(gregexpr('_', org))[3] +1,
                                   unlist(gregexpr('\\.', org))[1]-1),
                            format = "%Y_%m_%d_%H_%M")
  )%>%
  ungroup() %>%
  group_by(scenario, seed, model) %>%
  filter(!is.na(date)) %>%
  filter(date==max(date))

setDT(names_table)


names_table[,construct_actual_expected(resurv.predict_table=org),by=org]


location2 <- "~/heatmap_data"
names_hmp <- list.files(location2)  

dt <- data.table()

for(ix in names_hmp){
  
  tmp <- fread(paste0(location2,'\\',ix))
  dt <- rbind(dt,tmp)
  
}

tb1res = dt %>%
  mutate(residuals=(expected-actual)/actual,
         scenario=as.character(scenario))%>%
  group_by(scenario,
           model,
           AP_o,
           DP_o) %>%
  reframe(m.residuals=mean(residuals)) %>%
  ungroup() %>%
  mutate(scenario=recode(scenario,
                         `0` = "Alpha",
                         `1` = "Beta",
                         `2` = "Gamma",
                         `3` = "Delta",
                         `4` = "Epsilon"),
         model=recode(model,
                      CL = "CL",
                      cox = "COX",
                      deepsurv = "NN",
                      xgboost = "XGB",
                      .default = model))
heat.lim=c(-.2,.3)
tb1res %>%
  ggplot(aes(DP_o, AP_o, fill = m.residuals))+
  facet_wrap(~model+factor(scenario, levels=c('Alpha', 'Beta', 'Gamma', 'Delta', 'Epsilon')),ncol = 5) +
  scale_y_reverse() +
  geom_tile(color = "white")+
  ggplot2::scale_fill_gradient2(name=expression(RE^{kj}),
                                low="royalblue",
                                mid="#AAAABC",
                                high="#a71429",
                                midpoint=0,
                                space="Lab",
                                na.value="#454555",
                                oob=scales::squish,
                                limits=heat.lim,
                                guide="colourbar")+
  # scale_fill_gradient2(low = "blue", high = "red", mid = "green",
  # midpoint = 0, limit = c(0,+0.12), space = "Lab", oob=scales::squish,
  # name="Squared errors") +
  theme_bw(base_size=rel(4))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1),
        plot.title = element_text(size=18),
        strip.text = element_text(size=18),
        legend.text = element_text(size=14))+
  coord_fixed() + ggtitle("") +
  xlab("Development Period") + ylab("Accident Period")
