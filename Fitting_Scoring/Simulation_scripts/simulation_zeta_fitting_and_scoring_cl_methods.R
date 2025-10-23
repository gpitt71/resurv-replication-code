library(ReSurv)
library(data.table)
library(fastDummies)
library(keras)
require(parallel)
library(reticulate)
# library(clmplus)
# library(ChainLadder)

# Preprocess covariatesfile
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


fit_proportional_model <- function(data,
                                   max_dp){


  out <- list()

  counts_at_diagonal <- list()

  predictions_at_runoff <- list()

  lt<-(matrix(NA,nrow=max_dp,ncol=max_dp))

  colnames(lt) <- paste0("DP",1:max_dp)

  for (i in 2:max_dp) {

    proportions_ap <- rep(NA,(max_dp-1))

    zero_at_diagonal <- data[[paste0("DP", (max_dp - i + 1))]] == 0 # take them all here and just filer afterwards.

    at_diagonal <- data[["AP"]] ==(max_dp - (max_dp - i))

    counts_unfiltered <- sum(data[AP < (max_dp - (max_dp - i)), ][[paste0("DP", (max_dp -
                                                                                   i + 1))]])

    counts_at_diagonal[[paste0("AP",i)]] <- sum(data[AP == (max_dp - (max_dp - i)), ][[paste0("DP", (max_dp -
                                                                                                       i + 1))]])

    predicted_cumulatives <- NA

    if (sum(zero_at_diagonal & (!at_diagonal)) > 0) {

      j = (max_dp - i + 1)

      tmp <- data[zero_at_diagonal& (AP < (max_dp - j + 1)),][[paste0("DP", j+1)]]

      proportions_ap[j+1] <- sum(tmp)/counts_unfiltered


      if(i>2){

        for (j in (max_dp - i + 1 +1):(max_dp-1)) {

          tmp <- data[zero_at_diagonal& (AP < (max_dp - j + 1)),][[paste0("DP", j+1)]]
          tmp_1 <- data[zero_at_diagonal& (AP < (max_dp - j + 1)),][[paste0("DP", j)]]



          prop_tmp <-  sum(tmp)/sum(tmp_1)

          if(is.infinite(prop_tmp)){prop_tmp<-1}

          proportions_ap[j+1] <-prop_tmp

        }}


    }


    out[[paste0("AP",i)]] <- proportions_ap

    vector_of_predictions <- cumprod(c(counts_at_diagonal[[paste0("AP",i)]],proportions_ap[!is.na(proportions_ap)]))

    lt[i,!is.na(proportions_ap)]<-vector_of_predictions[-1]

    predictions_at_runoff[[paste0("AP",i)]]<- counts_at_diagonal[[paste0("AP",i)]]*last(cumprod(proportions_ap[!is.na(proportions_ap)]))



  }

  lt <- as.data.frame.matrix(cbind(AP=1:max_dp,
                                   lt))

  total_out <- list(proportions=out,
                    lower_triangle=lt,
                    counts_at_diagonal=counts_at_diagonal,
                    predictions_at_runoff=predictions_at_runoff)

  return(total_out)
}

nn_model_fit_and_predict <- function(data,
                                     max_dp,
                                     number_of_neurons,
                                     number_of_layers,
                                     categorical_features,
                                     continuous_features) {

  y_data_columns <- paste0("DP", 1:max_dp)
    x_cat <- dummy_cols(data[,..categorical_features],
                      select_columns=categorical_features,
                      remove_first_dummy=F)

  x_cat[, (categorical_features) := NULL]


  x_data <- cbind(x_cat,
                  data[,..continuous_features])

  y_data <- data[,..y_data_columns]

  output_frame <- data.table::copy(data)
  output_frame_score_diagonal <- data.table::copy(data)

  for(j in 1:(max_dp-1)){

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


      model$compile(loss = "mse", optimizer = "rmsprop")

      dat.X <- as.matrix.data.frame(x_data[train_set,][cond_positive_denominator,])
      dat.W <- as.matrix(wtrain[cond_positive_denominator])
      dat.Y <- as.matrix(ytgt[cond_positive_denominator])


      my_batch_size = eval(parse(text=(paste0(as.integer(max(floor(length(ytgt)/10),2)),"L"))))
      if (j == (max_dp - 1)) {
        my_val_split = 0L
      } else{
        my_val_split = 0.1
      }


      model_fit <- model$fit(
        list(dat.X, dat.W),
        dat.Y,
        epochs = 500L,
        verbose = 0,
        batch_size = my_batch_size,
        validation_split = my_val_split
      )


      tmp_3 <- y_data[!train_set,][[paste0("DP", j)]]

      cond_positive_denominator_test <- tmp_3 >0

      x_test <- as.matrix.data.frame(x_data[!train_set,])

      w_test = as.matrix(sqrt(tmp_3))

      pred <- as.vector(model$predict(list(x_test,w_test)))*w_test

      tmp_4 <- output_frame[!train_set,][[paste0("DP", j)]]
      w_test_1 = as.matrix(sqrt(tmp_4))

      pred1 <- as.vector(model$predict(list(x_test,w_test_1)))*w_test_1

      pred1[is.na(pred1)] <- tmp_3[is.na(pred1)] #if the nn fits something negative you just put df to one

    }else{

      pred1 <- pred <- y_data[!train_set, ][[paste0("DP", j)]]

    }

    eval(parse(text=paste0("output_frame[!train_set,DP",j+1,":=pred1]")))
    eval(parse(text=paste0("output_frame_score_diagonal[!train_set,DP",j+1,":=pred]")))


  }

  out <- list(score_total=(output_frame),
              score_diagonal=(output_frame_score_diagonal))

  return(out)

}



sim_fitting <- function(seed,scenario){

  # Set folder path
  folder_path <- "~/modi_mount/ReSurv_cv_results/"

  # List all files in the folder with full path
  all_files <- list.files(path = folder_path, full.names = TRUE)

  # Filter files containing "marios_cv"
  marios_cv_files <- all_files[grepl("marios_cv", all_files)]

  # Extract just filenames
  file_names <- basename(marios_cv_files)

  # Build data.table
  dt <- data.table(file = file_names, path = marios_cv_files)

  # Extract using regular expression
  dt[, c("sim", "seed", "datetime") := {
    m <- regexec("sim(\\d+)_(\\d+)_marios_cv_(\\d{4}_\\d{2}_\\d{2}_\\d{2}:\\d{2})\\.csv", file)
    parsed <- regmatches(file, m)
    parsed <- lapply(parsed, function(x) if (length(x) == 4) x[2:4] else rep(NA, 3))
    as.data.table(do.call(rbind, parsed))
  }]

  # Convert types
  dt[, sim := as.integer(sim)]
  dt[, seed := as.integer(seed)]
  dt[, datetime := as.POSIXct(datetime, format = "%Y_%m_%d_%H:%M")]

  # Drop rows with NA (in case any file doesn't match expected format)
  dt <- dt[!is.na(datetime)]

  # Keep only the latest per (sim, seed)
  dt_latest <- dt[dt[, .I[which.max(datetime)], by = .(sim, seed)]$V1]

  # Optional: drop full path if not needed
  dt_latest[, path := NULL]

  scenario_num=which(c('alpha','beta','gamma','delta','epsilon','zeta','eta')==scenario)-1
  seed_num=seed #datatable non sense. Confusion with outside environment


  hps_mario <- fread( dt_latest[sim == scenario_num & seed == seed_num, file.path("~/modi_mount/ReSurv_cv_results/", file)])

  # Extract number of layers and neurons from the configuration with minimum Score
  best_config <- hps_mario[which.min(Score), .(number_of_layers, number_of_neurons)]
  num_layers <- best_config$number_of_layers
  num_neurons <- best_config$number_of_neurons


  reticulate::use_condaenv("/home/gabriele_pittarello_uniroma1_it/modi_mount/r_environ")

  input_data <- data_generator(
    random_seed = seed,
    scenario = scenario,
    time_unit =  1 / 360,
    years = 4,
    period_exposure  = 200
  )

  setDT(input_data)
  
  

  max_dp = max(input_data$AP)

  individual_data <- IndividualDataPP(input_data,
                                        id = "claim_number",
                                        categorical_features = c("business_use"),
                                        continuous_features = c("property_value","AP","age"),
                                        accident_period = "AP",
                                        calendar_period = "RP",
                                        input_time_granularity = "days",
                                        output_time_granularity = "months",
                                        years = 4,
                                        continuous_features_spline = NULL,
                                        calendar_period_extrapolation = FALSE)

  individual_data_q <- IndividualDataPP(input_data,
                                        id = "claim_number",
                                        categorical_features = c("business_use"),
                                        continuous_features = c("property_value","AP","age"),
                                        accident_period = "AP",
                                        calendar_period = "RP",
                                        input_time_granularity = "days",
                                      output_time_granularity = "quarters",
                                      years = 4,
                                      continuous_features_spline = NULL,
                                      calendar_period_extrapolation = FALSE)


  individual_data_y <- IndividualDataPP(input_data,
                                        id = "claim_number",
                                        categorical_features = c("business_use"),
                                        continuous_features = c("property_value","AP","age"),
                                        accident_period = "AP",
                                        calendar_period = "RP",
                                        input_time_granularity = "days",
                                      output_time_granularity = "years",
                                      years=4,
                                      continuous_features_spline=NULL,
                                      calendar_period_extrapolation=F)


  categorical_features = c("business_use")
  continuous_features = c("property_value","age")
  development_period = "DP"
  accident_period = "AP"
  id_col = "claim_number"




    # Chain - Ladder ----

  max_dp_i=max(individual_data$full.data$AP_i)
  conversion_factor=individual_data_q$conversion_factor
  true_output_cl <- individual_data_q$full.data %>%
    mutate(
      DP_rev_o = floor(max_dp_i * conversion_factor) -
        ceiling(
          DP_i * conversion_factor +
            ((AP_i - 1) %% (
              1 / conversion_factor
            )) * conversion_factor) + 1,
      AP_o = ceiling(AP_i * conversion_factor),
      TR_o= AP_o-1
    ) %>%
    filter(DP_rev_o <= TR_o) %>%
    mutate(DP_o = max(individual_data_q$training.data$DP_rev_o) - DP_rev_o + 1) %>%
    group_by(AP_o, DP_o, DP_rev_o) %>%
    summarize(I = sum(I), .groups = "drop") %>%
    filter(DP_rev_o > 0)

  latest_observed <- individual_data_q$training.data %>%
    filter(DP_rev_o >= TR_o) %>%
    mutate(DP_o = max(individual_data_q$training.data$DP_rev_o) - DP_rev_o + 1) %>%
    group_by(AP_o) %>%
    mutate(DP_max = max(DP_o)) %>%
    group_by(AP_o, DP_max) %>%
    summarize(I = sum(I), .groups = "drop")

  clmodel <- individual_data_q$training.data %>%
    mutate(DP_o = max(individual_data_q$training.data$DP_rev_o) - DP_rev_o + 1) %>%
    group_by(AP_o, DP_o) %>%
    summarize(I = sum(I), .groups = "drop") %>%
    group_by(AP_o) %>%
    arrange(DP_o) %>%
    mutate(I_cum = cumsum(I), I_cum_lag = lag(I_cum, default = 0)) %>%
    ungroup() %>%
    group_by(DP_o) %>%
    reframe(df = sum(I_cum * (
      AP_o <= max(individual_data_q$training.data$AP_o) - DP_o + 1
    )) /
      sum(I_cum_lag * (
        AP_o <= max(individual_data_q$training.data$AP_o) - DP_o + 1
      )), I = sum(I * (
        AP_o <= max(individual_data_q$training.data$AP_o) - DP_o
      ))) %>%
    mutate(DP_o_join = DP_o) %>%
    mutate(DP_rev_o = max(DP_o) - DP_o + 1)

  predictions <- expand.grid(AP_o = latest_observed$AP_o,
                             DP_o = clmodel$DP_o_join) %>%
    left_join(clmodel[, c("DP_o_join", "df")], by = c("DP_o" = "DP_o_join")) %>%
    left_join(latest_observed, by = "AP_o") %>%
    rowwise() %>%
    filter(DP_o > DP_max) %>%
    ungroup() %>%
    group_by(AP_o) %>%
    arrange(DP_o) %>%
    mutate(df_cum = cumprod(df)) %>%
    mutate(I_expected = I * df_cum - I * lag(df_cum, default = 1)) %>%
    select(DP_o, AP_o, I_expected)

  conversion_factor <- individual_data_q$conversion_factor


  score_total <- predictions  %>%
    inner_join(true_output_cl, by = c("AP_o", "DP_o")) %>%
    mutate(ave = I - I_expected, abs_ave = abs(ave)) %>%
    # from here it is reformulated for the are tot
    ungroup() %>%
    group_by(AP_o, DP_rev_o) %>%
    reframe(abs_ave = sum(abs(ave)), I = sum(I))

  are_tot_cl <- sum(score_total$abs_ave) / sum(score_total$I)

  ei_r_cl <- sum(predictions$I_expected)/ sum(score_total$I)


  score_total_diagonal <- predictions  %>%
    inner_join(true_output_cl, by = c("AP_o", "DP_o")) %>%
    mutate(RP_o=AP_o+DP_o-1) %>%
    group_by(RP_o)%>%
    reframe(I = sum(I),
            I_expected  = sum(I_expected ))

  ## quarterly

  are_cal_q_cl=sum(abs(score_total_diagonal$I-score_total_diagonal$I_expected))/sum(score_total_diagonal$I)

  actual_reps <- sum(score_total_diagonal$I)


  #yearly


  conversion_factor=individual_data_y$conversion_factor
  true_output_cl <- individual_data_y$full.data %>%
    mutate(
      DP_rev_o = floor(max_dp_i * conversion_factor) -
        ceiling(
          DP_i * conversion_factor +
            ((AP_i - 1) %% (
              1 / conversion_factor
            )) * conversion_factor) + 1,
      AP_o = ceiling(AP_i * conversion_factor),
      TR_o= AP_o-1
    ) %>%
    filter(DP_rev_o <= TR_o) %>%
    mutate(DP_o = max(individual_data_y$training.data$DP_rev_o) - DP_rev_o + 1) %>%
    group_by(AP_o, DP_o, DP_rev_o) %>%
    summarize(I = sum(I), .groups = "drop") %>%
    filter(DP_rev_o > 0)

  latest_observed <- individual_data_y$training.data %>%
    filter(DP_rev_o >= TR_o) %>%
    mutate(DP_o = max(individual_data_y$training.data$DP_rev_o) - DP_rev_o + 1) %>%
    group_by(AP_o) %>%
    mutate(DP_max = max(DP_o)) %>%
    group_by(AP_o, DP_max) %>%
    summarize(I = sum(I), .groups = "drop")

  clmodel <- individual_data_y$training.data %>%
    mutate(DP_o = max(individual_data_y$training.data$DP_rev_o) - DP_rev_o + 1) %>%
    group_by(AP_o, DP_o) %>%
    summarize(I = sum(I), .groups = "drop") %>%
    group_by(AP_o) %>%
    arrange(DP_o) %>%
    mutate(I_cum = cumsum(I), I_cum_lag = lag(I_cum, default = 0)) %>%
    ungroup() %>%
    group_by(DP_o) %>%
    reframe(df = sum(I_cum * (
      AP_o <= max(individual_data_y$training.data$AP_o) - DP_o + 1
    )) /
      sum(I_cum_lag * (
        AP_o <= max(individual_data_y$training.data$AP_o) - DP_o + 1
      )), I = sum(I * (
        AP_o <= max(individual_data_y$training.data$AP_o) - DP_o
      ))) %>%
    mutate(DP_o_join = DP_o) %>%
    mutate(DP_rev_o = max(DP_o) - DP_o + 1)

  predictions <- expand.grid(AP_o = latest_observed$AP_o,
                             DP_o = clmodel$DP_o_join) %>%
    left_join(clmodel[, c("DP_o_join", "df")], by = c("DP_o" = "DP_o_join")) %>%
    left_join(latest_observed, by = "AP_o") %>%
    rowwise() %>%
    filter(DP_o > DP_max) %>%
    ungroup() %>%
    group_by(AP_o) %>%
    arrange(DP_o) %>%
    mutate(df_cum = cumprod(df)) %>%
    mutate(I_expected = I * df_cum - I * lag(df_cum, default = 1)) %>%
    select(DP_o, AP_o, I_expected)

  conversion_factor <- individual_data_y$conversion_factor

  score_total_diagonal <- predictions  %>%
    inner_join(true_output_cl, by = c("AP_o", "DP_o")) %>%
    mutate(RP_o=AP_o+DP_o-1) %>%
    group_by(RP_o)%>%
    reframe(I = sum(I),
            I_expected  = sum(I_expected ))

  ## quarterly

  are_cal_y_cl=sum(abs(score_total_diagonal$I-score_total_diagonal$I_expected))/sum(score_total_diagonal$I)


  #MW models: mw monthly ----
  input_data <- individual_data$training.data
  setDT(input_data)

  input_data[,DP_o := max(DP_rev_o)-DP_rev_o +1]

  input_data<-input_data[,.SD,.SDcols = c("claim_number", categorical_features, continuous_features , "AP_o", "DP_o")]

  setnames(input_data, c("AP_o", "DP_o"),c("AP","DP"))

  input_data[,RP:=(DP+AP-1)]

  max_dp = max(input_data$AP)

  setDT(input_data)
  # input_data[,DP:=pmin(RP-AP+1,max_dp)]

  vec1 <- sort(unique(input_data$DP))
  vec2 <- 1:max_dp

  v3 <- setdiff(union(vec1, vec2), intersect(vec1, vec2))


  wrapped_data <- data_wrapper(data=input_data,
                               continuous_features=continuous_features,
                               categorical_features = categorical_features,
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

  wrapped_data<-wrapped_data[order(DP),.SD,by=c(categorical_features,
                                                continuous_features,
                                                accident_period)]

  dt_target <- mw18_datapp_y(wrapped_data,
                             development_time = development_period,
                             dt_counts = "dt_count"
  )

  columns<-  c(paste0("DP",1:max_dp))


  tmp = as.data.frame(t(apply(dt_target[,..columns],1,cumsum)))

  setDT(tmp)

  dt_target[,(columns):=tmp]

  dt_covariates <- mw18_datapp_x(dt_target,
                                 continuous_features=continuous_features,
                                 categorical_features = categorical_features)


  common_cols <- intersect(colnames(dt_covariates),
                           colnames(dt_target))


  setDT(dt_covariates)

  dt_target[, (common_cols):=dt_covariates[,..common_cols]]


  # fit the pm
  out_pm <- fit_proportional_model(data=dt_target,
                              max_dp = max_dp)


  cat("...I have now finished the PM...")

  # fit the nn moodel
  out_nn <- nn_model_fit_and_predict(dt_target,
                                     max_dp=max_dp,
                                     number_of_neurons=num_neurons,
                                     number_of_layers=num_layers,
                                     categorical_features=categorical_features,
                                     continuous_features=continuous_features)


  cat("...I have now finished the NNM...")

  # out_nn <- list(
  #   score_total=fread("C:\\Users\\pwt887\\Downloads\\mw_score_diagonal_scenario_1_seed_5_2025_04_19_01_12.csv"),
  #   score_diagonal=fread("C:\\Users\\pwt887\\Downloads\\mw_score_diagonal_scenario_1_seed_5_2025_04_19_01_12.csv")
  #
  # )



  lowert_dt <- pivot_longer( out_pm$lower_triangle, cols = starts_with("DP"), names_to = "DP", values_to = "expected_counts_pm", names_prefix = "DP" )  %>%  mutate(DP = as.numeric(DP)) %>%  group_by(AP) %>%  # Make sure we're properly sorted first
    arrange(AP, DP) %>% # Fill NAs with the most recent non-NA value
    fill(expected_counts_pm, .direction = "down") %>% # If there are still NA values at the beginning of groups, replace with 0
    mutate(expected_counts_pm = replace_na(expected_counts_pm, 0)) %>% ungroup()

  setDT(lowert_dt)

  lowert_dt[is.na(lowert_dt)] <- 0

  setorder(lowert_dt,AP,DP)
  lowert_dt[,expected_counts_pm:=(expected_counts_pm-lag(expected_counts_pm,default=0)),
            by=.(AP)]

  scoring_dt <- pivot_longer(
    out_nn$score_total,
    cols = starts_with("DP"),
    names_to="DP",
    values_to = "expected_counts",
    names_prefix = "DP"
  )
  setDT(scoring_dt)
  scoring_dt[,DP:=as.numeric(DP)]

  scoring_dt[,c("business_use"):=list(as.factor(business_use))]


  #we don't care about continuous features
  scoring_dt<-scoring_dt[,.(expected_counts = sum(expected_counts)),
             by=.(business_use,AP,DP)]



  true_dt <- individual_data$starting.data %>%
    mutate(AP_i=AP,
           DP_i=RP-AP+1,
           DP_rev_o =   floor(max(AP_i)*individual_data$conversion_factor)-ceiling(DP_i*individual_data$conversion_factor+((AP_i-1)%%(1/individual_data$conversion_factor))*individual_data$conversion_factor) +1,
           AP = ceiling(AP_i*individual_data$conversion_factor),
           DP = pmin(max(DP_rev_o)-DP_rev_o +1,48))

  setDT(true_dt)

  true_dt<-true_dt[,.(actual_counts=.N),.(AP,DP,business_use)]

  setorder(true_dt, business_use, AP, DP)


  true_dt<-true_dt[,.(actual_counts=actual_counts),.(AP,DP,business_use)]

  # true_dt <- pivot_longer(
  #   dt_target,
  #   cols = starts_with("DP"),
  #   names_to="DP",
  #   values_to = "actual_counts",
  #   names_prefix = "DP"
  # )

  setDT(true_dt)
  true_dt[,DP:=as.numeric(DP)]
  true_dt[,business_use:=as.factor(business_use)]

  # Total metrics ----

  scoring_dt <- merge(scoring_dt,true_dt,by=c("AP","DP",
                                              categorical_features))


  data.table::setkey(scoring_dt,NULL)

  setorder(scoring_dt,business_use,AP,DP)
  scoring_dt[,expected_counts:=(expected_counts-lag(expected_counts,default=0)),
             by=.(AP,business_use)]

  scoring_total<-scoring_dt %>%
    group_by(AP,DP) %>%
    arrange(AP,DP)%>%
    reframe(expected_counts  =sum(expected_counts),
            actual_counts=sum(actual_counts))


  scoring_dt <- merge(scoring_total,lowert_dt,by=c("AP","DP"),all.x = T,all.y = F)


  if(sum(is.na(scoring_dt))>0){
    scoring_dt[is.na(scoring_dt)] <- 0}

  scoring_dt<-scoring_dt%>%
    group_by(AP,DP) %>%
    reframe(expected_counts  =sum(expected_counts+expected_counts_pm),
            actual_counts=actual_counts)


  scoring_total <- scoring_dt%>%
    filter((AP+DP-1)>max_dp)%>%
         group_by(AP,DP) %>%            
    mutate(ave=abs(expected_counts-actual_counts))%>%
    reframe(abs_ave = sum(ave),
            I=sum(actual_counts))

  are_tot_mw <- sum(scoring_total$abs_ave) / actual_reps
saved_ap_1_counts <- (nrow(input_data[AP==1,]))
  predicted_reps_nn <- sum(out_nn$score_total[[paste0("DP",
                                               max_dp)]])-sum(unlist(out_pm$counts_at_diagonal))- saved_ap_1_counts 

  pick <- names(unlist(out_pm$predictions_at_runoff))

  predicted_reps_pm<-sum(unlist(out_pm$predictions_at_runoff))

  ei_r_mw <- (predicted_reps_nn+predicted_reps_pm)/actual_reps

  # are cal ----

  scoring_total_diagonal <- scoring_dt%>%
    filter((AP+DP-1)>max_dp)%>%
    mutate(RP=(AP+DP-1)) %>%
    group_by(RP)%>%
    reframe(ave=abs(sum(expected_counts-actual_counts)))


  are_cal_y_mw<-are_cal_q_mw <- sum(scoring_total_diagonal$ave)/ actual_reps



  # MW quarterly ----
  input_data <- data_generator(
    random_seed = seed,
    scenario = scenario,
    time_unit =  1 / 360,
    years = 4,
    period_exposure  = 200
  )

  setDT(input_data)
  
  

  individual_data <- IndividualDataPP(input_data,
                                      id = "claim_number",
                                      categorical_features = c("business_use"),
                                      continuous_features = c("property_value","AP","age"),
                                      accident_period = "AP",
                                      calendar_period = "RP",
                                      input_time_granularity = "days",
                                      output_time_granularity = "quarters",
                                      years = 4,
                                      continuous_features_spline = NULL,
                                      calendar_period_extrapolation = FALSE)

  input_data <- individual_data$training.data
  setDT(input_data)

  input_data[,DP_o := max(DP_rev_o)-DP_rev_o +1]

  categorical_features = c("business_use")
  continuous_features = c("property_value","age")
  development_period = "DP"
  accident_period = "AP"
  id_col = "claim_number"


  input_data <- individual_data$training.data
  setDT(input_data)

  input_data[,DP_o := max(DP_rev_o)-DP_rev_o +1]

  input_data<-input_data[,.SD,.SDcols = c("claim_number", categorical_features, continuous_features , "AP_o", "DP_o")]

  setnames(input_data, c("AP_o", "DP_o"),c("AP","DP"))

  input_data[,RP:=(DP+AP-1)]

  max_dp = max(input_data$AP)

  setDT(input_data)
  # input_data[,DP:=pmin(RP-AP+1,max_dp)]

  vec1 <- sort(unique(input_data$DP))
  vec2 <- 1:max_dp

  v3 <- setdiff(union(vec1, vec2), intersect(vec1, vec2))


  wrapped_data <- data_wrapper(data=input_data,
                               continuous_features=continuous_features,
                               categorical_features = categorical_features,
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

  wrapped_data<-wrapped_data[order(DP),.SD,by=c(categorical_features,
                                                continuous_features,
                                                accident_period)]

  dt_target <- mw18_datapp_y(wrapped_data,
                             development_time = development_period,
                             dt_counts = "dt_count"
  )

  columns<-  c(paste0("DP",1:max_dp))


  tmp = as.data.frame(t(apply(dt_target[,..columns],1,cumsum)))

  setDT(tmp)

  dt_target[,(columns):=tmp]

  dt_covariates <- mw18_datapp_x(dt_target,
                                 continuous_features=continuous_features,
                                 categorical_features = categorical_features)


  common_cols <- intersect(colnames(dt_covariates),
                           colnames(dt_target))


  setDT(dt_covariates)

  dt_target[, (common_cols):=dt_covariates[,..common_cols]]


  # fit the pm
  out_pm <- fit_proportional_model(data=dt_target,
                                   max_dp = max_dp)


  cat("...I have now finished the PM...")

  # fit the nn moodel
  out_nn <- nn_model_fit_and_predict(dt_target,
                                     max_dp=max_dp,
                                     number_of_neurons=num_neurons,
                                     number_of_layers=num_layers,
                                     categorical_features=categorical_features,
                                     continuous_features=continuous_features)


  cat("...I have now finished the NNM...")

  lowert_dt <- pivot_longer( out_pm$lower_triangle, cols = starts_with("DP"), names_to = "DP", values_to = "expected_counts_pm", names_prefix = "DP" )  %>%  mutate(DP = as.numeric(DP)) %>%  group_by(AP) %>%  # Make sure we're properly sorted first
    arrange(AP, DP) %>% # Fill NAs with the most recent non-NA value
    fill(expected_counts_pm, .direction = "down") %>% # If there are still NA values at the beginning of groups, replace with 0
    mutate(expected_counts_pm = replace_na(expected_counts_pm, 0)) %>% ungroup()

  setDT(lowert_dt)

  lowert_dt[is.na(lowert_dt)] <- 0

  setorder(lowert_dt,AP,DP)
  lowert_dt[,expected_counts_pm:=(expected_counts_pm-lag(expected_counts_pm,default=0)),
            by=.(AP)]

  scoring_dt <- pivot_longer(
    out_nn$score_total,
    cols = starts_with("DP"),
    names_to="DP",
    values_to = "expected_counts",
    names_prefix = "DP"
  )
  setDT(scoring_dt)
  scoring_dt[,DP:=as.numeric(DP)]

  scoring_dt[,c("business_use"):=list(as.factor(business_use))]


  #we don't care about continuous features
  scoring_dt<-scoring_dt[,.(expected_counts = sum(expected_counts)),
             by=.(business_use,AP,DP)]



  true_dt <- individual_data$starting.data %>%
    mutate(AP_i=AP,
           DP_i=RP-AP+1,
           DP_rev_o =   floor(max(AP_i)*individual_data$conversion_factor)-ceiling(DP_i*individual_data$conversion_factor+((AP_i-1)%%(1/individual_data$conversion_factor))*individual_data$conversion_factor) +1,
           AP = ceiling(AP_i*individual_data$conversion_factor),
           DP = pmin(max(DP_rev_o)-DP_rev_o +1,48))

  setDT(true_dt)

  true_dt<-true_dt[,.(actual_counts=.N),.(AP,DP,business_use)]

  setorder(true_dt, business_use, AP, DP)


  true_dt<-true_dt[,.(actual_counts=actual_counts),.(AP,DP,business_use)]

  # true_dt <- pivot_longer(
  #   dt_target,
  #   cols = starts_with("DP"),
  #   names_to="DP",
  #   values_to = "actual_counts",
  #   names_prefix = "DP"
  # )

  setDT(true_dt)
  true_dt[,DP:=as.numeric(DP)]
  true_dt[,business_use:=as.factor(business_use)]

  # Total metrics ----

  scoring_dt <- merge(scoring_dt,true_dt,by=c("AP","DP",
                                              categorical_features))


  data.table::setkey(scoring_dt,NULL)

  setorder(scoring_dt,business_use,AP,DP)
  scoring_dt[,expected_counts:=(expected_counts-lag(expected_counts,default=0)),
             by=.(AP,business_use)]

  scoring_total<-scoring_dt %>%
    group_by(AP,DP) %>%
    arrange(AP,DP)%>%
    reframe(expected_counts  =sum(expected_counts),
            actual_counts=sum(actual_counts))


  scoring_dt <- merge(scoring_total,lowert_dt,by=c("AP","DP"),all.x = T,all.y = F)


  if(sum(is.na(scoring_dt))>0){
    scoring_dt[is.na(scoring_dt)] <- 0}

  scoring_dt<-scoring_dt%>%
    group_by(AP,DP) %>%
    reframe(expected_counts  =sum(expected_counts+expected_counts_pm),
            actual_counts=actual_counts)


  scoring_total <- scoring_dt%>%
    filter((AP+DP-1)>max_dp)%>%
         group_by(AP,DP) %>%            
    mutate(ave=abs(expected_counts-actual_counts))%>%
    reframe(abs_ave = sum(ave),
            I=sum(actual_counts))

  are_tot_mw_Q <- sum(scoring_total$abs_ave) / actual_reps
saved_ap_1_counts <- (nrow(input_data[AP==1,]))
  predicted_reps_nn <- sum(out_nn$score_total[[paste0("DP",
                                               max_dp)]])-sum(unlist(out_pm$counts_at_diagonal))- saved_ap_1_counts 

  pick <- names(unlist(out_pm$predictions_at_runoff))

  predicted_reps_pm<-sum(unlist(out_pm$predictions_at_runoff))

  ei_r_mw_Q <- (predicted_reps_nn+predicted_reps_pm)/actual_reps

  # are cal ----

  scoring_total_diagonal <- scoring_dt%>%
    filter((AP+DP-1)>max_dp)%>%
    mutate(RP=(AP+DP-1)) %>%
    group_by(RP)%>%
    reframe(ave=abs(sum(expected_counts-actual_counts)))


  are_cal_y_mw_Q<-are_cal_q_mw_Q <- sum(scoring_total_diagonal$ave)/ actual_reps

                     
  # output

  out <- data.table(
    are_tot=c(are_tot_mw,
              are_tot_cl,
              are_tot_mw_Q),
    are_cal_q=c(are_cal_q_mw,
              are_cal_q_cl,
              are_cal_q_mw_Q),
    are_cal_y=c(are_cal_y_mw,
                are_cal_y_cl,
                are_cal_y_mw_Q),
    ei_r = c(ei_r_mw,
             ei_r_cl,
             ei_r_mw_Q),
    m_crps=c(NA,
             NA,
             NA),
    model=c("MW_months",
            "CL",
            "MW_quarters"),
    scenario=scenario,
    seed=seed )

  name = paste0("/home/gabriele_pittarello_uniroma1_it/modi_mount/Scoring/Scoring_results",
                "/sim_in_chunks_",
                scenario,
                "_seed_",
                seed,
                "_",
                format(Sys.time(), "%Y_%m_%d_%H:%M"),".csv")

  fwrite(out,
         name)


}


args = commandArgs(trailingOnly=TRUE)
set.seed(1964)
seeds <- 1:8


cl <- makeCluster(6)
random_seed=1964
objects_export <- list(
  "random_seed",
  "sim_fitting",
  "MinMaxScaler",
  "data_wrapper",
  "mw18_datapp_x",
  "mw18_datapp_y",
  "fit_proportional_model",
  "nn_model_fit_and_predict"
)
clusterExport(cl, objects_export, envir = environment())

clusterEvalQ(cl, {library("ReSurv")
  library("fastDummies")
  library("reticulate")
  library("data.table")
  library("dplyr")
  library("keras")
  library("reticulate")
  library("ChainLadder")
  library("clmplus")
  set.seed(random_seed)} )


parallel::parSapply(cl, seeds, FUN=sim_fitting, scenario = args[1])






