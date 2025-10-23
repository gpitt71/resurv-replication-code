#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(tibble)
})

#' Read and summarize scoring results for the simulation study.
#'
#' The script searches for the temporary files produced by the scoring stage
#' (i.e. files starting with `sim_in_chunks_`) and retains the most recent
#' result for each combination of model, scenario and seed. The resulting table
#' is then enriched with the correction applied to the Mack chain ladder models
#' to make them comparable with the other approaches. Finally, summary tables
#' with the main performance metrics are exported to `CSV` files.
#'
#' The output directory can be provided as the first command line argument. If
#' it is not supplied, the script assumes it is executed from the repository
#' root and defaults to `Fitting_Scoring/Scoring_results`.
#'
args <- commandArgs(trailingOnly = TRUE)
results_dir <- if (length(args) >= 1) args[[1]] else "Fitting_Scoring/Scoring_results"

if (!dir.exists(results_dir)) {
  stop("Results directory not found: ", results_dir)
}

results_files <- list.files(
  results_dir,
  pattern = "^sim_in_chunks_.*\\\.csv$",
  full.names = FALSE
)

if (length(results_files) == 0L) {
  stop("No scoring files were found in ", results_dir, ".")
}

parsed_files <- tibble(org = results_files) %>%
  tidyr::extract(
    col = org,
    into = c("scenario", "seed", "timestamp"),
    regex = "^sim_in_chunks_([^_]+)_seed_([0-9]+)_(.*)\\.csv$",
    remove = FALSE
  ) %>%
  mutate(
    seed = as.integer(seed),
    date_parsed = as.POSIXct(timestamp, format = "%Y_%m_%d_%H_%M", tz = "UTC"),
    full_path = file.path(results_dir, org)
  )

if (any(is.na(parsed_files$date_parsed))) {
  stop("Failed to parse the timestamp of the following files: ",
       paste(parsed_files$org[is.na(parsed_files$date_parsed)], collapse = ", "))
}

data_list <- lapply(seq_len(nrow(parsed_files)), function(i) {
  dt <- fread(parsed_files$full_path[i])
  dt[, date_parsed := parsed_files$date_parsed[i]]
  dt
})

if (length(data_list) == 0L) {
  stop("The scoring files are empty.")
}

all_data <- rbindlist(data_list, fill = TRUE)

# Keep only the most recent result per (model, scenario, seed).
latest_data <- all_data[order(-date_parsed), .SD[1], by = .(model, scenario, seed)]

# Apply the correction to the Mack chain ladder models when available.
chain_ladder_models <- c("MW_months", "MW_quarters")
mw_models <- latest_data[model %in% chain_ladder_models]

apply_corrections <- function(mw_dt) {
  if (nrow(mw_dt) == 0L) {
    return(mw_dt)
  }

  suppressPackageStartupMessages(library(ReSurv))

  unique_pairs <- unique(mw_dt[, .(seed, scenario)])
  pairs_to_fix <- unique_pairs[scenario != "zeta"]

  if (nrow(pairs_to_fix) == 0L) {
    mw_dt[, `:=`(correct_amount = NA_real_, wrong_amount = NA_real_)]
    return(mw_dt)
  }

  get_correct_amount <- function(seed, scenario) {
    input_data <- data_generator(
      random_seed = seed,
      scenario = scenario,
      time_unit = 1 / 360,
      years = 4,
      period_exposure = 200
    )

    wrong_amount <- sum(input_data$RP > 1440)

    individual_data_q <- IndividualDataPP(
      input_data,
      id = "claim_number",
      categorical_features = c("claim_type"),
      continuous_features = c("AP"),
      accident_period = "AP",
      calendar_period = "RP",
      input_time_granularity = "days",
      output_time_granularity = "quarters",
      years = 4
    )

    max_dp_i <- max(individual_data_q$full.data$AP_i)
    conversion_factor <- individual_data_q$conversion_factor

    true_output_cl <- individual_data_q$full.data %>%
      mutate(
        DP_rev_o = floor(max_dp_i * conversion_factor) -
          ceiling(DP_i * conversion_factor +
                    ((AP_i - 1) %% (1 / conversion_factor)) * conversion_factor) + 1,
        AP_o = ceiling(AP_i * conversion_factor),
        TR_o = AP_o - 1
      ) %>%
      filter(DP_rev_o <= TR_o) %>%
      mutate(DP_o = max(individual_data_q$training.data$DP_rev_o) - DP_rev_o + 1) %>%
      group_by(AP_o, DP_o, DP_rev_o) %>%
      summarise(I = sum(I), .groups = "drop") %>%
      filter(DP_rev_o > 0)

    latest_observed <- individual_data_q$training.data %>%
      filter(DP_rev_o >= TR_o) %>%
      mutate(DP_o = max(individual_data_q$training.data$DP_rev_o) - DP_rev_o + 1) %>%
      group_by(AP_o) %>%
      mutate(DP_max = max(DP_o)) %>%
      group_by(AP_o, DP_max) %>%
      summarise(I = sum(I), .groups = "drop")

    clmodel <- individual_data_q$training.data %>%
      mutate(DP_o = max(individual_data_q$training.data$DP_rev_o) - DP_rev_o + 1) %>%
      group_by(AP_o, DP_o) %>%
      summarise(I = sum(I), .groups = "drop") %>%
      group_by(AP_o) %>%
      arrange(DP_o) %>%
      mutate(I_cum = cumsum(I), I_cum_lag = lag(I_cum, default = 0)) %>%
      ungroup() %>%
      group_by(DP_o) %>%
      reframe(
        df = sum(I_cum * (AP_o <= max(individual_data_q$training.data$AP_o) - DP_o + 1)) /
          sum(I_cum_lag * (AP_o <= max(individual_data_q$training.data$AP_o) - DP_o + 1)),
        I = sum(I * (AP_o <= max(individual_data_q$training.data$AP_o) - DP_o))
      ) %>%
      mutate(
        DP_o_join = DP_o,
        DP_rev_o = max(DP_o) - DP_o + 1
      )

    predictions <- expand.grid(AP_o = latest_observed$AP_o, DP_o = clmodel$DP_o_join) %>%
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

    score_total <- predictions %>%
      inner_join(true_output_cl, by = c("AP_o", "DP_o")) %>%
      mutate(ave = I - I_expected, abs_ave = abs(ave)) %>%
      ungroup() %>%
      group_by(AP_o, DP_rev_o) %>%
      reframe(abs_ave = sum(abs(ave)), I = sum(I))

    correct_amount <- sum(score_total$I)
    list(correct_amount = correct_amount, wrong_amount = wrong_amount)
  }

  corrections <- pairs_to_fix[, get_correct_amount(seed, scenario), by = .(seed, scenario)]

  mw_dt <- merge(mw_dt, corrections, by = c("seed", "scenario"), all.x = TRUE)

  scaling <- mw_dt$wrong_amount / mw_dt$correct_amount
  scaling[!is.finite(scaling)] <- NA_real_

  columns_to_fix <- c("are_tot", "are_cal_q", "are_cal_y")

  for (col in columns_to_fix) {
    if (col %in% names(mw_dt)) {
      mw_dt[[col]] <- ifelse(is.na(scaling), mw_dt[[col]], mw_dt[[col]] * scaling)
    }
  }

  mw_dt
}

mw_models_corrected <- apply_corrections(mw_models)

combined_data <- rbindlist(
  list(
    latest_data[!model %in% chain_ladder_models],
    mw_models_corrected
  ),
  use.names = TRUE,
  fill = TRUE
)

summary_case <- combined_data[, .(
  mean_ei_r = abs(1 - mean(ei_r, na.rm = TRUE)),
  sd_ei_r = sd(ei_r, na.rm = TRUE),
  mean_are_tot = mean(are_tot, na.rm = TRUE),
  sd_are_tot = sd(are_tot, na.rm = TRUE),
  mean_are_cal_q = mean(are_cal_q, na.rm = TRUE),
  sd_are_cal_q = sd(are_cal_q, na.rm = TRUE),
  mean_m_crps = mean(m_crps, na.rm = TRUE),
  sd_m_crps = sd(m_crps, na.rm = TRUE)
), by = .(model, scenario)]

summary_case_mw <- NULL
if (nrow(mw_models_corrected) > 0L) {
  summary_case_mw <- mw_models_corrected[, .(
    mean_ei_r = abs(1 - mean(ei_r, na.rm = TRUE)),
    sd_ei_r = sd(ei_r, na.rm = TRUE),
    mean_are_tot = mean(are_tot, na.rm = TRUE),
    sd_are_tot = sd(are_tot, na.rm = TRUE),
    mean_are_cal_q = mean(are_cal_q, na.rm = TRUE),
    sd_are_cal_q = sd(are_cal_q, na.rm = TRUE),
    mean_m_crps = mean(m_crps, na.rm = TRUE),
    sd_m_crps = sd(m_crps, na.rm = TRUE)
  ), by = .(model, scenario)]
}

timestamp <- format(Sys.time(), "%Y_%m_%d_%H_%M")

output_prefix <- file.path(results_dir, paste0("all_data_", timestamp))

fwrite(combined_data, paste0(output_prefix, ".csv"))

if (!is.null(summary_case_mw)) {
  fwrite(summary_case_mw, paste0(results_dir, "/summary_case_mw_", timestamp, ".csv"))
}

fwrite(summary_case, paste0(results_dir, "/summary_case_", timestamp, ".csv"))

message("Written combined results to ", paste0(output_prefix, ".csv"))
message("Written summary table to ", paste0(results_dir, "/summary_case_", timestamp, ".csv"))
if (!is.null(summary_case_mw)) {
  message("Written MW summary table to ", paste0(results_dir, "/summary_case_mw_", timestamp, ".csv"))
}
