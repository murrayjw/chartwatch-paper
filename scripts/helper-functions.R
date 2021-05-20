
evaluate_threshold <- function(df, thr) {
  n <- length(unique(df$ENCOUNTER_NUM))
  alarms <- df %>%
    dplyr::filter(.pred_1 >= thr) %>%
    dplyr::group_by(ENCOUNTER_NUM) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    mutate(tte = as.numeric(difftime(EVENT_TS, timestamp, unit  = "hours")))
  
  tp <- sum(alarms$OUTCOME_ALL)
  fp <- alarms %>% dplyr::filter(OUTCOME_ALL == 0) %>% nrow()
  fn <- df %>%
    dplyr::filter(OUTCOME_ALL == 1) %>%
    dplyr::filter(!ENCOUNTER_NUM %in% alarms$ENCOUNTER_NUM) %>%
    dplyr::pull(ENCOUNTER_NUM) %>%
    unique() %>%
    length()
  tn <- df %>%
    dplyr::filter(OUTCOME_ALL == 0) %>%
    dplyr::filter(!ENCOUNTER_NUM %in% alarms$ENCOUNTER_NUM) %>%
    dplyr::pull(ENCOUNTER_NUM) %>%
    unique() %>%
    length()
  
  tp_48 <- sum(alarms$outcome_all_48)
  fp_48 <- alarms %>%
    dplyr::filter(outcome_all_48 == 0) %>% 
    nrow()
  fn_48 <- df %>%
    dplyr::filter(outcome_all_48 == 1) %>%
    dplyr::filter(!ENCOUNTER_NUM %in% alarms$ENCOUNTER_NUM) %>%
    dplyr::pull(ENCOUNTER_NUM) %>%
    unique() %>%
    length()
  tn_48 <- df %>%
    dplyr::filter(outcome_all_48 == 0) %>%
    dplyr::filter(!ENCOUNTER_NUM %in% alarms$ENCOUNTER_NUM) %>%
    dplyr::pull(ENCOUNTER_NUM) %>%
    unique() %>%
    length()
  
  
  # Encounter-level metrics -------------------------------------------------
  
  tte_median <- median(alarms$tte, na.rm = TRUE)
  tte_iqr <- IQR(alarms$tte, na.rm = TRUE)
  ppv <- tp / (tp + fp)
  npv <- tn / (tn + fn)
  sens <- tp / (tp + fn)
  spec <- tn / (tn + fp)
  accuracy <- (tp + tn) / (tp + tn + fp + fn)
  if (n_distinct(df$OUTCOME_ALL) == 2) {
    auc <- pROC::auc(df$OUTCOME_ALL, df$.pred_1, quiet = TRUE) 
  } else {
    auc <- NA
  }
  f1 <- tp / (tp + (0.5 * (fp + fn)))
  pos_lr <- sens / (1 - spec)
  neg_lr <- (1  - sens) / spec
  
  
  # Metrics for next 48 hours -----------------------------------------------
  
  ppv_48 <- tp_48 / (tp_48 + fp_48)
  npv_48 <- tn_48 / (tn_48 + fn_48)
  sens_48 <- tp_48 / (tp_48 + fn_48)
  spec_48 <- tn_48 / (tn_48 + fp_48)
  accuracy_48 <- (tp_48 + tn_48) / (tp_48 + tn_48 + fp_48 + fn_48)
  if (n_distinct(df$outcome_all_48) == 2) {
    auc_48 <- pROC::auc(df$outcome_all_48, df$.pred_1, quiet = TRUE)
  } else {
    auc_48 <- NA
  }
  
  f1_48 <- tp_48 / (tp_48 + (0.5 * (fp_48 + fn_48)))
  
  return(
    list(
      ppv = ppv,
      npv = npv,
      sens = sens,
      spec = spec,
      accuracy = accuracy,
      auc = auc,
      f1 = f1,
      pos_lr = pos_lr,
      neg_lr = neg_lr,
      tte_median = tte_median,
      tte_iqr = tte_iqr,
      
      ppv_48 = ppv_48,
      npv_48 = npv_48,
      sens_48 = sens_48,
      spec_48 = spec_48,
      accuracy_48 = accuracy_48,
      auc_48 = auc_48,
      f1_48 = f1_48
    )
  )
}


get_metrics <- function(df, model_df, 
                        outcomes, 
                        primary_outcome = c("ICU", "Death", "Death outside ward"), 
                        iterations = 10) {
  
  # Get outcomes
  outcomes_df <- outcomes %>%
    filter(EVENT_TYPE_UPDATED %in% primary_outcome) %>%
    mutate(primary_outcome = 1,
           EVENT_TS = ymd_hms(EVENT_TS))
  # Extract 1 outcome/encounter, keeping most recent one when multiple are available
  encounter_level_outcomes <- df %>%
    left_join(outcomes_df, by = "ENCOUNTER_NUM") %>%
    mutate(diff = as.numeric(difftime(EVENT_TS, timestamp, unit = "hours"))) %>%
    filter(diff >= 0) %>%
    group_by(ENCOUNTER_NUM) %>%
    arrange(diff) %>%
    slice(1) %>%
    ungroup() %>%
    select(ENCOUNTER_NUM, EVENT_TYPE_UPDATED, EVENT_TS, primary_outcome)
  
  
  # Prediction-level info
  n_predictions <- nrow(df)
  n_predictions_per_clinician <- df %>%
    count(clinicianid) %>%
    summarize(
      `# predictions/clinician - Mean (SD)` = paste0(
        round(mean(n), digits = 2),
        " (",
        round(sd(n), digits = 2), 
        ")"
      ),
      `# predictions/clinician - Median (IQR)` = paste0(
        round(median(n), digits = 2),
        " (",
        round(IQR(n), digits = 2),
        ")"
      )
    )
  
  
  # Encounter-level info ----------------------------------------------------
  encounters <- df %>%
    select(ENCOUNTER_NUM) %>%
    unique() %>%
    left_join(encounter_level_outcomes, by = "ENCOUNTER_NUM")
  n_outcomes <- encounters %>%
    summarize(
      `# encounters` =  n(),
      `Primary outcome - N (%)` = paste0(
        sum(primary_outcome, na.rm = TRUE),
        " (",
        round(100 * sum(primary_outcome, na.rm = TRUE) / n(), digits = 2), 
        "%)"
      )
    )
  encounters_per_clinician <- df %>%
    count(ENCOUNTER_NUM, clinicianid) %>%
    count(clinicianid) %>%
    summarize(
      `# encounters/clinician - Mean (SD)` = paste0(
        round(mean(n), digits = 2),
        " (",
        round(sd(n), digits = 2), 
        ")"
      ),
      `# encounters/clinician - Median (IQR)` = paste0(
        round(median(n), digits = 2),
        " (",
        round(IQR(n), digits = 2),
        ")"
      )
    )
  
  
  
  # Compute results ---------------------------------------------------------
  
  clinician_results <- list()
  model_results <- list()
  for (i in 1:iterations) {
    # Sample 1 prediction/encounter at random
    clinician_evaluation_df <- df %>%
      group_by(ENCOUNTER_NUM) %>%
      sample_n(1) %>%
      ungroup() %>%
      left_join(encounter_level_outcomes, by = "ENCOUNTER_NUM") %>%
      mutate(primary_outcome = if_else(is.na(primary_outcome), 0, primary_outcome))
    
    # Get corresponding model prediction
    model_evaluation_df <- model_df %>%
      filter(clinician_prediction == 1) %>%
      left_join(clinician_evaluation_df, by = "ENCOUNTER_NUM") %>%
      
      # Keep model prediction that is closest in time to clinician prediction
      mutate(diff = as.numeric(difftime(timestamp, model_ts, unit = "hours"))) %>%
      filter(diff >= 0) %>%
      group_by(ENCOUNTER_NUM) %>%
      arrange(diff) %>%
      slice(1) %>%
      ungroup() %>%
      select(ENCOUNTER_NUM, model_ts, score) %>%
      left_join(encounter_level_outcomes, by = "ENCOUNTER_NUM") %>%
      mutate(primary_outcome = if_else(is.na(primary_outcome), 0, primary_outcome))
    
    clinician_results[[i]] <- evaluate_threshold(clinician_evaluation_df %>%
                                                   mutate(.pred_1 = ever,
                                                          OUTCOME_ALL = primary_outcome,
                                                          outcome_all_48 = NA), thr = 1) %>%
      as.data.frame() %>%
      select(-contains("48"))
    
    model_results[[i]] <- evaluate_threshold(model_evaluation_df %>%
                                               mutate(.pred_1 = score,
                                                      OUTCOME_ALL = primary_outcome,
                                                      outcome_all_48 = NA,
                                                      timestamp = model_ts), thr = ENSEMBLE_THRESHOLD_PPV30) %>%
      as.data.frame() %>%
      select(-contains("48"))
  }
  
  all_clinician_results <- do.call(rbind, clinician_results)
  all_model_results <- do.call(rbind, model_results)
  
  clinician_metrics <- all_clinician_results %>% 
    summarize_all(.funs = list( 
      "mean" = function(x) {paste0(
        round(mean(x), digits = 2),
        " (",
        round(sd(x), digits = 2),
        ")"
      )},
      "median" = function(x) {paste0(
        round(median(x), digits = 2),
        " (",
        round(IQR(x), digits = 2),
        ")"
      )})
    )
  
  model_metrics <- all_model_results %>% 
    summarize_all(.funs = list( 
      "mean" = function(x) {paste0(
        round(mean(x, na.rm = TRUE), digits = 2),
        " (",
        round(sd(x, na.rm = TRUE), digits = 2),
        ")"
      )},
      "median" = function(x) {paste0(
        round(median(x, na.rm = TRUE), digits = 2),
        " (",
        round(IQR(x, na.rm = TRUE), digits = 2),
        ")"
      )})
    )
  
  data_info <- cbind(
    n_outcomes,
    `# Predictions` = n_predictions,
    n_predictions_per_clinician,
    encounters_per_clinician,
    `# Clinicians` = length(unique(df$clinicianid))
  )
  
  res <- list(
    "data" = data_info,
    "model" = all_model_results,
    "model_metrics" = model_metrics,
    "clinician" = all_clinician_results,
    "clinician_metrics" = clinician_metrics
  )
  
  return(res)
}
