
evaluate_threshold <- function(df, thr) {
  n <- length(unique(df$ENCOUNTER_NUM))
  alarms <- df %>%
    dplyr::filter(.pred_1 >= thr) %>%
    dplyr::group_by(ENCOUNTER_NUM) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()
  
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
