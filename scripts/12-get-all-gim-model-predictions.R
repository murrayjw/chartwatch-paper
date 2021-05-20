#'  ***********************************************************************
#'  ***********************************************************************
#'  ***********************************************************************
#'  Script name:
#'  Initially created by: 
#'  Date created: 
#'  Maintainer information: 
#'
#'  Script contents: Get model predictions for all encounters that were on
#'  GIM. Currently only looking at GIM encounters that also had clinician 
#'  predictions.
#'
#'  ***********************************************************************
#'  ***********************************************************************
#'  ***********************************************************************

source("./constants.R")
source("./scripts/helper-functions.R")
library(keyring)
library(dplyr)
library(chartwatch)
library(lubridate)

clinician_predictions <- read.csv(FINAL_PAPER_TEST_CLINICIAN_PREDICTIONS_FILENAME)

predictions_folder <- "/mnt/research/LKS-CHART/Projects/gim_ews_preassessment_project/data/prediction_frames/"
all_predictions <- list()
i <- 1
for (prediction_date in list.files(predictions_folder)) {
  
  folder_name <- file.path(predictions_folder, prediction_date)
  
  if (dir.exists(folder_name)) {
    ensemble_filename <- file.path(folder_name, 
                                   "time_aware_mars_scores_2021_0401.csv")
    if (file.exists(ensemble_filename)) {
      
      ensemble_predictions <- read.csv(ensemble_filename,
                                       stringsAsFactors  = FALSE) %>%
        dplyr::mutate(clinician_prediction = if_else(
          ENCOUNTER_NUM %in% clinician_predictions$ENCOUNTER_NUM, 1, 0
        )) %>%
        dplyr::mutate(timestamp = lubridate::ymd_hms(timestamp))
      
      ensemble_predictions_outcomes <- ensemble_predictions %>%
        # Filter to only keep latest predictions
        dplyr::filter(timestamp >= max(ensemble_predictions$timestamp) - lubridate::ddays(1) &
                        timestamp <= max(ensemble_predictions$timestamp)) 
      
      
      all_predictions[[i]] <- ensemble_predictions_outcomes
      i <- i + 1
    }
    
  }
  
  
}

all_model_predictions <- do.call(rbind, all_predictions)


# Get outcomes ------------------------------------------------------------

options("keyring_backend" = "file",
        stringsAsFactors = F)
keyring_unlock(keyring = "database",
               password = Sys.getenv("R_KEYRING_PASSWORD"))

con_edw <- chartdb::connect_edw(
  username = keyring::key_get(service = "EDW",
                              username = "EDW_USERNAME",
                              keyring = "database"),
  password = keyring::key_get(service = "EDW",
                              username = "EDW_PASSWORD",
                              keyring = "database"))

# This includes:
# - death on ward
# - transfer from ward to ICU
# - palliative transfer
# - palliative discharge
all_outcomes <- chartwatch::get_outcome_events(con = con_edw,
                                               encounter_vector = unique(all_model_predictions$ENCOUNTER_NUM))
discharge_dispositions <- chartwatch::get_discharge_disposition(con = con_edw, 
                                                                encounter_vector = unique(all_model_predictions$ENCOUNTER_NUM))
deaths_outside_ward <- discharge_dispositions %>%
  filter(outcome == "death") %>%
  filter(!ENCOUNTER_NUM %in% all_outcomes$ENCOUNTER_NUM) %>%
  select(ENCOUNTER_NUM, EVENT_TS = DISCHARGE_TS) %>%
  mutate(EVENT_TYPE = "Death outside ward")
inpatient_encounters <- dim_tbl(con_edw, "INPATIENT_ENCOUNTER_FACT") %>%
  dplyr::filter(ENCOUNTER_NUM %in% local(unique(all_model_predictions$ENCOUNTER_NUM))) %>%
  collect()
odbc::dbDisconnect(con_edw)

prediction_outcomes <- all_model_predictions %>%
  dplyr::mutate(ENCOUNTER_NUM = as.character(ENCOUNTER_NUM)) %>%
  dplyr::left_join(
    rbind(all_outcomes, deaths_outside_ward), by = "ENCOUNTER_NUM") %>%
  dplyr::mutate(
    timestamp = lubridate::ymd_hms(timestamp),
    EVENT_TS = lubridate::ymd_hms(EVENT_TS)
  ) %>%
  
  dplyr::filter(EVENT_TS >= timestamp) %>%
  
  # Keep most recent event, if there are multiple
  dplyr::group_by(ENCOUNTER_NUM, timestamp) %>%
  arrange(EVENT_TS) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  
  dplyr::select(
    ENCOUNTER_NUM, timestamp, EVENT_TS, EVENT_TYPE
  ) %>%
  filter(!is.na(EVENT_TYPE)) %>%
  
  # Exclude any events that happen after cutoff date 
  # (this is 30 days after end of testing period)
  dplyr::mutate(EVENT_TYPE_UPDATED = if_else(EVENT_TS >= CUTOFF_DATE, "Discharge", EVENT_TYPE))


all_model_predictions_outcome <- all_model_predictions %>%
  dplyr::mutate(ENCOUNTER_NUM = as.character(ENCOUNTER_NUM)) %>%
  dplyr::left_join(prediction_outcomes, by = c("ENCOUNTER_NUM", "timestamp")) %>%
  dplyr::mutate(
    outcome = if_else(is.na(EVENT_TYPE_UPDATED), 0, 1)
  ) %>%
  dplyr::mutate(
    diff = as.numeric(difftime(EVENT_TS, timestamp, units = "hours")),
    outcome_48 = case_when(
      is.na(EVENT_TS) ~ 0,
      as.numeric(difftime(EVENT_TS, timestamp, units = "hours")) <= 48 ~ 1,
      
      TRUE ~ 0)
  ) %>%
  dplyr::mutate(
    EVENT_TYPE_UPDATED = if_else(is.na(EVENT_TYPE_UPDATED), "Discharge", EVENT_TYPE)
  ) 



pROC::roc(all_model_predictions_outcome$outcome_48, all_model_predictions_outcome$score)
pROC::roc(all_model_predictions_outcome$outcome, all_model_predictions_outcome$score)

evaluate_threshold(all_model_predictions_outcome %>%
                     dplyr::mutate(.pred_1 = score,
                                   OUTCOME_ALL =outcome, 
                                   outcome_all_48 =outcome_48), thr = FINAL_PAPER_ENSEMBLE_THRESHOLD) %>%
  as.data.frame()


write.csv(all_model_predictions_outcome,
          row.names = FALSE,
          file = FINAL_PAPER_TEST_TIME_AWARE_PREDICTIONS_ALL_FILENAME)


filtered_predictions <- all_model_predictions_outcome %>%
  # If there are multiple entries per time window, only keep
  # the earliest one, as the later one would have been generated when re-creating
  # the timeline for visits > 40 days
  dplyr::group_by(ENCOUNTER_NUM, time_window) %>%
  dplyr::arrange(timestamp) %>%
  slice(1) %>%
  ungroup() %>%
  filter(time_window <= 120)

pROC::roc(filtered_predictions$outcome_48, filtered_predictions$score)
pROC::roc(filtered_predictions$outcome, filtered_predictions$score)

alarmed <- filtered_predictions %>%
  dplyr::filter(score >= FINAL_PAPER_ENSEMBLE_THRESHOLD) %>%
  group_by(ENCOUNTER_NUM) %>%
  summarize(timestamp = min(timestamp),
            outcome = outcome[1],
            outcome_48 = outcome_48[1],
            EVENT_TYPE = EVENT_TYPE[1])
evaluate_threshold(filtered_predictions %>%
                     dplyr::mutate(.pred_1 = score,
                                   OUTCOME_ALL =outcome, 
                                   outcome_all_48 =outcome_48), thr = FINAL_PAPER_ENSEMBLE_THRESHOLD)

daily_predictions <- all_model_predictions_outcome %>%
  group_by(as.Date(timestamp), ENCOUNTER_NUM) %>%
  arrange(desc(timestamp)) %>%
  slice(1) %>%
  ungroup()

pROC::roc(daily_predictions$outcome_48, daily_predictions$score)
pROC::roc(daily_predictions$outcome, daily_predictions$score)


evaluate_threshold(daily_predictions %>%
                     dplyr::mutate(.pred_1 = score,
                                   OUTCOME_ALL =outcome, 
                                   outcome_all_48 =outcome_48), thr = FINAL_PAPER_ENSEMBLE_THRESHOLD)

rbind(all_outcomes,
      deaths_outside_ward) %>%
  dplyr::filter(ENCOUNTER_NUM %in% clinician_predictions$ENCOUNTER_NUM) %>%
  group_by(ENCOUNTER_NUM, EVENT_TYPE) %>%
  slice(1) %>%
  ungroup() %>%
  count(EVENT_TYPE)

clinician_predictions %>%
  select(ENCOUNTER_NUM, death, icu, pal) %>%
  unique() %>%
  summarize(sum(death), sum(icu), sum(pal))

daily_all_model_predictions <- all_model_predictions_outcome %>%
  # Prediction happens around 15:00, keep timestamp that is closest
  # to time of prediction
  mutate(prediction_time = if_else(hour(timestamp) <= 15,
                                   as.Date(timestamp) + dhours(15),
                                   as.Date(timestamp) + ddays(1) + dhours(15))) %>%
  group_by(ENCOUNTER_NUM, prediction_time) %>%
  arrange(desc(timestamp)) %>%
  slice(1) %>%
  ungroup()

pROC::roc(daily_all_model_predictions$outcome_48, daily_all_model_predictions$score)
pROC::roc(daily_all_model_predictions$outcome, daily_all_model_predictions$score)


evaluate_threshold(daily_all_model_predictions %>%
                     dplyr::mutate(.pred_1 = score,
                                   OUTCOME_ALL =outcome, 
                                   outcome_all_48 =outcome_48), thr = FINAL_PAPER_ENSEMBLE_THRESHOLD)


all_model_predictions_outcome %>%
  group_by(ENCOUNTER_NUM) %>%
  summarize(
    outcome = max(outcome)
  ) %>%
  count(outcome)

# Sanity check train/validation -------------------------------------------


valid_predictions <- read.csv(FINAL_PAPER_VALID_TIME_AWARE_PREDICTIONS_FILENAME,
                              stringsAsFactors = FALSE)
daily_valid_predictions <- valid_predictions %>%
  group_by(ENCOUNTER_NUM, as.Date(timestamp)) %>%
  arrange(desc(timestamp)) %>%
  slice(1) %>%
  ungroup()
pROC::roc(daily_valid_predictions$outcome_all_48, daily_valid_predictions$.pred_1)

evaluate_threshold(daily_valid_predictions, FINAL_PAPER_ENSEMBLE_THRESHOLD)

evaluate_threshold(valid_predictions, FINAL_PAPER_ENSEMBLE_THRESHOLD)

quantile(valid_predictions$mars, probs = seq(0.9, 0.99, 0.01))
quantile(all_model_predictions_outcome$mars, probs = seq(0.9, 0.99, 0.01))

quantile(valid_predictions$.pred_1, probs = seq(0.9, 0.99, 0.01))
quantile(all_model_predictions_outcome$score, probs = seq(0.9, 0.99, 0.01))

valid_predictions %>%
  group_by(ENCOUNTER_NUM) %>%
  summarize(outcome = max(OUTCOME_ALL)) %>%
  ungroup() %>%
  count(outcome)
