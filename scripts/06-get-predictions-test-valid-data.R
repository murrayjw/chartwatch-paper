#'  ***********************************************************************
#'  ***********************************************************************
#'  ***********************************************************************
#'  Script name:
#'  Initially created by: 
#'  Date created: March 1, 2021
#'  Maintainer information: 
#'
#'  Script contents: Get predictions for validation data. 
#'  Retrieve outcomes and format
#'  them so that they can be merged with the model predictions
#'
#'  ***********************************************************************
#'  ***********************************************************************
#'  ***********************************************************************

source("./constants.R")


# Load model files --------------------------------------------------------

ensemble_recipe <- readRDS(ENSEMBLE_MARS_RECIPE_FILENAME)
gim_model_logistic_final <- readRDS(ENSEMBLE_MARS_MODEL_FILENAME)



# Load valid data ---------------------------------------------------------

valid_encounters <- read.csv(FINAL_PAPER_VALID_ENCOUNTERS_FILENAME,
                                 stringsAsFactors = FALSE)


valid_mars_scores <- read.csv(file = ALL_MARS_SCORES_FILENAME,
                              stringsAsFactors = FALSE) %>% 
  dplyr::filter(ENCOUNTER_NUM %in% valid_encounters$ENCOUNTER_NUM) %>%
  dplyr::mutate(timestamp = lubridate::ymd_hms(timestamp)) %>%
  dplyr::rename(mars = score) %>%
  
  # If there are multiple predictions for the same timestamp, keep the one
  # with the earliest time_window (as this is the prediction that would have
  # been used to potentially alert)
  dplyr::group_by(ENCOUNTER_NUM, timestamp) %>%
  dplyr::arrange(time_window) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup()

valid_processed_mars_scores <- chartwatch::ensemble_mars_time_process(valid_mars_scores)


# Get valid data outcomes -------------------------------------------------

outcomes <- read.csv(
    "/mnt/research/LKS-CHART/Projects/gim_ews_project/data/retraining_2020_0120/train/outcome_timeseries.csv",
    stringsAsFactors = FALSE
  ) %>%
  dplyr::mutate(timestamp = lubridate::ymd_hms(timestamp))

encounter_level_outcomes <- outcomes %>%
  dplyr::filter(ENCOUNTER_NUM %in% valid_encounters$ENCOUNTER_NUM) %>%
  group_by(ENCOUNTER_NUM) %>%
  summarise(OUTCOME_TS = OUTCOME_TS[1],
            OUTCOME_ALL = max(OUTCOME_ALL),
            death = max(outcome_udeath_24),
            pal = max(outcome_pal_24),
            icu = max(outcome_icu_24)) %>%
  dplyr::ungroup()


valid_model_data <- valid_processed_mars_scores %>%
  dplyr::left_join(encounter_level_outcomes, by = c("ENCOUNTER_NUM")) %>%
  dplyr::mutate(timestamp = lubridate::ymd_hms(timestamp),
                OUTCOME_TS = lubridate::ymd_hms(OUTCOME_TS)) %>%
  dplyr::mutate(
    time_to_outcome = as.numeric(difftime(OUTCOME_TS, timestamp, units = "hours"))
  ) %>%
  dplyr::mutate(
    outcome_all_24 = if_else(OUTCOME_ALL == 1 & time_to_outcome <= 24, 1, 0),
    outcome_all_48 = if_else(OUTCOME_ALL == 1 & time_to_outcome <= 48, 1, 0),
    outcome_all_72 = if_else(OUTCOME_ALL == 1 & time_to_outcome <= 72, 1, 0),
    
    outcome_icu_24 = if_else(icu == 1 & time_to_outcome <= 24, 1, 0),
    outcome_icu_48 = if_else(icu == 1 & time_to_outcome <= 48, 1, 0),
    outcome_icu_72 = if_else(icu == 1 & time_to_outcome <= 72, 1, 0),
    
    outcome_death_24 = if_else(death == 1 & time_to_outcome <= 24, 1, 0),
    outcome_death_48 = if_else(death == 1 & time_to_outcome <= 48, 1, 0),
    outcome_death_72 = if_else(death == 1 & time_to_outcome <= 72, 1, 0),
    
    outcome_pal_24 = if_else(pal == 1 & time_to_outcome <= 24, 1, 0),
    outcome_pal_48 = if_else(pal == 1 & time_to_outcome <= 48, 1, 0),
    outcome_pal_72 = if_else(pal == 1 & time_to_outcome <= 72, 1, 0)
  ) 

valid_pred <- predict(gim_model_logistic_final, 
                     recipes::bake(ensemble_recipe, new_data = valid_model_data),
                     type = "prob") %>%
  bind_cols(valid_model_data)
roc_obj <- pROC::roc(valid_pred$outcome_all_48, valid_pred$.pred_1)
pROC::auc(roc_obj)

roc_obj <- pROC::roc(valid_pred$OUTCOME_ALL, valid_pred$.pred_1)
pROC::auc(roc_obj)

roc_obj <- pROC::roc(valid_pred$outcome_all_72, valid_pred$.pred_1)
pROC::auc(roc_obj)

write.csv(valid_pred, FINAL_PAPER_VALID_TIME_AWARE_PREDICTIONS_FILENAME,
          row.names = FALSE)
