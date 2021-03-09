#'  ***********************************************************************
#'  ***********************************************************************
#'  ***********************************************************************
#'  Script name:
#'  Initially created by: 
#'  Date created: March 8, 2021
#'  Maintainer information: 
#'
#'  Script contents: Get time-aware MARS predictions for prospective test set.
#'  Previous way of computing was incorrect bc the input MARS scores had been
#'  pre-filtered --> this is WRONG bc time-aware MARS needs past predictions
#'  to compute related features (e.g., % change from baseline)
#'
#'  ***********************************************************************
#'  ***********************************************************************
#'  ***********************************************************************

library(chartwatch)
library(dplyr)
source("constants.R")

ensemble_recipe <- readRDS(ENSEMBLE_MARS_RECIPE_FILENAME)
ensemble_model <- readRDS(ENSEMBLE_MARS_MODEL_FILENAME)

test_encounters <- read.csv(FINAL_PAPER_TEST_ENCOUNTERS_FILENAME,
                            stringsAsFactors = FALSE)
clinician_predictions <- read.csv(FINAL_PAPER_TEST_CLINICIAN_PREDICTIONS_FILENAME,
                                  stringsAsFactors = FALSE) %>%
  dplyr::filter(ENCOUNTER_NUM %in% test_encounters$ENCOUNTER_NUM)

predictions_folder <- "/mnt/research/LKS-CHART/Projects/gim_ews_preassessment_project/data/prediction_frames/"
all_predictions <- list()
i <- 1
for (prediction_date in unique(clinician_predictions$date)) {
  predictions_encounters <- clinician_predictions %>%
    dplyr::filter(date == prediction_date)
  predictions_filename <- file.path(predictions_folder, prediction_date,
                                    "mars_scores_2021_0222.csv")
  mars_predictions <- read.csv(predictions_filename,
                          stringsAsFactors = FALSE) %>%
    rename(mars = score)
  
  # Extract all time-aware MARS model predictions
  ensemble_data <- chartwatch::ensemble_mars_time_process(mars_predictions)
  ensemble_predictions <- chartwatch::ensemble_mars_time_predict(ensemble_data,
                                                                 ensemble_recipe,
                                                                 ensemble_model)
  write.csv(ensemble_predictions,
            file = file.path(predictions_folder, prediction_date, 
                             "time_aware_mars_scores_2021_0308.csv"),
            row.names = FALSE)
  chartwatch_predictions <- ensemble_predictions %>%
    dplyr::mutate(timestamp = lubridate::ymd_hms(timestamp)) %>%
    dplyr::filter(ENCOUNTER_NUM %in% predictions_encounters$ENCOUNTER_NUM) %>%
    dplyr::group_by(ENCOUNTER_NUM) %>%
    dplyr::arrange(desc(timestamp)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(prediction_date = prediction_date)
  
  all_predictions[[i]] <- chartwatch_predictions
  i <- i + 1
  
  if (nrow(chartwatch_predictions) != length(unique(predictions_encounters$ENCOUNTER_NUM))) {
    print(paste0("Missing predictions in ", predictions_filename))
  }
}

model_predictions <- do.call(rbind, all_predictions)


updated_test_predictions <- model_predictions %>% 
  rename(model_timestamp = timestamp) %>%
  dplyr::left_join(clinician_predictions, 
                   by = c("ENCOUNTER_NUM", "prediction_date" = "date")) %>%
  
  # Use outcomes from clinician dataset
  dplyr::mutate(outcome_all_48 = outcome48,
                OUTCOME_ALL = OUTCOME,
                .pred_1 = score) %>%
  
  # Only need 1 prediction/model
  dplyr::select(-professional_role, -clinicianid) %>%
  unique()

write.csv(updated_test_predictions,
          file = FINAL_PAPER_TEST_TIME_AWARE_PREDICTIONS_FILENAME,
          row.names = FALSE)

# sanity check
evaluate_threshold(updated_test_predictions, FINAL_PAPER_ENSEMBLE_THRESHOLD) %>%
  as.data.frame()
