#'  ***********************************************************************
#'  ***********************************************************************
#'  ***********************************************************************
#'  Script name:
#'  Initially created by: 
#'  Date created: 
#'  Maintainer information: 
#'
#'  Script contents: Previously only lookin g at model predictions at time of 
#'  clinician prediction. However, model is constantly running... could look at
#'  overnight predictions for example.
#'
#'  ***********************************************************************
#'  ***********************************************************************
#'  ***********************************************************************

source("./constants.R")
source("./scripts/helper-functions.R")

clinician_predictions <- read.csv(FINAL_PAPER_TEST_CLINICIAN_PREDICTIONS_FILENAME)

predictions_folder <- "/mnt/research/LKS-CHART/Projects/gim_ews_preassessment_project/data/prediction_frames/"
all_predictions <- list()
i <- 1
for (prediction_date in list.files(predictions_folder)) {

  folder_name <- file.path(predictions_folder, prediction_date)
  
  if (dir.exists(folder_name)) {
    ensemble_filename <- file.path(folder_name, 
                                   "time_aware_mars_scores_2021_0308.csv")
    if (file.exists(ensemble_filename)) {
      
      predicted_outcomes <- clinician_predictions %>%
        dplyr::filter(as.Date(date) == as.Date(prediction_date)) %>%
        dplyr::select(ENCOUNTER_NUM, outcome, outcome48, MRN, date) %>%
        unique()
      
      ensemble_predictions <- read.csv(ensemble_filename,
                                       stringsAsFactors  = FALSE) %>%
        dplyr::filter(ENCOUNTER_NUM %in% predicted_outcomes$ENCOUNTER_NUM) %>%
        dplyr::mutate(timestamp = lubridate::ymd_hms(timestamp))
      
      ensemble_predictions_outcomes <- ensemble_predictions %>%
        dplyr::filter(timestamp >= max(ensemble_predictions$timestamp) - lubridate::ddays(1) &
                        timestamp <= max(ensemble_predictions$timestamp))  %>%
        dplyr::left_join(predicted_outcomes, by = "ENCOUNTER_NUM")
      
      # Filter to only keep latest predictions
      all_predictions[[i]] <- ensemble_predictions_outcomes
      i <- i + 1
    }

  }
 

}

model_predictions <- do.call(rbind, all_predictions)
write.csv(model_predictions,
          row.names = FALSE,
          file = FINAL_PAPER_TEST_TIME_AWARE_PREDICTIONS_FULL_FILENAME)

pROC::roc(model_predictions$outcome48, model_predictions$score, plot = TRUE)

evaluate_threshold(model_predictions %>%
                     dplyr::mutate(.pred_1 = score,
                                   OUTCOME_ALL =outcome, 
                                   outcome_all_48 =outcome48), thr = FINAL_PAPER_ENSEMBLE_THRESHOLD)
