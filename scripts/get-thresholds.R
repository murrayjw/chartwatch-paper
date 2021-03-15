#'  ***********************************************************************
#'  ***********************************************************************
#'  ***********************************************************************
#'  Script name:
#'  Initially created by: 
#'  Date created: Feb 25, 2021
#'  Maintainer information: 
#'
#'  Script contents: Pick threshold which gives 40% PPV
#'  
#'  Using the train data, apply 10-fold CV:
#'  - For every threshold, compute metrics on each CV-fold
#'  - Pick thr which gives avg PPV of 40%
#'
#'  ***********************************************************************
#'  ***********************************************************************
#'  ***********************************************************************

source('constants.R')
source('scripts/helper-functions.R')

get_threshold_results <- function(df) {
  results <- list()
  i <- 1
 for (thr in seq(0.001, 0.999, 0.001)) {
    metrics <- evaluate_threshold(df, thr)
    
    
    results[[i]] <- dplyr::tibble(
      thr = thr,
      ppv = metrics$ppv,
      npv = metrics$npv,
      sens = metrics$sens,
      spec = metrics$spec,
      accuracy = metrics$accuracy
    )
    
    i <- i + 1
    
  }
  
  return(do.call(rbind, results))
}

pick_threshold <- function(df, ppv_min = 0.4, ppv_max = 0.45) {
  selected <- df %>%
    dplyr::mutate(ppv = as.numeric(ppv),
                  sens = as.numeric(sens)) %>%
    dplyr::filter(ppv >= ppv_min & ppv <= ppv_max) %>%
    dplyr::arrange(desc(sens)) %>%
    dplyr::slice(1) 
  
  return(selected$thr)
} 


predictions <- read.csv(FINAL_PAPER_TRAIN_TIME_AWARE_PREDICTIONS_FILENAME,
                        stringsAsFactors = FALSE)
cv_splits <- rsample::group_vfold_cv(data = predictions, group = "ENCOUNTER_NUM",
                                     v = 10)

cv_threshold_eval <- list()
# For every CV split
for (i in 1:10) {
  print(paste0("split ", i))
  cv_split <- cv_splits[[1]][[i]]
  analysis_set <- rsample::analysis(cv_split)
  assessment_set <- rsample::assessment(cv_split)
  
  # Pick the threshold which gives the highest sens with a min cutoff of 
  # encounter-level PPV = 0.40
  analysis_thresholds <- get_threshold_results(analysis_set)
  selected_threshold <- pick_threshold(analysis_thresholds)
  print(paste0("selected threshold: ", selected_threshold))
  
  # Evaluate the chosen threshold on the heldout/assessment CV split
  evaluation_metrics <- evaluate_threshold(assessment_set, 
                                           thr = selected_threshold)
  print(paste0("sensitivity: ", round(evaluation_metrics$sens, digits = 2), 
               " - ppv: ", round(evaluation_metrics$ppv, digits = 2),
               " -  accuracy: ", round(evaluation_metrics$accuracy, digits = 2)))
  
  # Save all results here
  cv_threshold_eval[[i]] <- dplyr::tibble(
    i = i,
    thr = selected_threshold,
    ppv = evaluation_metrics$ppv,
    npv = evaluation_metrics$npv,
    sens = evaluation_metrics$sens,
    spec = evaluation_metrics$spec,
    accuracy = evaluation_metrics$accuracy
  )
  
}

all_cv_threshold_eval <- do.call(rbind, cv_threshold_eval)
mean(all_cv_threshold_eval$thr)
# THRESHOLD = 0.17 --> PPV of 30%
# THRESHOLD = 0.2215 --> PPV of 40%
