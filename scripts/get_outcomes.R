source("constants.R")

all_mars_scores <- read.csv(file = ALL_MARS_SCORES_FILENAME,
                            stringsAsFactors = FALSE)
train_encounters <- read.csv(file = PAPER_TRAIN_ENCOUNTERS_FILENAME, 
                             stringsAsFactors = FALSE)
valid_encounters <- read.csv(file = PAPER_VALID_ENCOUNTERS_FILENAME,
                             stringsAsFactors = FALSE)
test_encounters <- read.csv(file = PAPER_TEST_ENCOUNTERS_FILENAME,
                            stringsAsFactors = FALSE) %>%
  dplyr::mutate(
    OUTCOME_ALL = if_else(outcome_type_updated == "Discharge", 0, 1)
  )

compute_auc <- function(data) {
  roc_obj <- pROC::roc(data$OUTCOME_ALL, data$score)
  return(
    list(
      "AUC" = pROC::auc(roc_obj),
      "AUC_CI" = pROC::ci.auc(roc_obj)
    )
  )
}

train_results <- compute_auc(all_mars_scores %>%
                                 filter(ENCOUNTER_NUM %in% train_encounters$ENCOUNTER_NUM) %>%
                                 dplyr::left_join(train_encounters, by = "ENCOUNTER_NUM"))

compute_auc(all_mars_scores %>%
              filter(ENCOUNTER_NUM %in% valid_encounters$ENCOUNTER_NUM) %>%
              dplyr::left_join(valid_encounters, by = "ENCOUNTER_NUM"))

compute_auc(all_mars_scores %>%
              filter(ENCOUNTER_NUM %in% test_encounters$ENCOUNTER_NUM) %>%
              dplyr::left_join(test_encounters, by = "ENCOUNTER_NUM"))
