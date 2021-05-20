source("./scripts/helper-functions.R")
source("constants.R")
library(dplyr)

compute_bootstraped_metrics <- function(df, n_bootstraps) {
  
  bootstrapped_metrics <- list()
  for (i in 1:n_bootstraps) {
    print(paste0("Computing bootstrap ", i, "/", n_bootstraps))
    encounter_nums <- unique(df$ENCOUNTER_NUM)
    
    # Sample encounters with replacement
    sampled_encounters <- sample(encounter_nums, 
                                 size = length(encounter_nums), 
                                 replace = T)
    sampled_data <- df %>%
      dplyr::filter(ENCOUNTER_NUM %in% sampled_encounters)
    encounters_count <- dplyr::tibble(sampled_encounters) %>%
      dplyr::count(sampled_encounters) %>%
      dplyr::rename(ENCOUNTER_NUM = sampled_encounters)
    
    # Bootstrap rows so that data from encounter shows up number of times
    # specified in encounters_count
    boostrapped_data <- sampled_data %>%
      dplyr::left_join(encounters_count, by = "ENCOUNTER_NUM")
    bootstrapped_data <- as.data.frame(
      lapply(boostrapped_data, rep, boostrapped_data$n)
    ) %>%
      dplyr::group_by(ENCOUNTER_NUM, timestamp) %>%
      dplyr::mutate(id = 1:dplyr::n()) %>%
      dplyr::mutate(new_encounter = paste(ENCOUNTER_NUM, id)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-ENCOUNTER_NUM) %>% 
      dplyr::rename(ENCOUNTER_NUM = new_encounter)
    
    bootstrapped_metrics[[i]] <- evaluate_threshold(bootstrapped_data, 
                                                    thr = FINAL_PAPER_ENSEMBLE_THRESHOLD) %>%
      as.data.frame()
    
  }
  
  all_bootstrapped_metrics <- do.call(rbind, bootstrapped_metrics)
  return(all_bootstrapped_metrics)
}

N_BOOTSTRAPS <- 10

train_predictions <- read.csv(FINAL_PAPER_TRAIN_TIME_AWARE_PREDICTIONS_FILENAME, 
                              stringsAsFactors = FALSE)
valid_predictions <- read.csv(FINAL_PAPER_VALID_TIME_AWARE_PREDICTIONS_FILENAME,
                              stringsAsFactors = FALSE)
test_predictions <- read.csv(FINAL_PAPER_TEST_TIME_AWARE_PREDICTIONS_FILENAME,
                             stringsAsFactors = FALSE)

train_all_metrics <- compute_bootstraped_metrics(train_predictions,
                                                 n_bootstraps = N_BOOTSTRAPS)
valid_all_metrics <- compute_bootstraped_metrics(valid_predictions,
                                                 n_bootstraps = N_BOOTSTRAPS)
test_all_metrics <- compute_bootstraped_metrics(test_predictions,
                                                n_bootstraps = N_BOOTSTRAPS)

# Save all metrics --------------------------------------------------------
write.csv(train_all_metrics, 
          file = FINAL_PAPER_TRAIN_ALL_METRICS_FILENAME, 
          append = TRUE, row.names = FALSE)
write.csv(valid_all_metrics,
          file = FINAL_PAPER_VALID_ALL_METRICS_FILENAME, 
          append = TRUE, row.names = FALSE)
write.csv(test_all_metrics,
          file = FINAL_PAPER_TEST_ALL_METRICS_FILENAME, 
          append = TRUE, row.names = FALSE)
