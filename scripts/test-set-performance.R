library(ggplot2)
library(lubridate)
library(dplyr)
source("scripts/helper-functions.R")

CLINICIAN_THRESHOLD <- 1

test_chartwatch_predictions <- read.csv(FINAL_PAPER_TEST_TIME_AWARE_PREDICTIONS_FILENAME, 
                                        stringsAsFactors = FALSE)
test_clinician_predictions <- read.csv(FINAL_PAPER_TEST_CLINICIAN_PREDICTIONS_FILENAME, 
                                       stringsAsFactors = FALSE) %>%
  dplyr::filter(ENCOUNTER_NUM %in% test_chartwatch_predictions$ENCOUNTER_NUM) %>%
  replace(is.na(.), 0)


# Evaluate chartwatch model -----------------------------------------------

chartwatch_model <- evaluate_threshold(test_chartwatch_predictions, 
                   FINAL_PAPER_ENSEMBLE_THRESHOLD) %>%
  as.data.frame()

chartwatch_threshold_results <- list()
i <- 1
for (thr in seq(0.01, 0.99, 0.01)) {
  chartwatch_threshold_results[[i]] <- evaluate_threshold(test_chartwatch_predictions, thr) %>%
    as.data.frame()
  i <- i + 1
}
all_chartwatch_threshold_results <- do.call(rbind, chartwatch_threshold_results)


# Evaluate clinician models -----------------------------------------------

# Overall
all_clinician_predictions <- test_clinician_predictions %>%
  select(ENCOUNTER_NUM, .pred_1 = ever, OUTCOME_ALL = outcome, outcome_all_48 = outcome48, professional_role)
physician_predictions <- all_clinician_predictions %>%
  dplyr::filter(grepl("Physician|Pyysician|Phsycian", professional_role))
resident_predictions <- all_clinician_predictions %>%
  dplyr::filter(grepl("Resident", professional_role))
nurse_predictions <- all_clinician_predictions %>%
  dplyr::filter(grepl("Nurse", professional_role))
  
all_clinician_results <- evaluate_threshold(all_clinician_predictions, 
                                            thr = CLINICIAN_THRESHOLD) %>%
  as.data.frame()
evaluate_threshold(physician_predictions, thr = CLINICIAN_THRESHOLD) %>%
  as.data.frame()
evaluate_threshold(resident_predictions, thr = CLINICIAN_THRESHOLD) %>%
  as.data.frame()
evaluate_threshold(nurse_predictions, thr = CLINICIAN_THRESHOLD) %>%
  as.data.frame()

# At individual level
individual_clinician_results <- list()
i <- 1
for (c_id in unique(test_clinician_predictions$clinicianid)) {
  clinician_predictions <- test_clinician_predictions %>%
    dplyr::filter(clinicianid == c_id) %>% 
    select(ENCOUNTER_NUM, .pred_1 = ever_conf, OUTCOME_ALL = outcome, 
           outcome_all_48 = outcome48, professional_role)
  individual_clinician_results[[i]] <- evaluate_threshold(clinician_predictions, 
                                                          thr = CLINICIAN_THRESHOLD) %>%
    as.data.frame() %>% 
    cbind(dplyr::tibble(
      "clinicianid" = c_id,
      "professional_role" = clinician_predictions$professional_role[1],
      n = nrow(clinician_predictions)
    ))
  i <- i + 1
}
all_individual_clinician_results <- do.call(rbind, individual_clinician_results)

all_individual_clinician_results %>%
  ggplot(aes(x = 1 - spec, y = sens, color = professional_role)) + 
  geom_point() + 
  geom_abline(slope=1, intercept = 0) 

all_chartwatch_threshold_results %>%
  ggplot(aes(x = 1 - spec, y = sens)) + 
  geom_line() + 
  geom_point() + 
  geom_abline(slope=1, intercept = 0) 

clinician_results_plot <- all_individual_clinician_results %>%
  dplyr::mutate(clinician_group = case_when(grepl("Physician|Pyysician|Phsycian", professional_role) ~ "physician",
                                  grepl("Resident", professional_role) ~ "resident",
                                  grepl("Nurse", professional_role) ~ "nurse")) %>%
  dplyr::mutate(group = "clinician") %>%
  dplyr::select(clinician_sens = sens, clinician_spec = spec, clinician_group, group) %>%
  dplyr::mutate(model_sens = NA, model_spec = NA)
 
model_results_plot <- all_chartwatch_threshold_results %>%
  dplyr::select(model_sens = sens, model_spec = spec) %>%
  dplyr::mutate(clinician_group = NA,
                group = "model", 
                clinician_sens = NA,
                clinician_spec = NA) 

rbind(clinician_results_plot, model_results_plot) %>%
  ggplot() +
  geom_point(aes(x = 1 - clinician_spec, y = clinician_sens, color = clinician_group)) +
  geom_line(aes(x = 1 - model_spec, y = model_sens))

all_individual_clinician_results <- all_individual_clinician_results %>%
  dplyr::mutate(group = case_when(grepl("Physician|Pyysician|Phsycian", professional_role) ~ "physician",
                                            grepl("Resident", professional_role) ~ "resident",
                                            grepl("Nurse", professional_role) ~ "nurse"))

all_individual_clinician_results %>%
  dplyr::select(sens, spec, group) %>%
  replace(is.na(.), 0) %>%
  rbind(chartwatch_model %>%
          select(sens, spec) %>%
          mutate(group = "model")) %>%
  ggplot(aes(x = 1 - spec, y = sens, color = group)) + geom_point() +
  geom_abline()


all_individual_clinician_results %>%
  dplyr::select(sens, ppv, group) %>%
  replace(is.na(.), 0) %>%
  rbind(chartwatch_model %>%
          select(sens, ppv) %>%
          mutate(group = "model")) %>%
  ggplot(aes(x = ppv, y = sens, color = group)) + geom_point() +
  geom_abline()
