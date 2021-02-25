#'  ***********************************************************************
#'  ***********************************************************************
#'  ***********************************************************************
#'  Script name:
#'  Initially created by: 
#'  Date created: Feb 25, 2021
#'  Maintainer information: 
#'
#'  Script contents: Train time-aware MARS model (logistic regression)
#'
#'  ***********************************************************************
#'  ***********************************************************************
#'  ***********************************************************************

library(dplyr)
library(recipes)
library(tune)
library(parsnip)
source("constants.R")


# Get training data -------------------------------------------------------

train_data <- final_mars_recipe$template
train_encounter_nums <- unique(train_data$ENCOUNTER_NUM)
soarian_train_encounter_nums <- paste0("00", train_encounter_nums)


# Load all MARS scores ----------------------------------------------------

train_mars_scores <- read.csv(file = ALL_MARS_SCORES_FILENAME,
                              stringsAsFactors = FALSE) %>% 
  dplyr::filter(ENCOUNTER_NUM %in% train_encounter_nums) %>%
  dplyr::mutate(timestamp = lubridate::ymd_hms(timestamp)) %>%
  dplyr::rename(mars = score)


# Process MARS score ------------------------------------------------------
# Use existing chartwatch function. The pre-processing it not dependent
# on training data, since it's only adding lagged variables.

processed_mars_scores <- chartwatch::ensemble_mars_time_process(train_mars_scores)



# Get outcomes ------------------------------------------------------------

outcomes <- read.csv(
  "/mnt/research/LKS-CHART/Projects/gim_ews_project/data/retraining_2020_0120/train/outcome_timeseries.csv",
  stringsAsFactors = FALSE
) %>%
  dplyr::mutate(timestamp = lubridate::ymd_hms(timestamp))

model_data <- processed_mars_scores %>%
  dplyr::left_join(outcomes, by = c("ENCOUNTER_NUM", "timestamp"))

# Train time-aware MARS model ---------------------------------------------
# Re-using code from https://github.com/LKS-CHART/gim_ews/blob/master/ensemble/03-train-ensemble.R

# 10-fold CV split
cv_split <- model_data %>%
  rsample::group_vfold_cv(group = "ENCOUNTER_NUM", v = 10)

# Load old recipe
# Can re-use old recipe since the recipe steps are not reliant on the train
# data
ensemble_recipe <- readRDS(ENSEMBLE_MARS_RECIPE_FILENAME)
summary(ensemble_recipe$steps)

# Set up model
gim_model_logistic <- parsnip::logistic_reg(mode = "classification", 
                                            penalty = tune::tune(), 
                                            mixture = tune::tune()) %>% 
  parsnip::set_engine("glmnet")

# Set up logistic regression hyperparameter search grid
logistic_hyperparam_grid <- expand.grid(penalty = 10^seq(-3, -1, length = 30), 
                                        mixture = (0:10)/10)

# Used for hyperparameter tuning
ctrl <- control_grid(verbose = T, save_pred = T)
roc_vals <- yardstick::metric_set(yardstick::roc_auc)

# Set up workflow
gim_wflow <- workflows::workflow() %>%
  workflows::add_recipe(ensemble_recipe) %>%
  workflows::add_model(gim_model_logistic)


# Train model! ------------------------------------------------------------

logistic_tuned <- tune::tune_grid(
  gim_wflow,
  grid = logistic_hyperparam_grid,
  resamples = cv_split,
  metrics = roc_vals,
  control = ctrl
)

# Retrain using best hyperparameters 
best_logistic <- select_best(logistic_tuned, metric = "roc_auc")
prepped_gim_data <- prep(ensemble_recipe, training = model_data, fresh = TRUE)

# Train on all training data
gim_model_logistic_final <- finalize_model(gim_model_logistic, best_logistic) %>% 
  fit(outcome_all_48 ~ ., data = juice(prepped_gim_data))


# Save
saveRDS(gim_model_logistic_final,
        file = ENSEMBLE_MARS_MODEL_FILENAME)

# Evaluate ----------------------------------------------------------------

# Evaluate on train data
train_pred <- predict(gim_model_logistic_final, 
                      recipes::bake(ensemble_recipe, new_data = model_data),
                      type = "prob") %>%
  bind_cols(model_data)

roc_obj <- pROC::roc( train_pred$outcome_all_48, train_pred$.pred_1)
pROC::auc(roc_obj)

roc_obj <- pROC::roc( train_pred$OUTCOME_ALL, train_pred$.pred_1)
pROC::auc(roc_obj)

roc_obj <- pROC::roc( train_pred$outcome_all_72, train_pred$.pred_1)
pROC::auc(roc_obj)

# Evaluate on validation data

valid_encounters <- read.csv(PAPER_VALID_ENCOUNTERS_FILENAME, stringsAsFactors = FALSE)

valid_mars_scores <-  read.csv(file = ALL_MARS_SCORES_FILENAME,
                               stringsAsFactors = FALSE) %>% 
  # Validation set - encounter nums in valid encounters but NOT in train_encounter_nums
  # since the training set was actually larger than expected
  dplyr::filter(ENCOUNTER_NUM %in% valid_encounters$ENCOUNTER_NUM) %>%
  dplyr::filter(!ENCOUNTER_NUM %in% train_encounter_nums) %>%
  dplyr::mutate(timestamp = lubridate::ymd_hms(timestamp)) %>%
  dplyr::rename(mars = score)
valid_processed_mars_scores <- chartwatch::ensemble_mars_time_process(valid_mars_scores)
valid_model_data <- valid_processed_mars_scores %>%
  dplyr::left_join(outcomes, by = c("ENCOUNTER_NUM", "timestamp"))

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

# Evaluate on prospective test data

test_encounters <- read.csv(ALL_MARS_SCORES_FILENAME,
                            stringsAsFactors = FALSE)

test_mars_scores <-  read.csv(file = ALL_MARS_SCORES_FILENAME,
                               stringsAsFactors = FALSE) %>% 
  dplyr::filter(ENCOUNTER_NUM %in% test_encounters$ENCOUNTER_NUM) %>%
  dplyr::mutate(timestamp = lubridate::ymd_hms(timestamp)) %>%
  dplyr::rename(mars = score)

test_processed_mars_scores <- chartwatch::ensemble_mars_time_process(test_mars_scores)
test_model_data <- test_processed_mars_scores %>%
  dplyr::left_join(outcomes, by = c("ENCOUNTER_NUM", "timestamp"))

test_pred <- predict(gim_model_logistic_final, 
                      recipes::bake(ensemble_recipe, new_data = test_model_data),
                      type = "prob") %>%
  bind_cols(test_model_data)

roc_obj <- pROC::roc(test_pred$outcome_all_48, test_pred$.pred_1)
pROC::auc(roc_obj)

roc_obj <- pROC::roc(test_pred$OUTCOME_ALL, test_pred$.pred_1)
pROC::auc(roc_obj)

roc_obj <- pROC::roc(test_pred$outcome_all_72, test_pred$.pred_1)
pROC::auc(roc_obj)
