#'  ***********************************************************************
#'  ***********************************************************************
#'  ***********************************************************************
#'  Script name:
#'  Initially created by: 
#'  Date created: 
#'  Maintainer information: 
#'
#'  Script contents: This is the script that was used to train the MARS model. 
#'  Run in Nov 2020 on the Lambda-Blade server
#'
#'  ***********************************************************************
#'  ***********************************************************************
#'  ***********************************************************************


# load the required libraries ---------------------------------------------
library(dplyr)
library(recipes)
library(rsample)
library(tidymodels)
library(tune)
library(workflows)
library(lubridate)

# directory to the training data ------------------------------------------
fs <- list.files('data/retraining_2020_0120/train', full.names = T)

# some helper functions
source("numeric-only-R-models/model-helpers.R")


# load the data -----------------------------------------------------------
numeric <- data.table::fread(fs[7])
encounters <- data.table::fread(fs[5])
demos <- data.table::fread(fs[4])
outcomes <- data.table::fread(fs[8])
baseline <- data.table::fread(fs[2])

train_encounters <- encounters %>% 
  filter(as_date(ADMIT_TS) >= ymd('2011-04-02'),
         as_date(ADMIT_TS) <= ymd('2018-11-30'))


valid_encounters <- encounters %>% 
  filter(as_date(ADMIT_TS) > ymd('2018-11-30'),
         as_date(ADMIT_TS) <= ymd('2019-04-30'))

train_numeric <- numeric %>% 
  filter(ENCOUNTER_NUM %in% train_encounters$ENCOUNTER_NUM)
train_demos <- demos %>% 
  filter(ENCOUNTER_NUM %in% train_encounters$ENCOUNTER_NUM)
train_outcomes <- outcomes %>% 
  filter(ENCOUNTER_NUM %in% train_encounters$ENCOUNTER_NUM)
train_baseline <- baseline %>% 
  filter(ENCOUNTER_NUM %in% train_encounters$ENCOUNTER_NUM)

valid_numeric <- numeric %>% 
  filter(ENCOUNTER_NUM %in% valid_encounters$ENCOUNTER_NUM)
valid_demos <- demos %>% 
  filter(ENCOUNTER_NUM %in% valid_encounters$ENCOUNTER_NUM)
valid_outcomes <- outcomes %>% 
  filter(ENCOUNTER_NUM %in% valid_encounters$ENCOUNTER_NUM)



# merge the data ----------------------------------------------------------
full_data <- train_numeric %>% 
  left_join(train_demos) %>% 
  left_join(train_encounters %>% select(ENCOUNTER_NUM, age, gender)) %>% 
  left_join(train_outcomes)



# prepare the model data --------------------------------------------------
model_data <- prepare_data(full_data, input_w = 6)



# set seed for reproducibility --------------------------------------------
set.seed(4322)


# downsample non outcomes -------------------------------------------------

# non outcomes
zeros <- model_data %>% 
  filter(outcome_all_48 == 0)

# outcomes
ones <- model_data %>% 
  filter(outcome_all_48 == 1)

# sample 15% of rows from non outcomes
zeros <- zeros %>% 
  group_by(ENCOUNTER_NUM) %>% 
  sample_frac(.25) %>% 
  ungroup()

# bind data back together
full_data <- ones %>% 
  bind_rows(zeros) %>% 
  arrange(ENCOUNTER_NUM, timestamp)

full_data[is.na(full_data)] <- 0


# create factor for the outcome -------------------------------------------
full_data <- full_data %>% 
  mutate(outcome_all_48 = ifelse(outcome_all_48 == 1, "death_icu_pa", "no_outcome"),
         outcome_all_48 = factor(outcome_all_48, levels = c("no_outcome", "death_icu_pa")))


# extract the start time for each outcome ---------------------------------

full_data <- full_data %>% 
  group_by(ENCOUNTER_NUM) %>% 
  mutate(start_time = min(timestamp)) %>% 
  ungroup()

# arrange by start time for intitial split
full_data <-full_data %>% 
  arrange(start_time)


# train test split --------------------------------------------------------

cvsplits <- group_vfold_cv(full_data,  group = "ENCOUNTER_NUM", v = 10)

train <- training(train_test_split)
test <- testing(train_test_split)



# add a recipe ------------------------------------------------------------
mars_rec <- 
  recipe(outcome_all_48 ~ ., data = full_data) %>% 
  step_mutate(outcome_all_48 = factor(outcome_all_48)) %>% 
  step_rm(contains('outcome'),
          OUTCOME_ALL,
          OUTCOME_TS,
          contains("ENCOU"),
          contains('diff2'),
          timestamp,
          time_elapsed, 
          window,
          contains('lab_ical'),
          start_time,
          -outcome_all_48) 


# create model object -----------------------------------------------------
mars_mod <-  
  mars(num_terms = tune("mars terms"), prod_degree = tune(), prune_method = "forward") %>% 
  set_engine("earth") %>% 
  set_mode("classification")



# create workflow object --------------------------------------------------

mars_wflow <-
  workflow() %>%
  add_recipe(mars_rec) %>%
  add_model(mars_mod)


# set the tuning parameters -----------------------------------------------
mars_set <-
  parameters(mars_wflow) %>%
  update(
    `mars terms` = num_terms(c(2, 100)))


ctrl <- control_bayes(verbose = TRUE, save_pred = TRUE)


set.seed(7891)

library(parallel)
cl <- makeCluster(25)

mars_tune <-
  tune_bayes(
    mars_wflow,
    resamples = cvsplits,
    iter = 35,
    param_info = mars_set,
    metrics = metric_set(roc_auc),
    initial = 6,
    control = ctrl
  )
parallel::stopCluster(cl)


# save the tuning object --------------------------------------------------
save(mars_tune, file = 'numeric-only-R-models/R-model-objects/mars/mars_tune.R')



# save the parameters of the best model -----------------------------------
best_mars <-
  select_best(mars_tune_paper, metric = "roc_auc", maximize = T)
save(best_mars_paper, file = 'numeric-only-R-models/R-model-objects/mars/best_mars_paper')



# finalize the workflow ---------------------------------------------------
final_mars_wfl <- finalize_workflow(mars_wflow, best_mars_paper)



# save the workflow object ------------------------------------------------
final_mars_wfl <- fit(final_mars_wfl, data = full_data)
save(final_mars_wfl_paper, file = 'numeric-only-R-models/R-model-objects/mars/final_mars_wfl_paper.R')



# save the final recipe ---------------------------------------------------
final_mars_recipe <- pull_workflow_preprocessor(final_mars_wfl)
save(final_mars_recipe, file = 'numeric-only-R-models/R-model-objects/mars/final_mars_recipe.R')


# save the prepped recipe -------------------------------------------------
prepped_mars_rec <- prep(final_mars_recipe, full_data)
test_mars_prep <- bake(prepped_mars_rec, full_data)
save(prepped_mars_rec, file = 'numeric-only-R-models/R-model-objects/mars/prepped_mars_rec.R')


# save the fitted model object --------------------------------------------
final_mars_model <- workflows::pull_workflow_fit(final_mars_wfl)
save(final_mars_model_paper , file = 'numeric-only-R-models/R-model-objects/mars/final_mars_model_paper.R')
