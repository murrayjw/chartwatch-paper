#'   ***********************************************************************
#'   ***********************************************************************
#'   ***********************************************************************
#'   Initially created by: Josh Murray
#'   Date: September 1st 2019
#'   contact information: murrayj@smh.ca
#'
#'   Script contents: This script extracts all of the data
#'   required to make predictions for the chart watch model
#'
#'   ***********************************************************************
#'   ***********************************************************************
#'   ***********************************************************************


# load the required libraries ---------------------------------------------
library(CHART)
library(tidyverse)
library(lubridate)
library(dbplyr)
library(glmnet)
library(tidymodels)
source('helpers.R')

# path to save data
path <- 'Z:\\LKS-CHART\\Projects\\gim_ews_preassessment_project\\data\\prediction_frames\\'
dirs <- list.files(path, full.names =T )
dirs <- dirs[!grepl('last_predictions', dirs)]
dirs <- dirs[!grepl("ensemble", dirs)]

mars_recipe <- readRDS("Z:/DSAA_Deployment/chartwatch_project/model_objects/mars/mars_recipe.Rds")
mars_model <- readRDS("Z:/DSAA_Deployment/chartwatch_project/model_objects/mars/mars_model.Rds")
ensemble_mars_recipe <- readRDS("Z:/DSAA_Deployment/chartwatch_project/model_objects/ensemble_time_aware_mars/logistic-ensemble_recipe2020_0505_formula_mars_pcts_baseline_interaction.Rds")
ensemble_mars_model <- readRDS("Z:/DSAA_Deployment/chartwatch_project/model_objects/ensemble_time_aware_mars/logistic-ensemble_2020_0505_formula_mars_pcts_baseline_interaction.Rds")


# connect to soarian ------------------------------------------------------
con_soarian <- DBI::dbConnect(odbc::odbc(), driver = "SQL Server", 
                              dsn = "soarian replica", database = "Soarian_Clin_Prd_1", 
                              uid = 'murrayj', server = "ClinicalRead")


# extract the gim census --------------------------------------------------
gim_census <- extract_census(con_soarian)


# save the census file ----------------------------------------------------


save(gim_census, file = paste0(path, 'gim_census.Rda'))

gim_census <- gim_census %>% 
  mutate(ENCOUNTER_NUM = stringr::str_sub(PatientAccountID, 3, -1)) %>% 
  as_tibble()

# connect to the various databases ----------------------------------------
con_edw <- connect_edw(keyring::key_get("EDW_USERNAME"),
                       password = keyring::key_get('EDW_PASSWORD'))

con_soarian <- RODBC::odbcConnect("soarian replica")

#source("H:\\gim_production\\utils.R")
#source("H:\\gim_production\\cleaning-utils.R")
source('extract-data-helpers.R')


adt <- get_adt(con_edw)
adt_serv <- CHART::get_location_timeseries(adt, location_or_service = 'service')
adt_loc <- CHART::get_location_timeseries(adt, location_or_service = 'location')

# get all gim patients
#missing_encounters <- sd
all_predictions <- list()
#missing_predictions <- list()
for(i in dirs) {
  
  tic <- Sys.time()
  cat('Read the encounters \n')
  
  encounters <- readr::read_csv(paste0(i, "\\gim_encounters.csv"))

  
  encounters <- encounters %>% 
    mutate(GIM_START_TS = if_else(is.na(GIM_START_TS), ADMIT_TS, GIM_START_TS))
  print(encounters)
  
  if(nrow(encounters) == 0) {
    next()
  }
  
  tmp_serv <- gim_census %>% 
    filter(ENCOUNTER_NUM %in% encounters$ENCOUNTER_NUM)
  
  tmp_serv <- tmp_serv %>% 
    mutate(GIM_START_TS = if_else(is.na(GIM_START_TS), VisitStartDateTime, GIM_START_TS))

  soarian_obs <- extract_soarian_feat(con_soarian, tmp_serv)
  soarian_labs <- chartwatch::extract_soarian_labs(con_soarian, tmp_serv)
  
  soarian_demographics <- tmp_serv %>%
    # select columns related to demographics
    select(ENCOUNTER_NUM = PatientAccountID,
           StreetAddress,
           City,
           age, 
           Sex, 
           BirthDate) %>%
    mutate(ENCOUNTER_NUM =as.integer(ENCOUNTER_NUM)) %>% 
    unique()
  
  
  encounters_data <- tmp_serv %>%
    select(ENCOUNTER_NUM, start_ts = VisitStartDateTime) %>%
    filter(!is.na(start_ts)) %>% 
    filter(start_ts < min(encounters$current_time))
  
  if(nrow(encounters_data) == 0) {
    next
  }
  if(any(minute(encounters_data$start_ts) == 0 & hour(encounters_data$start_ts))) {
    encounters_data <- encounters_data %>% 
      mutate(start_ts = if_else(minute(start_ts) == 0 &
                                  hour(start_ts) == 0,
                                start_ts + dminutes(1), start_ts))
  }
  

  
  
  patient_ts <- create_ts(encounters_data, 
                          time = as.character(min(encounters$current_time)))
  
  labs <- chartwatch::process_labs_data(soarian_labs)
  soarian_obs <- soarian_obs %>% 
    mutate(VALUE = as.character(VALUE))
  vitals <- chartwatch::process_vitals_data(soarian_obs)
  
  numeric_data <- patient_ts %>%
    mutate(ENCOUNTER_NUM = as.integer(ENCOUNTER_NUM)) %>% 
    dplyr::left_join(labs, by = c("ENCOUNTER_NUM", "timestamp")) %>%
    dplyr::left_join(vitals, by = c("ENCOUNTER_NUM", "timestamp"))
  
  cleaned_data <- chartwatch::prepare_numeric_timeseries(numeric_data, 
                                                         desc_name = "Z:/DSAA_Deployment/chartwatch_project/model_objects/numeric_descriptive_statistics.R", 
                                                         trans_name = "Z:/DSAA_Deployment/chartwatch_project/model_objects/numeric_variable_names.csv")
  full_data <- cleaned_data %>%
    left_join(soarian_demographics %>% select(ENCOUNTER_NUM, age), by = "ENCOUNTER_NUM")
  mars_predictions <- chartwatch::mars_predict(full_data, 
                                               mars_recipe = mars_recipe,
                                               mars_model = mars_model) %>%
    rename(mars = score)
  
  
  ensemble_data <- chartwatch::ensemble_mars_time_process(data = mars_predictions)
  ensemble_scores <- chartwatch::ensemble_mars_time_predict(ensemble_data = ensemble_data, 
                                                            ensemble_recipe = ensemble_mars_recipe,
                                                            ensemble_model = ensemble_mars_model)
  
  risk_groups <- get_risk_groups(ensemble_scores)
  
  readr::write_csv(risk_groups, paste0(i, '/full_ensemble_predictions.csv'))

  
  ensemble_scores_last <- ensemble_scores %>% 
    group_by(ENCOUNTER_NUM) %>% 
    summarize(timestamp = max(timestamp),
              max_score = max(score),
              last_score = score[row_number() == n()],
              last_score2 = mean(score[row_number() %in% c(n(), (n()-1))]),
              last_score2m = max(score[row_number() %in% c(n(), (n()-1))])) %>% 
    rename(score = max_score) %>% 
    ungroup()
  
  ensemble_scores_last <- get_risk_groups(ensemble_scores_last)
  #readr::write_csv(ensemble_scores_last, paste0(i, 'last_predictions.csv'))
  
  toc <- Sys.time()
 
  print(toc - tic)
  print(i)
  
  all_predictions[[i]] <- ensemble_scores_last
}

#m <- do.call(rbind, missing_predictions)
model_predictions_full <- do.call(rbind, all_predictions)

save(model_predictions, file = "Z:\\LKS-CHART\\Projects\\gim_ews_preassessment_project\\data\\model_predictions.Rda")
save(model_predictions_full, file = "Z:\\LKS-CHART\\Projects\\gim_ews_preassessment_project\\data\\model_predictions_full.Rda")
