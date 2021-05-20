#'  ***********************************************************************
#'  ***********************************************************************
#'  ***********************************************************************
#'  Script name:
#'  Initially created by: 
#'  Date created: 
#'  Maintainer information: 
#'
#'  Script contents: Generate MARS predictions for all available data
#'
#'  ***********************************************************************
#'  ***********************************************************************
#'  ***********************************************************************

library(dplyr)
library(keyring)
library(dplyr)
library(lubridate)
library(RODBC)
source("helpers.R")


# Keyring unlock - used for DB connections --------------------------------

options("keyring_backend" = "file",
        stringsAsFactors = F)
keyring_unlock(keyring = "database",
               password = Sys.getenv("R_KEYRING_PASSWORD"))


# Load model --------------------------------------------------------------
# Load the models and recipe objects. These were trained on Lambda-Blade
# with code in /train_mars_model

model_dir <- "/mnt/research/LKS-CHART/Projects/gim_ews_project/data/paper-data/"

# Note: The model was NOT trained on any short encounters. Short-stay encounters
# are currently counter in train data, but were not used to train the model
load(file.path(model_dir, "final_mars_model_paper.R")) # final_mars_model_paper
load(file.path(model_dir, "final_mars_recipe.R")) # final_mars_recipe
load(file.path(model_dir, "final_mars_wfl_paper.R")) # final_mars_wfl_paper
prepped_mars_recipe <- recipes::prep(final_mars_recipe)

# These are all the encounters the MARS model was trained on
model_encounters <- final_mars_recipe$template %>%
  pull(ENCOUNTER_NUM)

# Load data ---------------------------------------------------------------
# Load paper data. These files were created from the generate-data-for-descriptive-stats.R 
# script.
data_folder <- "/mnt/research/LKS-CHART/Projects/gim_ews_project/data/paper-data"
train_encounters <- read.csv(file.path(data_folder, "train_encounters.csv"), 
                             stringsAsFactors = FALSE)
valid_encounters <- read.csv(file.path(data_folder, "valid_encounters_method2.csv"), 
                             stringsAsFactors = FALSE)
test_encounters <- read.csv(file.path(data_folder, 
                                      "test_encounters_with_outcomes_method2.csv"), 
                            stringsAsFactors = FALSE) 

# Load data that was used to train models (?)
model_data_dir <- "/mnt/research/LKS-CHART/Projects/gim_ews_preassessment_project/data/modeling_data/"

# Note: This object contains all train_encounters
train_data <- readRDS(file.path(model_data_dir, "train_data.Rds"))
heldout_data <- readRDS(file.path(model_data_dir, "heldout_data.Rds"))
cv_splits <- readRDS(file.path(model_data_dir, "cv_splits.Rds"))

train_mars_scores <- chartwatch::mars_predict(mars_data = train_data, 
                                              mars_recipe = prepped_mars_recipe,
                                              mars_model = final_mars_model_paper)

valid_mars_scores <- chartwatch::mars_predict(mars_data = heldout_data, 
                                              mars_recipe = prepped_mars_recipe,
                                              mars_model = final_mars_model_paper)


# Get scores from prediction frames ---------------------------------------


# Connect to Soarian
con_soarian <- chartwatch::connect_soarian(
  username = keyring::key_get(service = "SOARIAN",
                              username = "SOARIAN_USERNAME",
                              keyring = "database"),
  password = keyring::key_get(service = "SOARIAN",
                              username = "SOARIAN_PASSWORD",
                              keyring = "database"),
  dsn = "soarian")

# Get all scores from prediction frames
# from Apr 2019 - Aug 2019
prediction_frames_folder <- "/mnt/research/LKS-CHART/Projects/gim_ews_preassessment_project/data/prediction_frames/"


# Load census - this file was created in 05-extract-model-data.R
load(file.path(prediction_frames_folder, "gim_census.Rda"))
gim_census <- gim_census %>%
  # Extract ENCOUNTER_NUM
  mutate(ENCOUNTER_NUM = stringr::str_sub(PatientAccountID, 3, -1)) %>%
  mutate(GIM_START_TS = if_else(is.na(GIM_START_TS), VisitStartDateTime, GIM_START_TS)) 


all_mars_scores <- list()
i <- 1

for (folder in list.files(prediction_frames_folder, full.names = TRUE)) {
  
  if (dir.exists(folder)) {
    print(folder)
    
    encounters <- read.csv(file.path(folder, "gim_encounters.csv"), stringsAsFactors = FALSE) %>%
      mutate(current_time = lubridate::ymd_hms(current_time))
    
    # This file contains housing information --> used as a variable in current
    # MARS model
    patient_demographics <- read.csv(file.path(folder, "patient_demographics.csv"))
    
    soarian_census <- gim_census %>%
      filter(ENCOUNTER_NUM %in% encounters$ENCOUNTER_NUM) %>%
      mutate(ENCOUNTER_NUM = as.numeric(ENCOUNTER_NUM)) %>%
      dplyr::mutate(gender = if_else(Sex == "F", 1, 0))
    
    soarian_obs <- extract_soarian_feat(con_soarian, soarian_census)
    soarian_labs <- chartwatch::extract_soarian_labs(con_soarian, soarian_census)
    
    # Create patient timeseries
    
    encounters_data <- soarian_census %>%
      select(ENCOUNTER_NUM, start_ts = VisitStartDateTime) %>%
      filter(!is.na(start_ts)) %>% 
      filter(start_ts < min(encounters$current_time))
    
    if(nrow(encounters_data) == 0) {
      next
    }
    if (any(minute(encounters_data$start_ts) == 0 & hour(encounters_data$start_ts) == 0)) {
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
                                                           desc_name = DESCRIPTIVE_STATS_FILE, 
                                                           trans_name = TRANSFORMATIONS_FILE)

    
    census_information <- gim_census %>%
      filter(ENCOUNTER_NUM %in% encounters$ENCOUNTER_NUM) %>%
      mutate(ENCOUNTER_NUM = as.numeric(ENCOUNTER_NUM)) %>%
      dplyr::mutate(gender = if_else(Sex == "F", 1, 0))
    
    model_data <- cleaned_data %>%
      dplyr::left_join(patient_demographics, by = "ENCOUNTER_NUM") %>%
      dplyr::left_join(census_information %>% select(ENCOUNTER_NUM, age, gender), by = "ENCOUNTER_NUM")
    
    mars_scores <- chartwatch::mars_predict(mars_data = model_data, 
                                            mars_recipe = prepped_mars_recipe,
                                            mars_model = final_mars_model_paper)
    
    # Save intermediate results and model predictions
    write.csv(labs, file = file.path(folder, "processed_labs.csv"), row.names = FALSE)
    write.csv(vitals, file = file.path(folder, "processed_vitals.csv"), row.names = FALSE)
    write.csv(patient_ts, file.path(folder, "patient_ts.csv"), row.names = FALSE)
    write.csv(mars_scores, file = file.path(folder, "mars_scores_2021_0219.csv"), row.names = FALSE)
    
    all_mars_scores[[i]] <- mars_scores
    i <- i + 1
  }
}

# Close ODBC connection
RODBC::odbcClose(con_soarian)
all_mars_scores_combined <- rbind(do.call(rbind, all_mars_scores) %>%
                                    unique(), 
                                  train_mars_scores, 
                                  valid_mars_scores) %>%
  unique()
write.csv(all_mars_scores_combined,
          file = "/mnt/research/LKS-CHART/Projects/gim_ews_project/data/paper-data/all_mars_score_2021_0222.csv",
          row.names = FALSE)











