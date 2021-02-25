#'  ***********************************************************************
#'  ***********************************************************************
#'  ***********************************************************************
#'  Script name:
#'  Initially created by: 
#'  Date created: 
#'  Maintainer information: 
#'
#'  Script contents: Script 02-generate-model-predictions is missing some
#'  predictions. Re-run without repulling Soarian data
#'
#'  ***********************************************************************
#'  ***********************************************************************
#'  ***********************************************************************


library(dplyr)
library(lubridate)
library(chartwatch)

source("helpers.R")
source("constants.R")

# IF re-running and need to re-pull data, set to TRUE
WRITE_MODE <- FALSE

#' Don't need to process BP bc they've already been split into systolic/diastolic
#' BP. Use VALUE column, since it's already been processed for temperature.
#' https://github.com/LKS-CHART/chartwatch/blob/update_get_numeric_descriptive_statistics/R/preprocessing.R#L931
#'
#' @param vitals_data 
#'
#' @return
#' @export
#'
#' @examples
process_vitals_from_numeric <- function(vitals_data) {
  
  vitals_soarian <- vitals_data %>%
    dplyr::mutate(FINDINGNAME = ifelse(is.na(FINDINGNAME), "sbp", FINDINGNAME)) %>%
    dplyr::mutate(numeric_value = ifelse(FINDINGNAME %in% c("Temperature", "Temperature (c)"), as.numeric(VALUE), numeric_value))
  
  
  # clean column names
  names(vitals_data) <- toupper(names(vitals_data))
  vitals_data <- vitals_data %>%
    dplyr::rename(numeric_value = NUMERIC_VALUE) %>%
    dplyr::select(-PATIENTVISIT_OID,
                  -OBSERVATIONSTATUS,
                  -VALUE,
                  -MINVALUE,
                  -MAXVALUE,
                  -FINDINGDATATYPE,
                  -ASSESSMENTSTATUS,
                  -ENTEREDDT,
                  -ORDERSASWRITTENTEXT,
                  -OBJECTID,
                  -contains("ASS_OBJECTID"),
                  -ASSESSMENT_OID)
  
  vitals_soarian <- vitals_soarian %>%
    dplyr::rename(variable = FINDINGABBR) %>%
    dplyr::mutate(variable = clean_var_name(variable))
  
  
  # keep most common vital signs
  vital_signs <- vitals_soarian %>%
    dplyr::filter(variable %in% c("SBPSYSTOLIC",
                                  "SBPDIASTOLIC",
                                  "SPulse",
                                  "SRespirations",
                                  "SO2Saturation",
                                  "STemperature",
                                  "SPainIntRest",
                                  "SPainIntMove",
                                  "SFIO2"))
  
  vital_signs <- vital_signs %>%
    dplyr::select(ENCOUNTER_NUM, variable, timestamp = OBS_CREATIONTIME, numeric_value) %>%
    dplyr::mutate(variable = paste0("vital_", tolower(variable)))%>%
    dplyr::arrange(ENCOUNTER_NUM, timestamp)
  
  # Initial shift assessment
  shift_assessment <- vitals_soarian %>%
    dplyr::filter(FORMUSAGE == "Initial Shift Assessment" |
                    variable == "SRpFiO2b")
  
  # select top occuring shift assessment variables:
  shift_assessment <- shift_assessment %>%
    dplyr::filter(variable %in% c("SPNIntstyRest1",
                                  "SPNInstyMov1",
                                  "SRpO2LMin",
                                  "SPNIntstyRest2",
                                  "SPNInstyMov2",
                                  "SCVHrtRate",
                                  "SRpFiO2b"))
  
  shift_assessment <- shift_assessment %>%
    dplyr::select(ENCOUNTER_NUM, variable, timestamp = OBS_CREATIONTIME, numeric_value) %>%
    dplyr::mutate(variable = paste0("shift_assess_", tolower(variable))) %>%
    dplyr::arrange(ENCOUNTER_NUM, timestamp)
  
  # intake outake
  in_out <- vitals_soarian %>%
    dplyr::filter(FORMUSAGE == "Intake and Output")
  
  # select top occuring intake outake variables:
  in_out <- in_out %>%
    dplyr::filter(variable %in% c("SIVNormalSaline",
                                  "AIVPB1",
                                  "ACatheter",
                                  "SOtherIntake",
                                  "AOthOutput",
                                  "SIV23and13",
                                  "ATmsIncontinent"))
  in_out <- in_out %>%
    dplyr::select(ENCOUNTER_NUM, variable, timestamp = OBS_CREATIONTIME, numeric_value) %>%
    dplyr::mutate(variable = paste0("in_out_", tolower(variable))) %>%
    dplyr::arrange(ENCOUNTER_NUM, timestamp)
  
  
  # skin assessment (BRADEN SCALE)
  skin <- vitals_soarian %>%
    dplyr::filter(FORMUSAGE == "Skin Assessment" |
                    (FORMUSAGE == "ADMISSION" & variable == "ABradenScore"))
  
  skin <- skin %>%
    dplyr::select(ENCOUNTER_NUM, variable, timestamp = OBS_CREATIONTIME, numeric_value) %>%
    dplyr::mutate(variable = paste0("skin_", tolower(variable))) %>%
    dplyr::arrange(ENCOUNTER_NUM, timestamp)
  
  # Diabetic Control
  # intake outake
  db <- vitals_soarian %>%
    dplyr::filter(FORMUSAGE == "Diabetic Protocol" |
                    (FORMUSAGE == "ADMISSION" & variable == "Diabetic Protocol"))
  
  db <- db %>%
    dplyr::select(ENCOUNTER_NUM, variable, timestamp = OBS_CREATIONTIME, numeric_value) %>%
    dplyr::mutate(variable = paste0("diabetic_", tolower(variable))) %>%
    dplyr::arrange(ENCOUNTER_NUM, timestamp)
  
  
  # alcohol
  alcohol <- vitals_soarian %>%
    dplyr::filter(FORMUSAGE == "CIWA Assessment",
                  variable == "SCIWAScore")
  
  alcohol <- alcohol %>%
    dplyr::select(ENCOUNTER_NUM, variable, timestamp = OBS_CREATIONTIME, numeric_value) %>%
    dplyr::mutate(variable = paste0("alcohol_", tolower(variable))) %>%
    dplyr::arrange(ENCOUNTER_NUM, timestamp)
  
  # recombine data
  vitals_processed <- vital_signs %>%
    dplyr::bind_rows(shift_assessment) %>%
    dplyr::bind_rows(in_out) %>%
    dplyr::bind_rows(skin) %>%
    dplyr::bind_rows(db) %>%
    dplyr::bind_rows(alcohol)
  
  
  # Get vitals at hourly level
  vitals_hourly <- vitals_processed  %>%
    dplyr::mutate(timestamp = lubridate::ceiling_date(timestamp, unit = "hour")) %>%
    dplyr::group_by(ENCOUNTER_NUM, variable, timestamp) %>%
    dplyr::summarize(numeric_value = mean(numeric_value, na.rm = TRUE)) %>%
    dplyr::ungroup()
  
  
  # Spread dataframe by vitals type ----------------------------------------
  vitals_soarian <- vitals_hourly %>%
    tidyr::pivot_wider(names_from = variable,
                       values_from = numeric_value,
                       values_fn = list(numeric_value = mean))
  
  # Change column format
  vitals_soarian <- vitals_soarian %>%
    dplyr::mutate(ENCOUNTER_NUM = as.integer(ENCOUNTER_NUM)) %>%
    dplyr::mutate(timestamp = lubridate::ymd_hms(as.character(timestamp)))
  
  return(vitals_soarian)
}

create_ts_from_discharge <- function(data) {
  time_series <- list()
  for (i in 1:nrow(data)) {
    tmp <- data %>% dplyr::slice(i)
    
    start <- lubridate::ceiling_date(lubridate::ymd_hms(as.character(tmp$start_ts)), 
                                     unit = "hour")
    
    end <- lubridate::ceiling_date(lubridate::ymd_hms(as.character(tmp$end_ts)), 
                                   unit = "hour")
    
    tte <- as.numeric(difftime(end, start, units = "day"))
    
    if(tte > LOS_CUTOFF) {
      start <- end - lubridate::ddays(LOS_CUTOFF)
    }
    
    
    timestamps <- seq(start, end, by = "hour")
    time_elapsed <- as.numeric(difftime(timestamps, start, 
                                        units = "hours"))
    encounter_timeseries <- dplyr::tibble(ENCOUNTER_NUM = tmp$ENCOUNTER_NUM, 
                                          timestamp = timestamps, time_elapsed = time_elapsed)
    n <- nrow(encounter_timeseries)
    time_series[[i]] <- encounter_timeseries
  }
  encounter_ts <- data.table::rbindlist(time_series) %>% tibble::as_tibble()
  encounter_ts <- encounter_ts %>% dplyr::group_by(ENCOUNTER_NUM) %>% 
    dplyr::mutate(timestamp_6hr = cumsum((time_elapsed + 
                                            1)%%6 == 1)) %>% dplyr::ungroup()
  return(encounter_ts)
}

# Load models -------------------------------------------------------------


model_dir <- "/mnt/research/LKS-CHART/Projects/gim_ews_project/data/paper-data/"
load(file.path(model_dir, "final_mars_model_paper.R")) # final_mars_model_paper
load(file.path(model_dir, "final_mars_recipe.R")) # final_mars_recipe
prepped_mars_recipe <- recipes::prep(final_mars_recipe)




# Load paper data ---------------------------------------------------------

data_folder <- "/mnt/research/LKS-CHART/Projects/gim_ews_project/data/paper-data"
train_encounters <- read.csv(file.path(data_folder, "train_encounters.csv"), 
                             stringsAsFactors = FALSE)
valid_encounters <- read.csv(file.path(data_folder, "valid_encounters_method2.csv"), 
                             stringsAsFactors = FALSE)
test_encounters <- read.csv(file.path(data_folder, 
                                      "test_encounters_with_outcomes_method2.csv"), 
                            stringsAsFactors = FALSE) 
# Load predictions for each date ------------------------------------------

prediction_frames_folder <- "/mnt/research/LKS-CHART/Projects/gim_ews_preassessment_project/data/prediction_frames/"

# Load census - this file was created in ../05-extract-model-data.R
load(file.path(prediction_frames_folder, "gim_census.Rda"))
gim_census <- gim_census %>%
  # Extract ENCOUNTER_NUM
  dplyr::mutate(ENCOUNTER_NUM = stringr::str_sub(PatientAccountID, 3, -1))


all_mars_scores <- list()
i <- 1


for (folder in list.files(prediction_frames_folder, full.names = TRUE)) {
  
  if (dir.exists(folder)) {
    print(folder)
    
    if (WRITE_MODE) {
      # List of all encounters for that prediction day
      encounters <- read.csv(file.path(folder, "gim_encounters.csv"), stringsAsFactors = FALSE) %>%
        mutate(current_time = lubridate::ymd_hms(current_time))
      
      # This file contains housing information --> used as a variable in paper
      # MARS model
      patient_demographics <- read.csv(file.path(folder, "patient_demographics.csv"))
      
      # Create patient timeseries
      encounters_data <- gim_census %>%
        # Only keep encounters that are predicted on for that day
        dplyr::filter(ENCOUNTER_NUM %in% encounters$ENCOUNTER_NUM) %>%
        dplyr::mutate(ENCOUNTER_NUM = as.numeric(ENCOUNTER_NUM)) %>%
        # Extract gender variable
        dplyr::mutate(gender = if_else(Sex == "F", 1, 0)) %>%
        # Set start_ts to be start of visit
        dplyr::select(ENCOUNTER_NUM, start_ts = VisitStartDateTime, age, gender) %>%
        dplyr::filter(!is.na(start_ts)) %>% 
        # Remove any encounters whose visit starts after prediction time
        dplyr::filter(start_ts < min(encounters$current_time))
      
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
      
      # Load already-processed labs and vitals information
      labs <- read.csv(file = file.path(folder, "processed_labs.csv"), 
                       stringsAsFactors  = FALSE) %>%
        dplyr::mutate(timestamp = lubridate::ymd_hms(timestamp))
      vitals <- read.csv(file = file.path(folder, "processed_vitals.csv"), 
                         stringsAsFactors = FALSE) %>%
        dplyr::mutate(timestamp = lubridate::ymd_hms(timestamp))
      
      # Create input to model
      numeric_data <- patient_ts %>%
        dplyr::mutate(ENCOUNTER_NUM = as.integer(ENCOUNTER_NUM)) %>% 
        dplyr::left_join(labs, by = c("ENCOUNTER_NUM", "timestamp")) %>%
        dplyr::left_join(vitals, by = c("ENCOUNTER_NUM", "timestamp"))
      cleaned_data <- chartwatch::prepare_numeric_timeseries(numeric_data, 
                                                             desc_name = "/mnt/research/DSAA_Deployment/chartwatch_project/model_objects/numeric_descriptive_statistics.R", 
                                                             trans_name = "/mnt/research/DSAA_Deployment/chartwatch_project/model_objects/numeric_variable_names.csv")
      model_data <- cleaned_data %>%
        dplyr::left_join(patient_demographics, by = "ENCOUNTER_NUM") %>%
        dplyr::left_join(encounters_data %>% select(ENCOUNTER_NUM, age, gender), by = "ENCOUNTER_NUM")
      
      mars_scores <- chartwatch::mars_predict(mars_data = model_data, 
                                              mars_recipe = prepped_mars_recipe,
                                              mars_model = final_mars_model_paper)
      
      old_mars_scores <- read.csv(file.path(folder, "mars_scores_2021_0219.csv"), stringsAsFactors = FALSE)
      
      if (nrow(old_mars_scores) != nrow(mars_scores)) {
        print(paste0("Difference in # of predictions in ", folder, ": ", nrow(old_mars_scores) - nrow(mars_scores)))
      }
      
      
      # Overwrite old patient_ts file --> the updated patient_ts
      # will either be the same as the old one OR will add new rows
      write.csv(patient_ts, file.path(folder, "patient_ts.csv"), row.names = FALSE)
      
      # Save updated MARS scores
      write.csv(mars_scores, file = file.path(folder, "mars_scores_2021_0222.csv"), row.names = FALSE)
    } else {
      mars_scores <- read.csv(file = file.path(folder, "mars_scores_2021_0222.csv"),
                              stringsAsFactors = FALSE)
    }
    all_mars_scores[[i]] <- mars_scores
    i <- i + 1
  }
}

# Load data that was used to train models (?)
model_data_dir <- "/mnt/research/LKS-CHART/Projects/gim_ews_preassessment_project/data/modeling_data/"
train_data <- readRDS(file.path(model_data_dir, "train_data.Rds"))
heldout_data <- readRDS(file.path(model_data_dir, "heldout_data.Rds"))
train_mars_scores <- chartwatch::mars_predict(mars_data = train_data, 
                                              mars_recipe = prepped_mars_recipe,
                                              mars_model = final_mars_model_paper)

valid_mars_scores <- chartwatch::mars_predict(mars_data = heldout_data, 
                                              mars_recipe = prepped_mars_recipe,
                                              mars_model = final_mars_model_paper)


# Pull missing data for validation encounters -----------------------------

all_mars_scores_combined <- rbind(do.call(rbind, all_mars_scores) %>%
                                    unique(), 
                                  train_mars_scores, 
                                  valid_mars_scores) %>%
  unique()

# Data missing for validation set
all_mars_scores_combined %>% 
  dplyr::mutate(group = case_when(
    ENCOUNTER_NUM %in% train_encounters$ENCOUNTER_NUM ~ 'train', 
    ENCOUNTER_NUM %in% valid_encounters$ENCOUNTER_NUM ~ 'valid', 
    ENCOUNTER_NUM %in% test_encounters$ENCOUNTER_NUM ~ 'test')
  ) %>% 
  dplyr::group_by(ENCOUNTER_NUM) %>% 
  dplyr::slice(1) %>% 
  dplyr::ungroup() %>% 
  dplyr::count(group)

missing_encs <- rbind(train_encounters, valid_encounters) %>%
  dplyr::filter(!ENCOUNTER_NUM %in% all_mars_scores_combined$ENCOUNTER_NUM) %>%
  unique() %>%
  dplyr::pull(ENCOUNTER_NUM)

# Read in training data (2011-2020)
encounters <- read.csv(
  "/mnt/research/LKS-CHART/Projects/gim_ews_project/data/retraining_2020_0120/train/encounters.csv", 
  stringsAsFactors = FALSE) %>%
  dplyr::filter(ENCOUNTER_NUM %in% missing_encs) %>%
  dplyr::mutate(start_ts = ADMIT_TS, 
                end_ts = DISCHARGE_TS)
demographics <- read.csv(
  "/mnt/research/LKS-CHART/Projects/gim_ews_project/data/retraining_2020_0120/train/demographics.csv",
  stringsAsFactors = FALSE) %>%
  dplyr::filter(ENCOUNTER_NUM %in% missing_encs)
load("/mnt/research/LKS-CHART/Projects/gim_ews_project/data/FULL-RAW-DATA/lab_results.Rda") 
lab_results <- lab_results %>%
  dplyr::filter(ENCOUNTER_NUM %in% missing_encs)
load("/mnt/research/LKS-CHART/Projects/gim_ews_project/data/FULL-RAW-DATA/numeric_soarian.Rda")
numeric_soarian <- numeric_soarian %>%
  dplyr::mutate(ENCOUNTER_NUM = substr(ENCOUNTER_NUM, 3, 11)) %>%
  dplyr::filter(ENCOUNTER_NUM %in% missing_encs)

# Process data
processed_labs <- chartwatch::process_labs_data(lab_results)
processed_vitals <- process_vitals_from_numeric(numeric_soarian)
patient_ts <- create_ts_from_discharge(data = encounters)


numeric_data <- patient_ts %>%
  mutate(ENCOUNTER_NUM = as.integer(ENCOUNTER_NUM)) %>% 
  dplyr::left_join(processed_labs, by = c("ENCOUNTER_NUM", "timestamp")) %>%
  dplyr::left_join(processed_vitals, by = c("ENCOUNTER_NUM", "timestamp"))

cleaned_data <- chartwatch::prepare_numeric_timeseries(numeric_data, 
                                                       desc_name = DESCRIPTIVE_STATS_FILE, 
                                                       trans_name = TRANSFORMATIONS_FILE)

model_data <- cleaned_data %>%
  dplyr::left_join(demographics, by = "ENCOUNTER_NUM") %>%
  dplyr::left_join(encounters %>% select(ENCOUNTER_NUM, age, gender), by = "ENCOUNTER_NUM")

missing_mars_scores <- chartwatch::mars_predict(mars_data = model_data, 
                                        mars_recipe = prepped_mars_recipe,
                                        mars_model = final_mars_model_paper)


# Merge
# Cast timestamp to character --> otherwise run into issues where some timestamps 
# get converted to numeric (e.g., 2020-01-01 01:00:00 --> 12341234)
updated_mars_scores <- rbind(all_mars_scores_combined %>% dplyr::mutate(timestamp = as.character(lubridate::ymd_hms(timestamp))),
                             missing_mars_scores %>% dplyr::mutate(timestamp = as.character(lubridate::ymd_hms(timestamp)))) %>%
  unique() 

write.csv(updated_mars_scores, 
          file = ALL_MARS_SCORES_FILENAME,
          row.names = FALSE)
