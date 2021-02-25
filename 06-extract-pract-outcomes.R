
#'   ***********************************************************************
#'   ***********************************************************************
#'   ***********************************************************************
#'   Initially created by: josh murray
#'   Date: September 1st 2019
#'   contact information: murrayj@smh.ca
#'
#'   Script contents: This script extracts outcomes at every
#'   prediction time point for each patient. I.e. A patient
#'   can have several outcomes throughout the course of the 
#'   summer
#'   
#'
#'   ***********************************************************************
#'   ***********************************************************************
#'   ***********************************************************************

# extract outcomes for patient predictions --------------------------------

# load the libraries
library(CHART)
library(tidyverse)
library(lubridate)
library(chartdb)


# connect to the edw ------------------------------------------------------
options("keyring_backend" = "file",
        stringsAsFactors = F)
keyring_unlock(keyring = "database",
               password = Sys.getenv("R_KEYRING_PASSWORD"))

con <- chartdb::connect_edw(
  username = keyring::key_get(service = "EDW",
                              username = "EDW_USERNAME",
                              keyring = "database"),
  password = keyring::key_get(service = "EDW",
                              username = "EDW_PASSWORD",
                              keyring = "database"))



# load clinician prediction -----------------------------------------------
all_predictions <- readr::read_csv("/mnt/research/LKS-CHART/Projects/gim_ews_preassessment_project/data/practitioner_predictions.csv")


# a tibble with every time a prediction was made --------------------------
distinct_prediction_times <- all_predictions %>% 
  filter(!is.na(ENCOUNTER_NUM)) %>% 
  distinct(ENCOUNTER_NUM, timestamp)



# cleaned up function to extract patient adt ------------------------------


get_adt <- function (edw_con, location_or_service = "location") {
  location_or_service <- tolower(location_or_service)
  if (!location_or_service %in% c("location", "service")) {
    stop("location_or_service must be either location or service")
  }
  patient_adm_event <- dim_tbl(edw_con, "PATIENT_ADMIN_EVENT_FACT") %>% 
    select(-ENCOUNTER_NUM)
  inpatients <- dim_tbl(edw_con, "INPATIENT_ENCOUNTER_FACT") %>% 
    select(ENCOUNTER_NUM, INPATIENT_ENCOUNTER_DK)
  patient_adm_event <- patient_adm_event %>% inner_join(inpatients, 
                                                        by = c(ENCOUNTER_DK = "INPATIENT_ENCOUNTER_DK"))
  service_type <- dim_tbl(edw_con, "SERVICE_TYPE_DIM") %>% 
    select(SERVICE_TYPE_DK, SERVICE = SERVICE_TYPE_DESCR, 
           SERVICE_CD = SERVICE_TYPE_CD)
  location <- dim_tbl(edw_con, "POINT_OF_CARE_LOCATION_DIM") %>% 
    select(POINT_OF_CARE_LOCATION_DK, UNIT = UNIT_NUM, UNIT_DESCR, 
           LOCATION_SERVICE = LOCATION_SERVICE_DESCR)
  patient_adm_event <- patient_adm_event %>% filter(EVENT_TYPE_CD != 
                                                      "PRE-ADMIT")
  patient_adm_event <- patient_adm_event %>% select(ENCOUNTER_NUM, 
                                                    EVENT_TYPE_CD, EVENT_TS, SERVICE_TO_DK, SERVICE_FROM_DK, 
                                                    LOCATION_TO_DK, LOCATION_FROM_DK) %>% left_join(service_type, 
                                                                                                    by = c(SERVICE_FROM_DK = "SERVICE_TYPE_DK")) %>% 
    rename(FROM_SERVICE = SERVICE, FROM_SERVICE_CD = SERVICE_CD) %>% 
    left_join(location, by = c(LOCATION_FROM_DK = "POINT_OF_CARE_LOCATION_DK")) %>% 
    rename(FROM_UNIT = UNIT, FROM_LOCATION_SERVICE = LOCATION_SERVICE, 
           FROM_UNIT_DESCR = UNIT_DESCR) %>% left_join(service_type, 
                                                       by = c(SERVICE_TO_DK = "SERVICE_TYPE_DK")) %>% 
    left_join(location, by = c(LOCATION_TO_DK = "POINT_OF_CARE_LOCATION_DK")) %>% 
    select(-SERVICE_TO_DK, -LOCATION_TO_DK, -LOCATION_FROM_DK, 
           -SERVICE_FROM_DK) %>% collect()
  return(patient_adm_event)
}


#' get_location_timeseries
#' Update function since all patients in this cohort have been discharged at this point
#'
#' @param data 
#' @param location_or_service 
#'
#' @return
#' @export
#'
#' @examples
get_location_timeseries <- function (data, location_or_service = "location") 
{
  if (!location_or_service %in% c("location", "service")) {
    stop("location_or_service must be either location or service")
  }
  still_admitted <- data %>% group_by(ENCOUNTER_NUM) %>% summarize(discharged = max(EVENT_TYPE_CD == 
                                                                                      "DISCHARGE"))
  data <- data %>% left_join(still_admitted, by = "ENCOUNTER_NUM")
  dishcarged <- data %>% filter(discharged == 1)
  not_discharged = data %>% filter(discharged == 0)
  if (location_or_service == "location") {
    dishcarged <- dishcarged %>% group_by(ENCOUNTER_NUM) %>% 
      mutate(lag_unit = lag(UNIT_DESCR), lag_time = lag(EVENT_TS)) %>% 
      select(ENCOUNTER_NUM, EVENT_TYPE_CD, EVENT_TS, FROM_UNIT, 
             FROM_SERVICE, UNIT_DESCR, UNIT, lag_unit, lag_time) %>% 
      mutate(time_spent = as.numeric(difftime(EVENT_TS, 
                                              lag_time, units = "days"))) %>% select(ENCOUNTER_NUM, 
                                                                                     from_time = lag_time, to_time = EVENT_TS, unit = lag_unit, 
                                                                                     time_spent) %>% filter(!is.na(unit)) %>% group_by(ENCOUNTER_NUM) %>% 
      mutate(unit_seq = seq_id(unit)) %>% group_by(ENCOUNTER_NUM, 
                                                   unit_seq) %>% summarize(unit = max(unit), start_time = min(from_time), 
                                                                           end_time = max(to_time), los = sum(time_spent)) %>% 
      ungroup()
    
    results <- dishcarged
  }
  else {
    dishcarged <- dishcarged %>% group_by(ENCOUNTER_NUM) %>% 
      mutate(lag_service = lag(SERVICE), lag_time = lag(EVENT_TS)) %>% 
      select(ENCOUNTER_NUM, EVENT_TYPE_CD, EVENT_TS, FROM_UNIT, 
             FROM_SERVICE, UNIT_DESCR, UNIT, lag_service, 
             lag_time) %>% mutate(time_spent = as.numeric(difftime(EVENT_TS, 
                                                                   lag_time, units = "days"))) %>% select(ENCOUNTER_NUM, 
                                                                                                          from_time = lag_time, to_time = EVENT_TS, service = lag_service, 
                                                                                                          time_spent) %>% filter(!is.na(service)) %>% group_by(ENCOUNTER_NUM) %>% 
      mutate(unit_seq = seq_id(service)) %>% group_by(ENCOUNTER_NUM, 
                                                      unit_seq) %>% summarize(service = max(service), start_time = min(from_time), 
                                                                              end_time = max(to_time), los = sum(time_spent)) %>% 
      ungroup()
    
    results <- dishcarged
  }
  return(results)
}


# extract patient adt for encounters in study -----------------------------
adt <- get_adt(con)
adt <- adt %>% 
  filter(ENCOUNTER_NUM %in% unique(all_predictions$ENCOUNTER_NUM))
adt_service_ts <- get_location_timeseries(adt, location_or_service = 'service')

# distinct encounters
encounters <- distinct_prediction_times %>% 
  distinct(ENCOUNTER_NUM)


# get inpatient data ------------------------------------------------------
inp <- dim_tbl(con, "INPATIENT_ENCOUNTER_FACT") %>% 
  select(ENCOUNTER_NUM, ADMIT_TS, DISCHARGE_TS, ADT_DISCHARGE_DISPOSITION_DK)

disp <- dim_tbl(con, "DISCHARGE_DISPOSITION_DIM") %>% 
  select(DISCHARGE_DISPOSITION_DK, DISCHARGE_DISPOSITION_CD,
         DISCHARGE_DISPOSITION_DESCR)

inp <- inp %>% 
  left_join(disp, by = c("ADT_DISCHARGE_DISPOSITION_DK" = "DISCHARGE_DISPOSITION_DK")) %>% 
  collect() %>% 
  filter(ENCOUNTER_NUM %in% encounters$ENCOUNTER_NUM) %>% 
  select(-ADT_DISCHARGE_DISPOSITION_DK)


# list of ICUS ------------------------------------------------------------
icus <- c("INTENSIVE CARE ACUTE CARE GENERAL SURG",
          "INTENSIVE CARE CARDIOVASCULAR",
          "INTENSIVE CARE MEDICAL",
          "INTENSIVE CARE NEURO SURGERY",
          "INTENSIVE CARE TRAUMA",
          "INTENSIVE CORONARY CARE")


# list to store data ------------------------------------------------------
outcome_data <- list()

for(i in 1:nrow(distinct_prediction_times)) {
  
  # the prediction time for this encounter
  tmp <- distinct_prediction_times %>% 
    slice(i)
  
  # the encounter number
  aa <- as.character(tmp$ENCOUNTER_NUM)
  
  # the time of the prediction
  tt <- tmp$timestamp
  
  # the service timeseries for this patient
  tmp_service <- adt_service_ts %>% 
    filter(ENCOUNTER_NUM == distinct_prediction_times$ENCOUNTER_NUM[i]) %>% 
    filter(start_time > tt)
  
  # the inpatient data for this encounter
  tmp_inp <- inp %>% 
    filter(ENCOUNTER_NUM == distinct_prediction_times$ENCOUNTER_NUM[i])
  
  tmp_inp <- tmp_inp %>% 
    rename(OUTCOME_TS = DISCHARGE_TS) %>% 
    mutate(death = ifelse(DISCHARGE_DISPOSITION_DESCR == 'Expired                             *EXP',
                          1,0))
  
  icu_ts <- tmp_service %>% 
    filter(service %in% icus)
  
  pal_ts <- tmp_service %>% 
    filter(service %in% "PALLIATIVE")
  
  pal_outcome <- tmp_inp %>% 
    filter(DISCHARGE_DISPOSITION_DESCR %in% 
             c("Palliative - Acute Hospital"))
  
  # create outcome values for this prediction time
  
  
  # death -------------------------------------------------------------------
  death <- tmp_inp$death
  
  if(death> 0) {
    death_ts <- tmp_inp$OUTCOME_TS
  } else {
    death_ts <- NA_Date_
  }
  
  
  # icu transfer ------------------------------------------------------------
  icu <- ifelse(nrow(icu_ts) > 0, 1, 0)
  
  if(icu> 0) {
    icu_ts <- min(icu_ts$start_time)
  } else {
    icu_ts <- NA_Date_
  }
  
  
  # palliative care ---------------------------------------------------------
  pal <- ifelse(nrow(pal_outcome) > 0 | nrow(pal_ts) > 0, 1, 0)
  
  if(pal > 0) {
    if(nrow(pal_ts) > 0) {
      pal_ts <- min(pal_ts$start_time)
    } else {
      pal_ts <- min(pal_outcome$OUTCOME_TS)
    }
  } else {
    pal_ts <- NA_Date_
  }
  
  discharge_ts <- tmp_inp$OUTCOME_TS
  
  outcome <- ifelse(icu ==0  & death == 0 & pal == 0, 0, 1)
  
  #outcome_ts <- pmin(death_ts, icu_ts, pal_ts, discharge_ts, na.rm = T)
  
  
  tmp_outcomes <- tmp %>% 
    mutate(death = death, 
           death_ts = death_ts,
           icu = icu,
           icu_ts = icu_ts,
           pal = pal,
           pal_ts = pal_ts,
           outcome = outcome,
           discharge_ts = discharge_ts)
  
  tmp_outcomes <- tmp_outcomes %>% 
    mutate(OUTCOME_TYPE = radiant.data::which.pmin(icu_ts,
                                                   death_ts,
                                                   pal_ts,
                                                   discharge_ts),
           OUTCOME = ifelse((OUTCOME_TYPE  %in% 1:2), 1, 0))
  
  # Extract the outcome time
  tmp_outcomes <- tmp_outcomes %>% 
    mutate(OUTCOME_TS = case_when(
      OUTCOME_TYPE == 1 ~ ymd_hms(icu_ts),
      OUTCOME_TYPE == 2 ~ ymd_hms(death_ts),
      OUTCOME_TYPE == 3~ ymd_hms(pal_ts),
      TRUE ~ ymd_hms(discharge_ts)))
  
  outcome_data[[i]] <- tmp_outcomes
  print(i)
}

outcome_data <- do.call(rbind, outcome_data)

all_predictions <- all_predictions %>%
  inner_join(outcome_data, by = c("timestamp", "ENCOUNTER_NUM"))


all_predictions <- all_predictions %>%
  mutate(outcome48 = ifelse(outcome == 1 &
                              as.numeric(difftime(OUTCOME_TS, timestamp, units = "hours")) <= 48, 1, 0))


readr::write_csv(all_predictions, '/mnt/research/LKS-CHART/Projects/gim_ews_preassessment_project/data/practitioner_predictions_outcomes.csv')


# Get correct time to outcome ---------------------------------------------

# To compute time to outcome, need to retrieve GIM_START_TS
data_folder <- "/mnt/research/LKS-CHART/Projects/gim_ews_preassessment_project/data/prediction_frames/"

all_encounters <- list()
i <- 1
for (input_folder in list.files(data_folder, full.names = TRUE)) {
  if (dir.exists(input_folder)) {
    encounters <- read.csv(file.path(input_folder, "gim_encounters.csv"), stringsAsFactors = FALSE)
    all_encounters[[i]] <- encounters
    i <- i + 1
  }
}

service_start_ts <- do.call(rbind, all_encounters) %>%
  select(ENCOUNTER_NUM, GIM_START_TS, service, SERVICE_START_TS) %>%
  unique() %>%
  mutate(GIM_START_TS = ymd_hms(GIM_START_TS))

outcome_events <- all_predictions %>%
  filter(OUTCOME_TYPE  != 4) %>%
  select(ENCOUNTER_NUM, OUTCOME_TS, OUTCOME_TYPE) %>%
  unique()


inp %>% 
  filter(ENCOUNTER_NUM %in% all_predictions$ENCOUNTER_NUM) %>% 
  mutate(ENCOUNTER_NUM = as.numeric(ENCOUNTER_NUM)) %>%
  left_join(service_start_ts, by = "ENCOUNTER_NUM") %>%
  
  # Get first time patient is on GIM
  group_by(ENCOUNTER_NUM) %>%
  arrange(GIM_START_TS) %>%
  slice(1) %>%
  ungroup() %>%
  
  # Limit to 1 outcome/encounter
  left_join(outcome_events, by = "ENCOUNTER_NUM") %>%
  group_by(ENCOUNTER_NUM) %>%
  arrange(OUTCOME_TS) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(gim_to_outcome = difftime(OUTCOME_TS, GIM_START_TS, unit = "days")) %>% 
  group_by(OUTCOME_TYPE) %>%
  summarize(mean(gim_to_outcome, na.rm = TRUE),
            median(gim_to_outcome, na.rm = TRUE))



inp %>% 
  filter(ENCOUNTER_NUM %in% all_predictions$ENCOUNTER_NUM) %>% 
  mutate(ENCOUNTER_NUM = as.numeric(ENCOUNTER_NUM)) %>% 
  left_join(outcome_events, by = "ENCOUNTER_NUM") %>%
  group_by(ENCOUNTER_NUM) %>%
  arrange(OUTCOME_TS) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(gim_to_outcome = difftime(OUTCOME_TS, ADMIT_TS, unit = "days")) %>% 
  group_by(OUTCOME_TYPE) %>%
  summarize(mean(time_to_outcome),
            median(time_to_outcome))

# Figure out when prediction happens in pt timeline

adt_service_ts_predictions <- all_predictions %>% 
  select(ENCOUNTER_NUM, timestamp) %>%
  mutate(ENCOUNTER_NUM = as.character(ENCOUNTER_NUM)) %>%
  right_join(adt_service_ts, by = "ENCOUNTER_NUM") %>%
  filter(timestamp >= start_time & timestamp <= end_time) %>%
  select(ENCOUNTER_NUM, start_time, end_time) %>%
  unique() %>%
  mutate(prediction = 1)

# Use firt GIM start ts as outcome
adt_service_ts %>%
  left_join(adt_service_ts_predictions, by = c("ENCOUNTER_NUM", "start_time", "end_time")) %>%
  replace(is.na(.), 0) %>%
  filter(prediction == 1) %>%
  group_by(ENCOUNTER_NUM) %>%
  summarize(
    gim_start_ts = min(start_time),
    service = service[1]
  ) %>%
  ungroup() %>%
  
  mutate(ENCOUNTER_NUM = as.numeric(ENCOUNTER_NUM)) %>%
  left_join(outcome_events, by = "ENCOUNTER_NUM") %>%
  group_by(ENCOUNTER_NUM) %>%
  arrange(OUTCOME_TS) %>%
  slice(1) %>%
  ungroup() %>%
  
  
  # Remove encounters outside of prospective test set
  filter(gim_start_ts >= as.Date("2019-05-01") & OUTCOME_TS <= as.Date("2019-10-01")) %>%
  mutate(gim_to_outcome = difftime(OUTCOME_TS, gim_start_ts, unit = "days")) %>% 
  
  
  group_by(OUTCOME_TYPE) %>%
  summarize(mean(gim_to_outcome, na.rm = TRUE),
            median(gim_to_outcome, na.rm = TRUE), n = n())



# Get all outcomes --------------------------------------------------------

events <- chartwatch::get_outcome_events(con, encounter_vector = unique(all_predictions$ENCOUNTER_NUM))
missing_events <- events %>% 
  filter(EVENT_TS >= as.Date("2019-05-01") & EVENT_TS <= as.Date("2020-01-01")) %>% 
  mutate(ENCOUNTER_NUM = as.double(ENCOUNTER_NUM)) %>% 
  left_join(outcome_events, by = c("ENCOUNTER_NUM", "EVENT_TS" = "OUTCOME_TS")) %>% filter(is.na(OUTCOME_TYPE))
