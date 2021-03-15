library(tidyverse)
library(lubridate)
library(glmnet)
source("./constants.R")

calculate_hews <- function(data) {
  hews_data <- data %>% 
    mutate(pulse = ifelse(vital_spulse <= 40, 2,
                          ifelse(vital_spulse <= 50, 1,
                                 ifelse(vital_spulse <= 100, 0,
                                        ifelse(vital_spulse <= 110, 1,
                                               ifelse(vital_spulse <= 130, 2, 3))))),
           systolic_bp = ifelse(vital_sbpsystolic <= 70, 3,
                                ifelse(vital_sbpsystolic <= 90, 2,
                                       ifelse(vital_sbpsystolic <= 170, 0,
                                              ifelse(vital_sbpsystolic <= 200, 2, 3)))),
           resp_rate = ifelse(vital_srespirations <= 8, 3,
                              ifelse(vital_srespirations <= 13, 2,
                                     ifelse(vital_srespirations <= 20, 0,
                                            ifelse(vital_srespirations <= 30, 2, 3)))),
           temp = ifelse(vital_stemperature <= 35, 3,
                         ifelse(vital_stemperature <= 36, 1, 
                                ifelse(vital_stemperature <= 37.9, 0,
                                       ifelse(vital_stemperature <= 39, 1, 2)))),
           o2_sat = ifelse(vital_so2saturation <= 85, 3, 
                           ifelse(vital_so2saturation <= 92, 1, 0)),
           flow_rate = flow_rate,
           cns = ifelse(cam == 1, 2, alc)) %>% 
    mutate(HEWS = pulse + systolic_bp + resp_rate + temp + flow_rate + cns,
           HEWS_group = ifelse(HEWS <3, "low risk",
                               ifelse(HEWS <6, "moderate risk",
                                      ifelse(HEWS < 9, "high risk",
                                             "very high risk")))) %>% 
    mutate(HEWS_group = factor(HEWS_group, levels = c("low risk", 'moderate risk', "high risk", "very high risk"))) %>% 
    select(ENCOUNTER_NUM, timestamp, HEWS, HEWS_group)
  
  
}

calculate_news <- function(data) {
  news_data <- data%>% 
    mutate(pulse = ifelse(vital_spulse <= 40, 3,
                          ifelse(vital_spulse <= 50, 1,
                                 ifelse(vital_spulse <= 90, 0,
                                        ifelse(vital_spulse <= 110, 1,
                                               ifelse(vital_spulse <= 130, 2, 3))))),
           systolic_bp = ifelse(vital_sbpsystolic <= 90, 3,
                                ifelse(vital_sbpsystolic <= 100, 2,
                                       ifelse(vital_sbpsystolic <= 110, 1,
                                              ifelse(vital_sbpsystolic <= 219,0, 3 )))),
           resp_rate = ifelse(vital_srespirations <= 8, 3,
                              ifelse(vital_srespirations <= 11, 1,
                                     ifelse(vital_srespirations <= 20, 0,
                                            ifelse(vital_srespirations <= 24, 2, 3)))),
           temp = ifelse(vital_stemperature <= 35, 3,
                         ifelse(vital_stemperature <= 36, 1, 
                                ifelse(vital_stemperature <= 38, 0,
                                       ifelse(vital_stemperature <= 39, 1, 2)))),
           o2_sat = ifelse(vital_so2saturation <= 91, 3, 
                           ifelse(vital_so2saturation <= 93, 2, 
                                  ifelse(vital_so2saturation <= 95, 1, 0))),
           flow_rate = ifelse(flow_rate == 2, 0, 1),
           cns = ifelse(cam == 2 | alc != 1, 0, 3)) %>% 
    mutate(NEWS = pulse + systolic_bp + resp_rate + temp + flow_rate + cns,
           NEWS_group = ifelse(NEWS <3, "low risk",
                               ifelse(NEWS <6, "moderate risk",
                                      ifelse(NEWS < 9, "high risk",
                                             "very high risk")))) %>% 
    mutate(HEWS_group = factor(NEWS_group, levels = c("low risk", 'moderate risk', "high risk", "very high risk"))) %>% 
    select(ENCOUNTER_NUM,timestamp, pulse, systolic_bp, resp_rate, temp, flow_rate, cns, NEWS, NEWS_group)
  return(news_data)
  
} 


load(DESCRIPTIVE_STATS_FILE)

#nsd <- numeric_descriptive_statistics[names(numeric_timeseries)]
#fs <- '/media/ubuntu/cryptscratch2/gim-pre-assessment'
#dirs <- list.files(fs, full.names = T)
#dirs <- dirs[1:68]

predictions_folder <- "/mnt/research/LKS-CHART/Projects/gim_ews_preassessment_project/data/prediction_frames/"
all_hews_news <- list()

for (date in list.files(predictions_folder)) {
  folder_name <- file.path(predictions_folder, date)
  if (dir.exists(folder_name)) {
    
    encounters <- readr::read_csv(paste0(folder_name, "/gim_encounters.csv"))
    
    time <- lubridate::ymd_hms(unique(encounters$current_time))
    hews_news_vars <-  readr::read_csv(paste0(folder_name, "/hews_news_vars.csv")) %>%
      dplyr::mutate(timestamp = lubridate::ymd_hms(timestamp)) %>%
      dplyr::filter(timestamp <= time) %>%
      dplyr::group_by(ENCOUNTER_NUM) %>%
      dplyr::arrange(desc(timestamp)) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::select(-timestamp)
    
    encounters <- encounters %>% 
      rowwise() %>% 
      mutate(start_time = max(GIM_START_TS, ADMIT_TS, na.rm = T)) %>% 
      ungroup() %>% 
      mutate(start_time = if_else(start_time > time, time, start_time))
    
    vitals_data <- read.csv(file.path(folder_name, "processed_vitals.csv")) %>% 
      dplyr::mutate(timestamp = lubridate::ymd_hms(timestamp)) %>%
      dplyr::filter(timestamp <=  time) %>%
      dplyr::select(ENCOUNTER_NUM, timestamp, vital_spulse, 
                    vital_sbpsystolic, vital_srespirations,
                    vital_stemperature, vital_so2saturation) %>%
      group_by(ENCOUNTER_NUM) %>%
      arrange(timestamp) %>%
      # Fill forward
      tidyr::fill(c("vital_spulse", "vital_sbpsystolic", 
                    "vital_srespirations", "vital_stemperature", 
                    "vital_so2saturation")) %>%
      # Only keep most recent value
      dplyr::slice(1) %>%
      dplyr::ungroup() 
    
    hews_news <- vitals_data %>%
      dplyr::left_join(hews_news_vars)
    
    hews_data <- calculate_hews(hews_news)
    news_data <- calculate_news(hews_news)
    
    hews_news_data <- hews_data %>% 
      left_join(news_data)
  }
}
for(i in dirs) {
  
  
  numeric_timeseries <- readr::read_csv(paste0(i, "/numeric_timeseries.csv"))
  numeric_timeseries <- numeric_timeseries %>% 
    select(ENCOUNTER_NUM, time_window, vital_spulse, 
           vital_sbpsystolic, vital_srespirations,
           vital_stemperature, vital_so2saturation)
  
  
  patient_ts <- create_patient_timeseries(encounters, time = time)
  
  patient_ts <- patient_ts %>% 
    group_by(ENCOUNTER_NUM, time_window =timestamp_6hr ) %>% 
    summarize(timestamp = max(timestamp)) %>% 
    select(-timestamp) %>% 
    ungroup()
  
  numeric_timeseries <- numeric_timeseries %>% 
    mutate(vital_spulse = vital_spulse*(nsd[['vital_spulse']]$q99  - nsd[['vital_spulse']]$q01) + nsd[['vital_spulse']]$q01,
           vital_sbpsystolic = vital_sbpsystolic*(nsd[['vital_sbpsystolic']]$q99  - nsd[['vital_sbpsystolic']]$q01) + nsd[['vital_sbpsystolic']]$q01,
           vital_srespirations = vital_spulse*(nsd[['vital_srespirations']]$q99  - nsd[['vital_srespirations']]$q01) + nsd[['vital_srespirations']]$q01,
           vital_stemperature = vital_spulse*(nsd[['vital_stemperature']]$q99  - nsd[['vital_stemperature']]$q01) + nsd[['vital_stemperature']]$q01,
           vital_so2saturation = vital_spulse*(nsd[['vital_so2saturation']]$q99  - nsd[['vital_so2saturation']]$q01) + nsd[['vital_so2saturation']]$q01)
  hews_news <- patient_ts %>% 
    left_join(numeric_timeseries) %>% 
    left_join(hews_news_vars)
  
  hews_data <- calculate_hews(hews_news)
  hews_data <- hews_data %>% 
    group_by(ENCOUNTER_NUM) %>% 
    arrange(desc(time_window)) %>% 
    filter(row_number() == 1) %>% 
    ungroup() %>% 
    select(ENCOUNTER_NUM, time_window, HEWS, HEWS_group)
  
  news_data <- calculate_news(hews_news)
  news_data <- news_data %>% 
    group_by(ENCOUNTER_NUM) %>% 
    arrange(desc(time_window)) %>% 
    filter(row_number() == 1) %>% 
    ungroup() %>% 
    select(ENCOUNTER_NUM, time_window, NEWS, NEWS_group)
  
  hews_news_data <- hews_data %>% 
    left_join(news_data)
  all_hews_news[[i]] <- hews_news_data
  print(i)
}

save(all_hews_news, file = "/media/ubuntu/cryptscratch2/gim-pre-assessment/hews-news-pred.R")



# numeric data ------------------------------------------------------------
load(paste0(fpc, "numeric_timeseries.Rda"))
load(paste0(fp,"gim_encounters.Rda"))
load(paste0(fpc,"encounter_ts.Rda"))
text_file_name <- paste0(fp, "text_soarian.Rda")
load(text_file_name)

hews_vars <- numeric_timeseries %>% 
  select(ENCOUNTER_NUM, timestamp, vital_spulse, 
         vital_sbpsystolic, vital_srespirations,
         vital_stemperature, vital_so2saturation)



# FLOW RATE ---------------------------------------------------------------
flow_rate <- text_soarian %>% 
  filter(FINDINGNAME == "Oxygen Flow Rate (L/Min)")
flow_rate <- flow_rate %>% 
  mutate(VALUE = gsub("\\..", "\\.", VALUE))
flow_rate <- flow_rate %>% 
  mutate(room_air = ifelse(grepl("r", VALUE, ignore.case = T), 1, 0),
         numeric_flow = as.numeric(gsub("([0-9]+).*$", "\\1", VALUE)),
         value = ifelse(room_air == 1, -1, numeric_flow),
         flow_rate = ifelse(value == -1, 0,
                            ifelse(value <= 5, 1, 2)))



flow_rate <- flow_rate %>% 
  filter(!is.na(flow_rate)) %>% 
  select(ENCOUNTER_NUM, ass_ObjectID, FORMUSAGEDISPLAYNAME,
         FINDINGNAME, OBS_CREATIONTIME, 
         flow_rate, VALUE) %>% 
  mutate(timestamp = lubridate::ceiling_date(OBS_CREATIONTIME, unit = 'hour')) %>% 
  group_by(ENCOUNTER_NUM, timestamp) %>% 
  summarize(flow_rate = max(flow_rate, na.rm = T))
#   -----------------------------------------------------------------------


# CAM ---------------------------------------------------------------------
cam <- text_soarian %>% 
  filter(FINDINGNAME == "CAM Results") %>% 
  mutate(cam = ifelse(VALUE == "Positive", 1, 0)) %>% 
  mutate(timestamp = lubridate::ceiling_date(OBS_CREATIONTIME, unit = 'hour')) %>% 
  group_by(ENCOUNTER_NUM, timestamp) %>% 
  summarize(cam = max(cam, na.rm = T))
#   -----------------------------------------------------------------------


# ALTERNATE LEVEL OF CONSCIOUSNESS ----------------------------------------
# alert  is alert
# coma is unresponsive
# lethargic is voice
# stupour is pain
# hyper alert -> CAM positive
alc <- text_soarian %>% 
  filter(FINDINGNAME == "Altered Level of Consciousness") %>% 
  mutate(alc = case_when(
    VALUE == "Alert (normal)" ~ '0',
    VALUE == "Lethargic (drowsy, easily aroused)" ~ '1',
    VALUE == "Stupor (difficult to arouse)" ~ '2',
    VALUE == "Coma (can't arouse)" ~ '3',
    VALUE == "Vigilant (hyper-alert)" ~ '2',
    TRUE ~ ''
  ),
  alc = as.numeric(alc)) %>% 
  filter(!is.na(alc)) %>% 
  mutate(timestamp = lubridate::ceiling_date(OBS_CREATIONTIME, unit = 'hour')) %>% 
  group_by(ENCOUNTER_NUM, timestamp) %>% 
  summarize(alc = max(alc, na.rm = T))
#   -----------------------------------------------------------------------



# JOIN TOGETHER -----------------------------------------------------------




encounter_ts <- encounter_ts %>% 
  left_join(hews_vars, by = c("ENCOUNTER_NUM", "timestamp")) %>% 
  left_join(cam, by = c("SOARIAN_ENCOUNTER_NUM" = "ENCOUNTER_NUM", "timestamp")) %>% 
  left_join(alc, by = c("SOARIAN_ENCOUNTER_NUM"  = "ENCOUNTER_NUM", "timestamp")) %>% 
  left_join(flow_rate, by = c("SOARIAN_ENCOUNTER_NUM"  = "ENCOUNTER_NUM", "timestamp"))

encounter_ts <- encounter_ts %>% 
  mutate_if(is.numeric,  funs(zoo::na.locf(., na.rm = FALSE)))

#impute
encounter_ts <- encounter_ts %>% 
  mutate(vital_spulse = ifelse(is.na(vital_spulse), mean(vital_spulse, na.rm = T), vital_spulse),
         vital_sbpsystolic = ifelse(is.na(vital_sbpsystolic), mean(vital_sbpsystolic, na.rm = T), vital_sbpsystolic),
         vital_srespirations = ifelse(is.na(vital_srespirations), mean(vital_srespirations, na.rm = T), vital_srespirations),
         vital_stemperature = ifelse(is.na(vital_stemperature), mean(vital_stemperature, na.rm = T), vital_stemperature),
         vital_so2saturation = ifelse(is.na(vital_so2saturation), mean(vital_so2saturation, na.rm = T), vital_so2saturation),
         cam = ifelse(is.na(cam), 0, cam),
         alc = ifelse(is.na(alc), 0, cam),
         flow_rate = ifelse(is.na(flow_rate), 0, flow_rate))

# Load the training encounters and testing encounters in 8 hour intervals
load(paste0(fpc, "train_numeric_data_bin_", 8, ".Rda"))
load(paste0(fpc, "test_numeric_data_bin_", 8, ".Rda"))
load(paste0(fpc, "valid_numeric_data_bin_", 8, ".Rda"))

hews8_train <- train_numeric_data %>% 
  select(ENCOUNTER_NUM, timestamp) %>% 
  left_join(encounter_ts)

hews8_train <- hews8_train %>% 
  mutate(vital_spulse = ifelse(is.na(vital_spulse), mean(vital_spulse, na.rm = T), vital_spulse),
         vital_sbpsystolic = ifelse(is.na(vital_sbpsystolic), mean(vital_sbpsystolic, na.rm = T), vital_sbpsystolic),
         vital_srespirations = ifelse(is.na(vital_srespirations), mean(vital_srespirations, na.rm = T), vital_srespirations),
         vital_stemperature = ifelse(is.na(vital_stemperature), mean(vital_stemperature, na.rm = T), vital_stemperature),
         vital_so2saturation = ifelse(is.na(vital_so2saturation), mean(vital_so2saturation, na.rm = T), vital_so2saturation),
         cam = ifelse(is.na(cam), 0, cam),
         alc = ifelse(is.na(alc), 0, cam),
         flow_rate = ifelse(is.na(flow_rate), 0, flow_rate))


hews8_test <- test_numeric_data %>% 
  select(ENCOUNTER_NUM, timestamp)%>% 
  left_join(encounter_ts)

hews8_test <- hews8_test %>% 
  mutate(vital_spulse = ifelse(is.na(vital_spulse), mean(vital_spulse, na.rm = T), vital_spulse),
         vital_sbpsystolic = ifelse(is.na(vital_sbpsystolic), mean(vital_sbpsystolic, na.rm = T), vital_sbpsystolic),
         vital_srespirations = ifelse(is.na(vital_srespirations), mean(vital_srespirations, na.rm = T), vital_srespirations),
         vital_stemperature = ifelse(is.na(vital_stemperature), mean(vital_stemperature, na.rm = T), vital_stemperature),
         vital_so2saturation = ifelse(is.na(vital_so2saturation), mean(vital_so2saturation, na.rm = T), vital_so2saturation),
         cam = ifelse(is.na(cam), 0, cam),
         alc = ifelse(is.na(alc), 0, cam),
         flow_rate = ifelse(is.na(flow_rate), 0, flow_rate))

hews8_valid <- valid_numeric_data %>% 
  select(ENCOUNTER_NUM, timestamp) %>% 
  left_join(encounter_ts)

hews8_valid <- hews8_valid %>% 
  mutate(vital_spulse = ifelse(is.na(vital_spulse), mean(vital_spulse, na.rm = T), vital_spulse),
         vital_sbpsystolic = ifelse(is.na(vital_sbpsystolic), mean(vital_sbpsystolic, na.rm = T), vital_sbpsystolic),
         vital_srespirations = ifelse(is.na(vital_srespirations), mean(vital_srespirations, na.rm = T), vital_srespirations),
         vital_stemperature = ifelse(is.na(vital_stemperature), mean(vital_stemperature, na.rm = T), vital_stemperature),
         vital_so2saturation = ifelse(is.na(vital_so2saturation), mean(vital_so2saturation, na.rm = T), vital_so2saturation),
         cam = ifelse(is.na(cam), 0, cam),
         alc = ifelse(is.na(alc), 0, cam),
         flow_rate = ifelse(is.na(flow_rate), 0, flow_rate))


# add the outcome ---------------------------------------------------------
hews8_train <- hews8_train %>% 
  left_join(gim_encounters %>% select(ENCOUNTER_NUM, OUTCOME_ALL, OUTCOME_TS)) %>% 
  mutate(outcome = ifelse(as.numeric(difftime(OUTCOME_TS, timestamp, units = 'hours')) <= 48, OUTCOME_ALL, 0))%>% 
  select(-OUTCOME_TS, -OUTCOME_ALL, -OUTCOME)

hews8_test <- hews8_test %>% 
  left_join(gim_encounters %>% select(ENCOUNTER_NUM, OUTCOME_ALL, OUTCOME_TS)) %>% 
  mutate(outcome = ifelse(as.numeric(difftime(OUTCOME_TS, timestamp, units = 'hours')) <= 48, OUTCOME_ALL, 0)) %>% 
  select(-OUTCOME_TS, -OUTCOME_ALL, -OUTCOME)

hews8_valid <- hews8_valid %>% 
  left_join(gim_encounters %>% select(ENCOUNTER_NUM, OUTCOME_ALL, OUTCOME_TS)) %>% 
  mutate(outcome = ifelse(as.numeric(difftime(OUTCOME_TS, timestamp, units = 'hours')) <= 48, OUTCOME_ALL, 0)) %>% 
  select(-OUTCOME_TS, -OUTCOME_ALL, -OUTCOME)


# categorize hews variables -----------------------------------------------

news_8_train <- hews8_train %>% 
  mutate(pulse = ifelse(vital_spulse <= 40, 3,
                        ifelse(vital_spulse <= 50, 1,
                               ifelse(vital_spulse <= 90, 0,
                                      ifelse(vital_spulse <= 110, 1,
                                             ifelse(vital_spulse <= 130, 2, 3))))),
         systolic_bp = ifelse(vital_sbpsystolic <= 90, 3,
                              ifelse(vital_sbpsystolic <= 100, 2,
                                     ifelse(vital_sbpsystolic <= 110, 1,
                                            ifelse(vital_sbpsystolic <= 219,0, 3 )))),
         resp_rate = ifelse(vital_srespirations <= 8, 3,
                            ifelse(vital_srespirations <= 11, 1,
                                   ifelse(vital_srespirations <= 20, 0,
                                          ifelse(vital_srespirations <= 24, 2, 3)))),
         temp = ifelse(vital_stemperature <= 35, 3,
                       ifelse(vital_stemperature <= 36, 1, 
                              ifelse(vital_stemperature <= 38, 0,
                                     ifelse(vital_stemperature <= 39, 1, 2)))),
         o2_sat = ifelse(vital_so2saturation <= 91, 3, 
                         ifelse(vital_so2saturation <= 93, 2, 
                                ifelse(vital_so2saturation <= 95, 1, 0))),
         flow_rate = ifelse(flow_rate == 2, 0, 1),
         cns = ifelse(cam == 2 | alc != 1, 0, 3)) %>% 
  mutate(NEWS = pulse + systolic_bp + resp_rate + temp + flow_rate + cns,
         NEWS_group = ifelse(NEWS <3, "low risk",
                             ifelse(NEWS <6, "moderate risk",
                                    ifelse(NEWS < 9, "high risk",
                                           "very high risk")))) %>% 
  mutate(HEWS_group = factor(NEWS_group, levels = c("low risk", 'moderate risk', "high risk", "very high risk"))) %>% 
  select(ENCOUNTER_NUM, pulse, systolic_bp, resp_rate, temp, flow_rate, cns, NEWS, NEWS_group, outcome)




hews8_train <- hews8_train %>% 
  mutate(pulse = ifelse(vital_spulse <= 40, 2,
                        ifelse(vital_spulse <= 50, 1,
                               ifelse(vital_spulse <= 100, 0,
                                      ifelse(vital_spulse <= 110, 1,
                                             ifelse(vital_spulse <= 130, 2, 3))))),
         systolic_bp = ifelse(vital_sbpsystolic <= 70, 3,
                              ifelse(vital_sbpsystolic <= 90, 2,
                                     ifelse(vital_sbpsystolic <= 170, 0,
                                            ifelse(vital_sbpsystolic <= 200, 2, 3)))),
         resp_rate = ifelse(vital_srespirations <= 8, 3,
                            ifelse(vital_srespirations <= 13, 2,
                                   ifelse(vital_srespirations <= 20, 0,
                                          ifelse(vital_srespirations <= 30, 2, 3)))),
         temp = ifelse(vital_stemperature <= 35, 3,
                       ifelse(vital_stemperature <= 36, 1, 
                              ifelse(vital_stemperature <= 37.9, 0,
                                     ifelse(vital_stemperature <= 39, 1, 2)))),
         o2_sat = ifelse(vital_so2saturation <= 85, 3, 
                         ifelse(vital_so2saturation <= 92, 1, 0)),
         flow_rate = flow_rate,
         cns = ifelse(cam == 1, 2, alc)) %>% 
  mutate(HEWS = pulse + systolic_bp + resp_rate + temp + flow_rate + cns,
         HEWS_group = ifelse(HEWS <3, "low risk",
                             ifelse(HEWS <6, "moderate risk",
                                    ifelse(HEWS < 9, "high risk",
                                           "very high risk")))) %>% 
  mutate(HEWS_group = factor(HEWS_group, levels = c("low risk", 'moderate risk', "high risk", "very high risk"))) %>% 
  select(ENCOUNTER_NUM, time_elapsed, timestamp, pulse, systolic_bp, resp_rate, temp, o2_sat, flow_rate, cns, HEWS, HEWS_group, outcome)

news_8_test <- hews8_test %>% 
  mutate(pulse = ifelse(vital_spulse <= 40, 3,
                        ifelse(vital_spulse <= 50, 1,
                               ifelse(vital_spulse <= 90, 0,
                                      ifelse(vital_spulse <= 110, 1,
                                             ifelse(vital_spulse <= 130, 2, 3))))),
         systolic_bp = ifelse(vital_sbpsystolic <= 90, 3,
                              ifelse(vital_sbpsystolic <= 100, 2,
                                     ifelse(vital_sbpsystolic <= 110, 1,
                                            ifelse(vital_sbpsystolic <= 219,0, 3 )))),
         resp_rate = ifelse(vital_srespirations <= 8, 3,
                            ifelse(vital_srespirations <= 11, 1,
                                   ifelse(vital_srespirations <= 20, 0,
                                          ifelse(vital_srespirations <= 24, 2, 3)))),
         temp = ifelse(vital_stemperature <= 35, 3,
                       ifelse(vital_stemperature <= 36, 1, 
                              ifelse(vital_stemperature <= 38, 0,
                                     ifelse(vital_stemperature <= 39, 1, 2)))),
         o2_sat = ifelse(vital_so2saturation <= 91, 3, 
                         ifelse(vital_so2saturation <= 93, 2, 
                                ifelse(vital_so2saturation <= 95, 1, 0))),
         flow_rate = ifelse(flow_rate == 2, 0, 1),
         cns = ifelse(cam == 2 | alc != 1, 0, 3)) %>% 
  mutate(NEWS = pulse + systolic_bp + resp_rate + temp + flow_rate + cns,
         NEWS_group = ifelse(NEWS <3, "low risk",
                             ifelse(NEWS <6, "moderate risk",
                                    ifelse(NEWS < 9, "high risk",
                                           "very high risk")))) %>% 
  mutate(HEWS_group = factor(NEWS_group, levels = c("low risk", 'moderate risk', "high risk", "very high risk"))) %>% 
  select(ENCOUNTER_NUM, pulse, systolic_bp, resp_rate, temp, flow_rate, cns, NEWS, NEWS_group, outcome)


hews8_test <- hews8_test %>% 
  mutate(pulse = ifelse(vital_spulse <= 40, 2,
                        ifelse(vital_spulse <= 50, 1,
                               ifelse(vital_spulse <= 100, 0,
                                      ifelse(vital_spulse <= 110, 1,
                                             ifelse(vital_spulse <= 130, 2, 3))))),
         systolic_bp = ifelse(vital_sbpsystolic <= 70, 3,
                              ifelse(vital_sbpsystolic <= 90, 2,
                                     ifelse(vital_sbpsystolic <= 170, 0,
                                            ifelse(vital_sbpsystolic <= 200, 2, 3)))),
         resp_rate = ifelse(vital_srespirations <= 8, 3,
                            ifelse(vital_srespirations <= 13, 2,
                                   ifelse(vital_srespirations <= 20, 0,
                                          ifelse(vital_srespirations <= 30, 2, 3)))),
         temp = ifelse(vital_stemperature <= 35, 3,
                       ifelse(vital_stemperature <= 36, 1, 
                              ifelse(vital_stemperature <= 37.9, 0,
                                     ifelse(vital_stemperature <= 39, 1, 2)))),
         o2_sat = ifelse(vital_so2saturation <= 85, 3, 
                         ifelse(vital_so2saturation <= 92, 1, 0)),
         flow_rate = flow_rate,
         cns = ifelse(cam == 1, 2, alc)) %>% 
  mutate(HEWS = pulse + systolic_bp + resp_rate + temp + flow_rate + cns,
         HEWS_group = ifelse(HEWS <3, "low risk",
                             ifelse(HEWS <6, "moderate risk",
                                    ifelse(HEWS < 9, "high risk",
                                           "very high risk")))) %>% 
  mutate(HEWS_group = factor(HEWS_group, levels = c("low risk", 'moderate risk', "high risk", "very high risk"))) %>% 
  select(ENCOUNTER_NUM, time_elapsed, timestamp, pulse, systolic_bp, resp_rate, temp, o2_sat, flow_rate, cns, HEWS, HEWS_group, outcome)

hews8_valid <- hews8_valid %>% 
  mutate(pulse = ifelse(vital_spulse <= 40, 2,
                        ifelse(vital_spulse <= 50, 1,
                               ifelse(vital_spulse <= 100, 0,
                                      ifelse(vital_spulse <= 110, 1,
                                             ifelse(vital_spulse <= 130, 2, 3))))),
         systolic_bp = ifelse(vital_sbpsystolic <= 70, 3,
                              ifelse(vital_sbpsystolic <= 90, 2,
                                     ifelse(vital_sbpsystolic <= 170, 0,
                                            ifelse(vital_sbpsystolic <= 200, 2, 3)))),
         resp_rate = ifelse(vital_srespirations <= 8, 3,
                            ifelse(vital_srespirations <= 13, 2,
                                   ifelse(vital_srespirations <= 20, 0,
                                          ifelse(vital_srespirations <= 30, 2, 3)))),
         temp = ifelse(vital_stemperature <= 35, 3,
                       ifelse(vital_stemperature <= 36, 1, 
                              ifelse(vital_stemperature <= 37.9, 0,
                                     ifelse(vital_stemperature <= 39, 1, 2)))),
         o2_sat = ifelse(vital_so2saturation <= 85, 3, 
                         ifelse(vital_so2saturation <= 92, 1, 0)),
         flow_rate = flow_rate,
         cns = ifelse(cam == 1, 2, alc)) %>% 
  mutate(HEWS = pulse + systolic_bp + resp_rate + temp + flow_rate + cns,
         HEWS_group = ifelse(HEWS <3, "low risk",
                             ifelse(HEWS <6, "moderate risk",
                                    ifelse(HEWS < 9, "high risk",
                                           "very high risk")))) %>% 
  mutate(HEWS_group = factor(HEWS_group, levels = c("low risk", 'moderate risk', "high risk", "very high risk"))) %>% 
  select(ENCOUNTER_NUM, time_elapsed, timestamp, pulse, systolic_bp, resp_rate, temp, o2_sat, flow_rate, cns, HEWS, HEWS_group, outcome)


news_8_valid <- hews8_valid %>% 
  mutate(pulse = ifelse(vital_spulse <= 40, 3,
                        ifelse(vital_spulse <= 50, 1,
                               ifelse(vital_spulse <= 90, 0,
                                      ifelse(vital_spulse <= 110, 1,
                                             ifelse(vital_spulse <= 130, 2, 3))))),
         systolic_bp = ifelse(vital_sbpsystolic <= 90, 3,
                              ifelse(vital_sbpsystolic <= 100, 2,
                                     ifelse(vital_sbpsystolic <= 110, 1,
                                            ifelse(vital_sbpsystolic <= 219,0, 3 )))),
         resp_rate = ifelse(vital_srespirations <= 8, 3,
                            ifelse(vital_srespirations <= 11, 1,
                                   ifelse(vital_srespirations <= 20, 0,
                                          ifelse(vital_srespirations <= 24, 2, 3)))),
         temp = ifelse(vital_stemperature <= 35, 3,
                       ifelse(vital_stemperature <= 36, 1, 
                              ifelse(vital_stemperature <= 38, 0,
                                     ifelse(vital_stemperature <= 39, 1, 2)))),
         o2_sat = ifelse(vital_so2saturation <= 91, 3, 
                         ifelse(vital_so2saturation <= 93, 2, 
                                ifelse(vital_so2saturation <= 95, 1, 0))),
         flow_rate = ifelse(flow_rate == 2, 0, 1),
         cns = ifelse(cam == 2 | alc != 1, 0, 3)) %>% 
  mutate(NEWS = pulse + systolic_bp + resp_rate + temp + flow_rate + cns,
         NEWS_group = ifelse(NEWS <3, "low risk",
                             ifelse(NEWS <6, "moderate risk",
                                    ifelse(NEWS < 9, "high risk",
                                           "very high risk")))) %>% 
  mutate(HEWS_group = factor(NEWS_group, levels = c("low risk", 'moderate risk', "high risk", "very high risk"))) %>% 
  select(ENCOUNTER_NUM, pulse, systolic_bp, resp_rate, temp, flow_rate, cns, NEWS, NEWS_group, outcome)



hews8_trainx <- hews8_train %>% 
  mutate(hews1 = ifelse(HEWS == 1, 1, 0),
         hews2 = ifelse(HEWS == 2, 1, 0),
         hews3 = ifelse(HEWS == 3, 1, 0),
         hews4 = ifelse(HEWS == 4, 1, 0),
         hews5 = ifelse(HEWS == 5, 1, 0),
         hews6 = ifelse(HEWS == 6, 1, 0),
         hews7 = ifelse(HEWS == 7, 1, 0),
         hews8 = ifelse(HEWS == 8, 1, 0),
         hews9 = ifelse(HEWS == 9, 1, 0),
         hews10 = ifelse(HEWS == 10, 1, 0),
         hews11 = ifelse(HEWS >= 11, 1, 0)) %>% 
  select(hews1:hews11) %>% 
  as.matrix()
hews8_trainy <- hews8_train$outcome
hews8_testx <- hews8_test %>% 
  mutate(hews1 = ifelse(HEWS == 1, 1, 0),
         hews2 = ifelse(HEWS == 2, 1, 0),
         hews3 = ifelse(HEWS == 3, 1, 0),
         hews4 = ifelse(HEWS == 4, 1, 0),
         hews5 = ifelse(HEWS == 5, 1, 0),
         hews6 = ifelse(HEWS == 6, 1, 0),
         hews7 = ifelse(HEWS == 7, 1, 0),
         hews8 = ifelse(HEWS == 8, 1, 0),
         hews9 = ifelse(HEWS == 9, 1, 0),
         hews10 = ifelse(HEWS == 10, 1, 0),
         hews11 = ifelse(HEWS >= 11, 1, 0))%>% 
  select(hews1:hews11) %>% 
  as.matrix()
hews8_testy <- hews8_test$outcome


fraction_all <- table(hews8_trainy)/length(hews8_trainy)
# assign 1 - that value to a "weights" vector
weights_all <- 1 - fraction_all[as.character(hews8_trainy)]

hews_longitudinal  <- glmnet::cv.glmnet(x = hews8_trainx, y = hews8_trainy, weights = weights_all, type.measure = 'auc',
                                        family = 'binomial')



hews_long_predictions <- hews8_test %>% 
  mutate(hews_predictions  = as.numeric(predict(hews_longitudinal , hews8_testx,  s = "lambda.min", type = "response")))


save(hews_long_predictions , file = "models/hews_long_predictions.R")




news8_trainx <- news_8_train %>% 
  mutate(hews1 = ifelse(NEWS == 1, 1, 0),
         hews2 = ifelse(NEWS == 2, 1, 0),
         hews3 = ifelse(NEWS == 3, 1, 0),
         hews4 = ifelse(NEWS == 4, 1, 0),
         hews5 = ifelse(NEWS == 5, 1, 0),
         hews6 = ifelse(NEWS == 6, 1, 0),
         hews7 = ifelse(NEWS == 7, 1, 0),
         hews8 = ifelse(NEWS == 8, 1, 0),
         hews9 = ifelse(NEWS == 9, 1, 0),
         hews10 = ifelse(NEWS == 10, 1, 0),
         hews11 = ifelse(NEWS >= 11, 1, 0)) %>% 
  select(hews1:hews11) %>% 
  as.matrix()
news8_trainy <- news_8_train$outcome
news8_testx <- news_8_test %>% 
  mutate(hews1 = ifelse(NEWS == 1, 1, 0),
         hews2 = ifelse(NEWS == 2, 1, 0),
         hews3 = ifelse(NEWS == 3, 1, 0),
         hews4 = ifelse(NEWS == 4, 1, 0),
         hews5 = ifelse(NEWS == 5, 1, 0),
         hews6 = ifelse(NEWS == 6, 1, 0),
         hews7 = ifelse(NEWS == 7, 1, 0),
         hews8 = ifelse(NEWS == 8, 1, 0),
         hews9 = ifelse(NEWS == 9, 1, 0),
         hews10 = ifelse(NEWS == 10, 1, 0),
         hews11 = ifelse(NEWS >= 11, 1, 0))%>% 
  select(hews1:hews11) %>% 
  as.matrix()
news8_testy <- news_8_test$outcome


fraction_all <- table(news8_trainy)/length(news8_trainy)
# assign 1 - that value to a "weights" vector
weights_all <- 1 - fraction_all[as.character(news8_trainy)]

news_longitudinal  <- glmnet::cv.glmnet(x = news8_trainx, y = news8_trainy, weights = weights_all, type.measure = 'auc',
                                        family = 'binomial')

news_long_predictions <- news_long_predictions %>% ungroup()
news_8_test <- news_8_test %>% bind_cols()
news_long_predictions <- news_8_test %>% 
  mutate(news_predictions  = as.numeric(predict(news_longitudinal , news8_testx,  s = "lambda.min", type = "response")))
news_long_predictions <-news_long_predictions %>% bind_cols(hews8_test %>% select(timestamp))

library(pROC)
roc_obj <- roc(hews_long_predictions$outcome, hews_long_predictions$news_predictions)
auc(roc_obj)

pred <- factor(ifelse(hews_long_predictions$news_predictions > .5, 1, 0))
actual <- factor(hews_long_predictions$outcome)

caret::confusionMatrix(pred, actual, positive = "1")
save(news_long_predictions , file = "models/news_long_predictions.R")


hews_long_predictions <- hews_long_predictions %>% 
  filter(news_predictions >.5) %>% 
  group_by(ENCOUNTER_NUM) %>% 
  filter(row_number() == 1)

hews_long_predictions <- hews_long_predictions %>% 
  left_join(gim_encounters %>% select(ENCOUNTER_NUM, OUTCOME_TS)) %>% 
  mutate(tto = as.numeric(difftime(OUTCOME_TS, timestamp, units = "days")))
