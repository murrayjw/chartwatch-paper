library(dplyr)
library(lubridate)
library(pROC)
source("./constants.R")
source("./scripts/helper-functions.R")


# Helper functions --------------------------------------------------------
# Re-using same fcts from calculate-hews-news-scores.R
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
    dplyr::mutate_at(dplyr::vars(c("pulse", 
                                   "systolic_bp", 
                                   "resp_rate", 
                                   "temp", 
                                   "o2_sat", 
                                   "flow_rate", 
                                   "cns")),  
                     list(measured = ~dplyr::if_else(is.na(.), 0, 1))) %>%
    replace(is.na(.), 0) %>%
    mutate(HEWS_measured_vars = pulse_measured + systolic_bp_measured +
             resp_rate_measured + temp_measured +
             o2_sat_measured + flow_rate_measured +
             cns_measured,
           HEWS = pulse + systolic_bp + resp_rate + temp + flow_rate + cns,
           HEWS_group = ifelse(HEWS <3, "low risk",
                               ifelse(HEWS <6, "moderate risk",
                                      ifelse(HEWS < 9, "high risk",
                                             "very high risk")))) %>% 
    mutate(HEWS_group = factor(HEWS_group, 
                               levels = c("low risk", 'moderate risk', "high risk", "very high risk"))) %>% 
    select(ENCOUNTER_NUM, timestamp, HEWS, HEWS_group, HEWS_measured_vars)
  
  
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
    dplyr::mutate_at(dplyr::vars(c("pulse", 
                                   "systolic_bp", 
                                   "resp_rate", 
                                   "temp", 
                                   "flow_rate", 
                                   "cns")),  
                     list(measured = ~dplyr::if_else(is.na(.), 0, 1))) %>%
    replace(is.na(.), 0) %>%
    mutate(NEWS = pulse + systolic_bp + resp_rate + temp + flow_rate + cns,
           NEWS_group = ifelse(NEWS <3, "low risk",
                               ifelse(NEWS <6, "moderate risk",
                                      ifelse(NEWS < 9, "high risk",
                                             "very high risk"))),
           NEWS_measured_vars = pulse_measured + systolic_bp_measured + 
             resp_rate_measured + temp_measured + flow_rate_measured +
             cns_measured
             ) %>% 
    mutate(HEWS_group = factor(NEWS_group, levels = c("low risk", 'moderate risk', "high risk", "very high risk"))) %>% 
    select(ENCOUNTER_NUM,timestamp, NEWS, NEWS_group, NEWS_measured_vars)
  return(news_data)
  
} 




# Generate HEWS and NEWS scores -------------------------------------------


load(DESCRIPTIVE_STATS_FILE)
predictions_folder <- "/mnt/research/LKS-CHART/Projects/gim_ews_preassessment_project/data/prediction_frames/"
all_hews_news <- list()
i <- 1

for (date in list.files(predictions_folder)) {
  folder_name <- file.path(predictions_folder, date)
  if (dir.exists(folder_name)) {
    
    encounters <- readr::read_csv(paste0(folder_name, "/gim_encounters.csv"))
    
    time <- lubridate::ymd_hms(unique(encounters$current_time))
    
    # This file contains flow rate, cam, and alc
    hews_news_vars <-  readr::read_csv(paste0(folder_name, "/hews_news_vars.csv")) %>%
      dplyr::mutate(timestamp = lubridate::ymd_hms(timestamp)) %>%
      dplyr::filter(timestamp <= time) %>%
      dplyr::group_by(ENCOUNTER_NUM) %>%
      # Only keep most recent available values
      dplyr::arrange(desc(timestamp)) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::select(-timestamp)
    
    encounters <- encounters %>% 
      rowwise() %>% 
      mutate(start_time = max(GIM_START_TS, ADMIT_TS, na.rm = T)) %>% 
      ungroup() %>% 
      mutate(start_time = if_else(start_time > time, time, start_time))
    
    # Load vitals
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
      left_join(news_data) %>%
      dplyr::mutate(date = date)
    
    # Save HEWS/NEWS predictions
    write.csv(hews_news_data,
             file = file.path(folder_name, "hews_news_predictions_2021_0401.csv"),
             row.names = FALSE)
    all_hews_news[[i]] <- hews_news_data
    i <- i + 1
    
  }
}


combined_hews_news <- do.call(rbind, all_hews_news)


# Check performance -------------------------------------------------------
clinician_predictions <- read.csv(FINAL_PAPER_TEST_CLINICIAN_PREDICTIONS_FILENAME,
                                  stringsAsFactors = FALSE)
unique_outcomes <- clinician_predictions %>%
  dplyr::select(ENCOUNTER_NUM, timestamp, outcome, outcome48, date) %>%
  unique()


hews_news_outcomes <- unique_outcomes %>%
  dplyr::left_join(combined_hews_news, by = c("ENCOUNTER_NUM", "date")) %>%
  dplyr::select(ENCOUNTER_NUM, date, contains('outcome'), HEWS, NEWS) 

# Save
write.csv(hews_news_outcomes,
          file = FINAL_PAPER_TEST_HEWS_NEWS_FILENAME,
          row.names = FALSE)


# Evaluate HEWS/NEWS models -----------------------------------------------

hews_outcomes <- hews_news_outcomes %>%
  dplyr::mutate(.pred_1 = HEWS,
                OUTCOME_ALL = outcome,
                outcome_all_48 = outcome48)

news_outcomes <- hews_news_outcomes %>%
  mutate(.pred_1 = NEWS,
         OUTCOME_ALL = outcome,
         outcome_all_48 = outcome48)

# For HEWS, 0-2 = low risk,
# 3-5 = moderate risk,
# 6-8 = high risk
HEWS_CUTOFF <- 6

# For NEWS, 0-2 = low risk
# 3-5 = moderate risk
# 6-8 = high risk
# 9 = very high risk
NEWS_CUTOFF <- 6

pROC::auc(hews_news_outcomes$outcome, hews_news_outcomes$HEWS)

pROC::auc(hews_news_outcomes$outcome, hews_news_outcomes$NEWS)

# HEWS with high risk cutoff
evaluate_threshold(hews_outcomes, thr = HEWS_CUTOFF) %>%
  as.data.frame()

# HEWS with moderate risk cutoff
evaluate_threshold(hews_outcomes, thr = 3) %>%
  as.data.frame()

# NEWS with high risk cutoff
evaluate_threshold(news_outcomes, thr = NEWS_CUTOFF) %>%
  as.data.frame()

# NEWS with moderate risk cutoff
evaluate_threshold(news_outcomes, thr = 3) %>%
  as.data.frame()

# NEWS with very high risk cutoff
evaluate_threshold(news_outcomes, thr = 9) %>%
  as.data.frame()


# Look at missing HEWS/NEWS scores ----------------------------------------

test_encounters <- read.csv(FINAL_PAPER_TEST_ENCOUNTERS_FILENAME,
                            stringsAsFactors = FALSE)

hews_news_outcomes  %>%
  count(HEWS)
hews_news_outcomes %>%
  count(NEWS)

hews_news_outcomes %>%
  filter(!is.na(HEWS)) %>%
  nrow()

hews_news_outcomes %>%
  filter(!is.na(NEWS)) %>%
  nrow()

hews_news_outcomes %>%
  dplyr::filter(is.na(HEWS)) %>%
  dplyr::left_join(test_encounters, by = "ENCOUNTER_NUM") %>%
  dplyr::mutate(
    time_since_admit = difftime(date, as.Date(ADMIT_TS), unit = "days")
  )
