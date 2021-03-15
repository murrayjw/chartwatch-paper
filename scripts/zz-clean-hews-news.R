#'  ***********************************************************************
#'  ***********************************************************************
#'  ***********************************************************************
#'  Script name:
#'  Initially created by: 
#'  Date created: 
#'  Maintainer information: 
#'
#'  Script contents: Don't use --> use compute-hews-news.R script to 
#'  re-generate predictions instead
#'
#'  ***********************************************************************
#'  ***********************************************************************
#'  ***********************************************************************

library(dplyr)
library(lubridate)
library(pROC)
source("./constants.R")
load('/mnt/research/LKS-CHART/Projects/gim_ews_preassessment_project/data/data-analysis/hews-news-pred.Rda.R')

split_path <- function(x) {
  if (dirname(x)==x) {
    x
  } else {
    c(basename(x),split_path(dirname(x)))
  }
}

hews_news <- list()
n <- names(all_hews_news)
for(i in n) {
  
  tmp <- all_hews_news[[i]]
  d <- split_path(i)[1]
  time <- paste(d, "15:00:00")
  tmp <- tmp %>% 
    mutate(timestamp = lubridate::ymd_hms(time))
  hews_news[[i]] <- tmp
}

hews_news <- do.call(rbind, hews_news) %>%
  dplyr::mutate(prediction_date = as.Date(timestamp))

clinician_predictions <- read.csv(FINAL_PAPER_TEST_CLINICIAN_PREDICTIONS_FILENAME,
                                  stringsAsFactors = FALSE)
unique_outcomes <- clinician_predictions %>%
  dplyr::select(ENCOUNTER_NUM, timestamp, pal, death, icu, outcome, outcome48) %>%
  unique()

filtered_hews_news <- hews_news %>%
  dplyr::rename(timestamp_pred = timestamp) %>%
  dplyr::left_join(unique_outcomes, by = c("ENCOUNTER_NUM")) %>%
  dplyr::mutate(
    timestamp_pred = lubridate::ymd_hms(timestamp_pred),
    timestamp = lubridate::ymd_hms(timestamp)
  ) %>%
  dplyr::group_by(ENCOUNTER_NUM, timestamp) %>%
  dplyr::arrange(desc(timestamp_pred)) %>%
  dplyr::filter(timestamp_pred <= timestamp) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() 

# Missing HEWS/NEWS predictions
# Sanity check
predictions_folder <- "/mnt/research/LKS-CHART/Projects/gim_ews_preassessment_project/data/prediction_frames/"

for (date in list.files(predictions_folder)) {
  folder_name <- file.path(predictions_folder, date)
  if (dir.exists(folder_name)) {
    print(folder_name)
    
    encounters <- read.csv(file.path(folder_name, "gim_encounters.csv"))
    curr_hews_news <- filtered_hews_news %>%
      dplyr::filter(prediction_date == date)
    curr_unique_predictions <- unique_outcomes %>%
      dplyr::filter(as.Date(timestamp) == as.Date(date))
      
    if (nrow(curr_hews_news) != nrow(curr_unique_predictions)) {
      missing_encounter_nums <- curr_unique_predictions %>%
        dplyr::filter(!ENCOUNTER_NUM %in% curr_hews_news$ENCOUNTER_NUM) %>%
        dplyr::pull(ENCOUNTER_NUM) %>%
        unique()
      
      print(paste0("Missing ", 
                   length(missing_encounter_nums),
                   " predictions"))
      vitals_data <- read.csv(file.path(folder_name, "processed_vitals.csv"))
      
      vitals_data %>%
        dplyr::select(ENCOUNTER_NUM, timestamp, vital_spulse, 
                      vital_sbpsystolic, vital_srespirations,
                      vital_stemperature, vital_so2saturation) %>%
        dplyr::filter(ENCOUNTER_NUM %in% missing_encounter_nums) %>%
        dplyr::filter(as.Date(timestamp) <= date) %>%
        dplyr::group_by(ENCOUNTER_NUM) %>%
        arrange(desc(timestamp)) %>%
        slice(1)
        
    }
    

    
  
  }
}
