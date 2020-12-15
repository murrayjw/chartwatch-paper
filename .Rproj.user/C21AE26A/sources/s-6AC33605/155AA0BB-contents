
# Create prediction frames ------------------------------------------------

#'   ***********************************************************************
#'   ***********************************************************************
#'   ***********************************************************************
#'   Initially created by: Josh Murray
#'   Date: 
#'   contact information: 
#'
#'   Script contents: The following script creates the prediction data
#'   required as input into the GIM risk model
#'   
#'   One folder is created per date 
#'
#'   ***********************************************************************
#'   ***********************************************************************
#'   ***********************************************************************

library(tidyverse)
library(lubridate)
library(CHART)

# connect to the edw
con <- connect_edw(keyring::key_get("EDW_USERNAME"),
                   keyring::key_get("EDW_PASSWORD"))


# load the data entry files 
load("Z:\\LKS-CHART\\Projects\\gim_ews_preassessment_project\\data\\data-analysis\\all_data_entry.Rda")

all_data_entry <- all_data_entry %>% 
  mutate(professional_role = case_when(
    clinicianid == 121 ~ "Physician - Team E",
    clinicianid == 106 ~ "Physician - Team C",
    clinicianid == 108 ~ "Physician - Team B",
    clinicianid == 116 ~ "Physician - Team D",
    clinicianid == 124 ~ "Resident - Team C",
    clinicianid == 125 ~ "Resident - Team D",
    clinicianid == 109 ~ "Physician - Team A",
    clinicianid == 112 ~ "Physician - Team B",
    clinicianid == 129 ~ "Resident - Team D",
    clinicianid == 169 ~ "Resident - Team D",
    clinicianid == 117 ~ "Physician - Team A",
    clinicianid == 115 ~ "Physician - Team D",
    clinicianid == 123 ~ "Physician - Team B",
    clinicianid == 149 ~ "Senior Resident - Team B",
    clinicianid == 105 ~ "Physician - Team E",
    TRUE ~ professional_role
  ))
# load the link log 

link_log_path <- "Z:\\LKS-CHART\\Projects\\gim_ews_preassessment_project\\data\\link_log\\"
link_log_name <- paste0(link_log_path, "link_log.xlsx")
link_log <- readxl::read_excel(link_log_name, sheet = "Link Log")


all_data_entry <- all_data_entry %>% 
  mutate(date = as_date(timestamp))


path <- 'Z:\\LKS-CHART\\Projects\\gim_ews_preassessment_project\\data\\prediction_frames\\'
dirs <- list.files(path, full.names =T )

# function to split paths in directories
split_path <- function(x) {
  if (dirname(x)==x) {
    x
  } else {
    c(basename(x),split_path(dirname(x)))
  }
}

## get MRN
inp <- dim_tbl(con, "INPATIENT_ENCOUNTER_FACT") %>% 
  select(ENCOUNTER_NUM, PATIENT_DK)
pat <- dim_tbl(con, "PATIENT_DIM") %>% 
  select(PATIENT_DK, MEDICAL_RECORD_NUM)
inp <- inp %>% left_join(pat) %>% 
  collect()

inp <- inp %>% 
  select(ENCOUNTER_NUM, MEDICAL_RECORD_NUM)
# for each directory
all_predictions <- list()
total_negative_predictions <- 0
dirs <- dirs[!grepl("ensemble|last|census|soarian", dirs)]
for(i in 1:length(dirs)) {
  
  # the encounter file
  files <- list.files(dirs[i], full.names = T)
  encounter_file <- grep('encounter', files, ignore.case = T, value = T)
  today_date <- split_path(dirs[i])[1]
  encounters <- readr::read_csv(encounter_file)
  
  # join to inpatient data to get MRN
  encounters <- encounters %>% 
    mutate(ENCOUNTER_NUM = as.character(ENCOUNTER_NUM)) %>% 
    left_join(inp, by = "ENCOUNTER_NUM")
 
  
  encounters <- encounters %>% 
    mutate(gim_service = case_when(
      grepl("TMA", service)  ~ "A",
      grepl("TMB", service)  ~ "B",
      grepl("TMC", service)  ~ "C",
      grepl("TMD", service)  ~ "D",
      grepl("TME", service)  ~ "E",
      TRUE ~ "OTHER"
    ))
  
  
  # add MRN to encounters
  
  
  daily_predictions <- all_data_entry %>% 
    filter(as_date(date) == ymd(today_date))
  
  cat('actual predictions =', nrow(daily_predictions), "\n")
  
  unique_pract <- unique(daily_predictions$clinicianid)
  
  daily_pract_preds <- list()
  total_negative_predictions <- 0
  for(j in unique_pract) {
    
    pract_pred <- daily_predictions %>% 
      filter(clinicianid == j)
    
    role <- unique(pract_pred$professional_role)
    
    if(tolower(role) == "nurse") {
      
      d <- unique(as_date(pract_pred$date))
      tt <- min(pract_pred$timestamp)
      
      results <- pract_pred %>% 
        select(clinicianid,
               professional_role,
               patientid,
               next48 = anyone_next48,
               next48_conf = anyone_next48_conf,
               ever = anyone_ever,
               ever_conf = anyone_ever_conf,
               palliative = any_palliative,
               MRN) %>% 
        mutate(
          date = d,
          timestamp = tt)
      
      results <- results %>% 
        mutate(ever = ifelse(grepl("Y", ever, ignore.case = T),
                             1, 0),
               next48 = ifelse(grepl("Y", next48, ignore.case = T),
                               1, 0))%>% 
        mutate(palliative = as.numeric(palliative))
      
      full_pred <- results %>% 
        left_join(encounters %>% select(ENCOUNTER_NUM,
                                        MRN = MEDICAL_RECORD_NUM),
                  by = 'MRN')
      
      full_pred <- full_pred%>% 
        mutate(negative = 0)
        
      
    } else {
      
      pract_pred <- pract_pred %>% 
        mutate(gim_service = stringr::str_sub(professional_role, -1, -1))
      
      d <- unique(as_date(pract_pred$date))
      tt <- min(pract_pred$timestamp)
      tmp_service <- unique(pract_pred$gim_service)
      
      team_pred <- encounters %>% 
        filter(gim_service == tmp_service)
      
      not_miss <- pract_pred %>% 
        filter(!is.na(patientid))
      
      not_miss <- not_miss %>% 
        left_join(encounters %>% select(ENCOUNTER_NUM,
                                        MRN = MEDICAL_RECORD_NUM),
                  by = 'MRN')
      
      if(nrow(team_pred) > 0) {
        
        not_miss_encounters <- not_miss$ENCOUNTER_NUM
        
        miss <- team_pred %>% 
          filter(!ENCOUNTER_NUM %in% not_miss_encounters)
        
        miss <- miss %>% 
          mutate(clinicianid = j,
                 professional_role = role,
                 date = d,
                 timestamp = tt,
                 ENCOUNTER_NUM, 
                 MRN = MEDICAL_RECORD_NUM,
                 next48 = 0,
                 next48_conf = NA,
                 ever = 0,
                 ever_conf = NA,
                 palliative = 0) %>% 
          select(clinicianid,
                 professional_role,
                 date,
                 timestamp,
                 next48,
                 next48_conf,
                 ever,
                 ever_conf,
                 palliative,
                 MRN)
        
        miss <-miss %>% 
          left_join(link_log, by = "MRN") %>% 
          rename(patientid = study_id)
        
        miss <- miss %>% 
          left_join(encounters %>% select(ENCOUNTER_NUM,
                                          MRN = MEDICAL_RECORD_NUM),
                    by = 'MRN')
        total_negative_predictions <- total_negative_predictions + nrow(miss)
        
        miss <- miss %>% 
          mutate(negative = 1)
      }
      
      
      if(nrow(not_miss) > 0) {
        
        results <- not_miss %>% 
          select(clinicianid,
                 professional_role,
                 patientid,
                 next48 = anyone_next48,
                 next48_conf = anyone_next48_conf,
                 ever = anyone_ever,
                 ever_conf = anyone_ever_conf,
                 palliative = any_palliative,
                 MRN) %>% 
          mutate(
            date = d,
            timestamp = tt)
        
        results <- results %>% 
          mutate(ever = ifelse(grepl("Y", ever, ignore.case = T),
                               1, 0),
                 next48 = ifelse(grepl("Y", next48, ignore.case = T),
                                 1, 0))
        
        results <- results %>% 
          left_join(encounters %>% select(ENCOUNTER_NUM,
                                          MRN = MEDICAL_RECORD_NUM),
                    by = 'MRN') %>% 
          mutate(palliative = as.numeric(palliative))
        
        results <- results %>% 
          mutate(negative = 0)
        
        full_pred <- miss %>% 
          bind_rows(results)
          
      } else {
        full_pred <- miss
      }
      
      
      
    }
    daily_pract_preds[[j]] <- full_pred
   
  }
  daily_pract_preds <- do.call(rbind, daily_pract_preds)
  
  a <- daily_predictions %>% 
    filter(!is.na(patientid))
  
  diff <- (nrow(a) + total_negative_predictions) - nrow(daily_pract_preds)
  cat(' number of predictions:', nrow(daily_pract_preds), '\n',
      'number of non-missing data entries:', nrow(a), '\n',
      'number of negative-preds added:', total_negative_predictions,'\n',
      'difference:', diff,
      '\n')
 
  
 
 
  all_predictions[[i]] <- daily_pract_preds
 
  print(i)
}

  
physician_predictions <- do.call(rbind, all_predictions)

readr::write_csv(physician_predictions,
                 'Z:\\LKS-CHART\\Projects\\gim_ews_preassessment_project\\data\\practitioner_predictions.csv')











