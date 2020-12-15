
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

con <- connect_edw("murrayj")

load("Z:\\LKS-CHART\\Projects\\gim_ews_preassessment_project\\data\\data-analysis\\all_data_entry.Rda")
load("Z:\\LKS-CHART\\Projects\\gim_ews_preassessment_project\\data\\data-analysis\\patient_list.Rda")



#######################################################
# extract unique dates

all_data_entry <- all_data_entry %>% 
  mutate(date = as_date(timestamp))

dates <- as.character(unique(all_data_entry$date))
########################################################

########################################################
# get patient adt and timeseries

get_patient_adt <- function (edw_con, location_or_service = "location") {
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
adt <- get_patient_adt(con)
adt <- adt %>% 
  ungroup()

# combine GIM services
adt <- adt %>% 
  mutate(SERVICE_CD = ifelse(SERVICE_CD %in% c("TMA", "TMB",
                                               "TMC", "TMD",
                                               "TME"), "GIM",
                             SERVICE_CD)) %>% 
  mutate(SERVICE = ifelse(SERVICE_CD %in% c("TMA GENERAL MEDICINE",
                                            "TMB GENERAL MEDICINE",
                                            "TMC GENERAL MEDICINE", 
                                            "TMD GENERAL MEDICINE",
                                            "TME GENERAL MEDICINE"
                                               ), "GIM",
                          SERVICE))

adt_location_ts <- get_location_timeseries(adt,
                                           location_or_service = "location")
adt_service_ts <- get_location_timeseries(adt, 
                                          location_or_service = "service")

# add MRN to the locations and services

inpatient <- dim_tbl(con, "INPATIENT_ENCOUNTER_FACT") %>% 
  select(ENCOUNTER_NUM, PATIENT_DK)
pat <- dim_tbl(con, "PATIENT_DIM") %>% 
  select(PATIENT_DK, MEDICAL_RECORD_NUM) 

pat <- inpatient %>% 
  left_join(pat) %>% 
  collect()

pat <- pat %>% 
  select(-PATIENT_DK)

adt_location_ts <- adt_location_ts %>% 
  left_join(pat, by = "ENCOUNTER_NUM")

adt_service_ts <- adt_service_ts %>% 
  left_join(pat, by = "ENCOUNTER_NUM")

all_adt_mrns <- unique(c(adt_location_ts$MEDICAL_RECORD_NUM, adt_service_ts$MEDICAL_RECORD_NUM))

all_mrns <- unique(c(all_data_entry$MRN, patient_list$MedicalRecordNumber))
mrns <- all_mrns[all_mrns %in% all_adt_mrns]

# filter for available MRNs
adt_location_ts <- adt_location_ts %>% 
  filter(MEDICAL_RECORD_NUM %in% mrns)

adt_service_ts <- adt_service_ts %>% 
  filter(MEDICAL_RECORD_NUM %in% mrns)

all_data_entry <- all_data_entry %>% 
  filter(MRN %in% mrns)

patient_list <- patient_list %>% 
  filter(MedicalRecordNumber %in% mrns)

gim_units <- c("14C MEDICINE  NORTH-5335,SOUTH-6737",
               "14C MEDICINE STEP UP UNIT           3112")
########################################################


# for each date
missed_mrns <- c()
captured_mrns <- c()
missed_position <- c()
all_data <- list()
for(i in dates) {
  
  directory <- paste0('Z:\\LKS-CHART\\Projects\\gim_ews_preassessment_project\\data\\prediction_frames\\', i)
  if(!dir.exists(directory)) {
    dir.create(directory)
  }
  # get the full list of MRNs for that day
  today_entry_mrn <- all_data_entry %>% 
    filter(date == ymd(i))
  
  # get the timestamp of the data collection
  td <- unique(today_entry_mrn$timestamp)
  
  # Get the list of patients on the unit at that time
  tmp <- adt_location_ts %>% 
    filter(unit %in% gim_units) %>% 
    filter(ymd_hms(start_time) <= ymd_hms(td),
           ymd_hms(end_time) > ymd_hms(td))
  
  tmp_service <- adt_service_ts %>% 
    filter(grepl("TM", service)) %>% 
    filter(ymd_hms(start_time) <= ymd_hms(td),
           ymd_hms(end_time) > ymd_hms(td))
    
  # combine their MRNs
  today_mrns <- unique(c(today_entry_mrn$MRN,
                         tmp$MEDICAL_RECORD_NUM, tmp_service$MEDICAL_RECORD_NUM))
  today_mrns <- today_mrns[!is.na(today_mrns)]
  
  # get the encounter numbers for those patients
  today_loc <- adt_location_ts %>% 
    filter(MEDICAL_RECORD_NUM %in% today_mrns) %>% 
    filter(ymd_hms(start_time) <= ymd_hms(td),
           ymd_hms(end_time) > ymd_hms(td))
  
  
  d <- setdiff(today_mrns, today_loc$MEDICAL_RECORD_NUM)
  
  if(length(d) > 0) {
   missing <-  adt_location_ts %>% 
      filter(MEDICAL_RECORD_NUM %in% d) %>% 
      filter(ymd_hms(start_time) <= ymd_hms(td) |ymd_hms(start_time) <= ymd_hms(td) + ddays(1),
             ymd_hms(end_time) > ymd_hms(td))
   
   if(nrow(missing) == length(d)) {
     today_loc <- today_loc %>% 
       bind_rows(missing)
   }
   
  }
  
  today_encounters <- tibble(ENCOUNTER_NUM = today_loc$ENCOUNTER_NUM)
  
  
  admit_time <- adt %>% 
    filter(ENCOUNTER_NUM %in% today_loc$ENCOUNTER_NUM) %>% 
    filter(EVENT_TYPE_CD == "ADMIT") %>% 
    select(ENCOUNTER_NUM, ADMIT_TS = EVENT_TS) %>% 
    group_by(ENCOUNTER_NUM) %>% 
    arrange(ADMIT_TS) %>% 
    filter(row_number() == 1) %>% 
    ungroup()
  
  gim_unit_time <- adt_location_ts %>% 
    filter(ENCOUNTER_NUM %in% today_loc$ENCOUNTER_NUM) %>% 
    filter(unit %in% gim_units) %>% 
    select(ENCOUNTER_NUM, GIM_START_TS = start_time) %>% 
    group_by(ENCOUNTER_NUM) %>% 
    arrange(GIM_START_TS) %>% 
    filter(row_number() == 1) %>% 
    ungroup()
  
  gim_serv_time <- adt_service_ts %>% 
    filter(ENCOUNTER_NUM %in% today_loc$ENCOUNTER_NUM) %>% 
    filter(ymd_hms(start_time) <= ymd_hms(td),
           ymd_hms(end_time) > ymd_hms(td)) %>% 
    select(ENCOUNTER_NUM, service, SERVICE_START_TS = start_time )
  
  icus <- c("INTENSIVE CARE ACUTE CARE GENERAL SURG",
            "INTENSIVE CARE CARDIOVASCULAR",
            "INTENSIVE CARE MEDICAL",
            "INTENSIVE CARE NEURO SURGERY",
            "INTENSIVE CARE TRAUMA",
            "INTENSIVE CORONARY CARE")
  
  icu_time <- adt_service_ts %>% 
    filter(ENCOUNTER_NUM %in% today_loc$ENCOUNTER_NUM) %>% 
    filter(ymd_hms(start_time) > ymd_hms(td)) %>% 
    filter(service %in% icus) %>% 
    select(ENCOUNTER_NUM, ICU_TS = start_time, ICU = service) %>% 
    group_by(ENCOUNTER_NUM) %>% 
    arrange(ICU_TS) %>% 
    filter(row_number() == 1) %>% 
    ungroup()%>% 
    mutate(post_gim_icu = ifelse(!is.na(ICU_TS),      
                          1, 0))
  
  disposition <- dim_tbl(con, "INPATIENT_ENCOUNTER_FACT") %>% 
    filter(ENCOUNTER_NUM %in% local(today_encounters$ENCOUNTER_NUM)) %>% 
    select(ENCOUNTER_NUM, DISCHARGE_TS, ADT_DISCHARGE_DISPOSITION_DK)
  
  disp_cd <- dim_tbl(con, "DISCHARGE_DISPOSITION_DIM") %>% 
    select(DISCHARGE_DISPOSITION_DK, DISCHARGE_DISPOSITION_DESCR)
  disposition <- disposition %>% 
    left_join(disp_cd, by = c("ADT_DISCHARGE_DISPOSITION_DK" = "DISCHARGE_DISPOSITION_DK")) %>% 
    select(-ADT_DISCHARGE_DISPOSITION_DK) %>% 
    collect()
  disposition <- disposition %>% 
    mutate(death = ifelse(DISCHARGE_DISPOSITION_DESCR == "Expired                             *EXP",
                          1, 0))
    
  
  today_encounters <- today_encounters %>% 
    left_join(admit_time, by = "ENCOUNTER_NUM") %>% 
    left_join(gim_unit_time, by = "ENCOUNTER_NUM") %>% 
    left_join(gim_serv_time, by = "ENCOUNTER_NUM")%>% 
    left_join(icu_time, by = "ENCOUNTER_NUM")%>% 
    left_join(disposition, by = "ENCOUNTER_NUM")
  
  today_encounters <- today_encounters %>% 
    mutate(current_time = td)
  all_data[[i]] <- today_encounters
  readr::write_csv(today_encounters, path = paste0(directory, "\\gim_encounters.csv"))
  
  print(i)
}


all_data <- do.call(rbind, all_data)

readr::write_csv(all_data, 'Z:\\LKS-CHART\\Projects\\gim_ews_preassessment_project\\data\\all_encounters.csv')
