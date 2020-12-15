#'   ***********************************************************************
#'   ***********************************************************************
#'   ***********************************************************************
#'   Initially created by: Josh Murray
#'   Date: April 30th 2019
#'   contact information: murrayj@smh.ca
#'
#'   Script contents: This script updates the linking log with the latest
#'   daily patient list
#'
#'   ***********************************************************************
#'   ***********************************************************************
#'   ***********************************************************************


# load the required libraries ---------------------------------------------
library(readxl)
library(tidyverse)
library(xlsx)


# set the paths to patient and link-logs ----------------------------------
link_log_path <- "Z:\\LKS-CHART\\Projects\\gim_ews_preassessment_project\\data\\link_log\\"
patient_list_path <- "Z:\\LKS-CHART\\Projects\\gim_ews_preassessment_project\\implementation\\patient_list\\"


# the data entry paths ----------------------------------------------------
data_entry_path <- "Z:\\LKS-CHART\\Projects\\gim_ews_preassessment_project\\data\\"

# note, this was a local path set up for safety purposes. Files are no longer there
data_entry_path <- "H:\\gim_ews_data_extraction\\d\\"


# get a list of all the data entries --------------------------------------
files <- list.files(path = data_entry_path, pattern = "*.xlsx", full.names = T)


# select files to update


all_dates <- stringr::str_sub(files, -14, -6)
all_dates <- paste0(str_sub(all_dates, 1, 7), "_", str_sub(all_dates, 8, 9)) 


patient_files <- list.files(path = patient_list_path, pattern = "*.xlsx", full.names = T)
patient_filesd <- stringr::str_sub(patient_files, -15, -6)
patient_filesd <- patient_filesd[patient_filesd %in% all_dates]
patient_files <- paste0("patient_list_", patient_filesd, ".xlsx")

# load the link log
link_log_name <- paste0(link_log_path, "link_log.xlsx")
link_log <- readxl::read_excel(link_log_name, sheet = "Link Log")
n <- length(files)



for(j in 1:n) {
  
  # load the patient list
  patient_list_name <- paste0(patient_list_path, patient_files[j])
  patient_list <- readxl::read_excel(patient_list_name)
  
  
  for(i in 1:nrow(patient_list)) {
    
    pid <- patient_list$MedicalRecordNumber[i]
    
    # if the patient is already in the link log
    # replace their MRN with their study ID
    if(pid %in% link_log$MRN) {
      
      id_rep <- link_log %>% 
        filter(MRN == pid) %>% 
        select(study_id)
      
      patient_list$MedicalRecordNumber[i] <- id_rep$study_id
    } else {
      # extract a new id
      new_id <- link_log %>% 
        filter(is.na(MRN)) %>%
        slice(1)
      patient_list$MedicalRecordNumber[i] <- new_id$study_id
      
      # map the MRN into the link log
      link_log$MRN[link_log$study_id == new_id$study_id] <- pid
    }
    
  }
  
  # remove additional information from the patient list
  patient_list <- patient_list %>% 
    select(-PatientLastName,
           -PatientFirstName,
           -PatientMiddleName,
           -RoomBed) %>% 
    rename(study_id = MedicalRecordNumber)
  
  # archive the patient list
  char_date <- stringr::str_sub(files[j], -14, -6)
  
  archive_list_name <- paste0(patient_list_path, "archive\\", "patient_list_", char_date, ".csv")
  readr::write_csv(patient_list, path = archive_list_name)
  
  # read in the data entry
  
  data_entry <- readxl::read_excel(files[j], skip = 2,
                                   sheet = "Original")
  
  for(i in 1:nrow(data_entry)) {
    pid <- data_entry$PatientID[i]
    
    # if the study has already been replaced, go to the next ID
    if(nchar(pid) == 4 | is.na(pid) | !grepl("[0-9]", pid)) {
      next
    }
    
    # if the patient is already in the link log
    # replace their MRN with their study ID
    if(pid %in% link_log$MRN) {
      id <- link_log %>% 
        filter(MRN == pid)
      
      data_entry$PatientID[i] <- id$study_id
    } else {
      # extract a new id
      new_id <- link_log %>% 
        filter(is.na(MRN)) %>%
        slice(1)
      data_entry$PatientID[i] <- new_id$study_id
      
      # map the MRN into the link log
      link_log$MRN[link_log$study_id == new_id$study_id] <- pid
    }
    
    print(j)
  }
  

  
  # save the data entry
  wb = createWorkbook()
  sheet = createSheet(wb, "Data Entry")
  addDataFrame(as.data.frame(data_entry), sheet=sheet,  row.names=FALSE)
  file_name <- paste0("Z:\\LKS-CHART\\Projects\\gim_ews_preassessment_project\\data\\archive\\data_entry_",
                      char_date, ".xlsx")
  saveWorkbook(wb, file_name)
  
}


# save the link log
wb = createWorkbook()
sheet = createSheet(wb, "Link Log")
addDataFrame(as.data.frame(link_log), sheet=sheet,  row.names=FALSE)
saveWorkbook(wb, "Z:\\LKS-CHART\\Projects\\gim_ews_preassessment_project\\data\\link_log\\link_log.xlsx")



