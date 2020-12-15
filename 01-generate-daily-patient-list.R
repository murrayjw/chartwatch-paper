#'   ***********************************************************************
#'   ***********************************************************************
#'   ***********************************************************************
#'   Initially created by: Josh Murray
#'   Date: April 30th 2019
#'   contact information: murrayj@smh.ca
#'
#'   Script contents: Generate daily patient list
#'
#'   ***********************************************************************
#'   ***********************************************************************
#'   ***********************************************************************



# load libraries ----------------------------------------------------------

library(CHART)
library(tidyverse)
library(lubridate)
library(dbplyr)
library(xlsx)

# Connect to Soarian
con <- connect_soarian("murrayj", password = "")


# query the patient list --------------------------------------------------

patient_list <- tbl(con, in_schema("", "vw_SMH_NursingCensus")) %>% 
  filter(UnitContactedName %like% 'Team Medicine%') %>% 
  select(PatientLastName,
         PatientFirstName,
         PatientMiddleName,
         MedicalRecordNumber,
         AttendDr,
         UnitContactedName,
         RoomBed) %>% 
  collect() %>% 
  arrange(UnitContactedName, RoomBed)


# write the data to file --------------------------------------------------
file_location = "Z:/LKS-CHART/Projects/gim_ews_preassessment_project/implementation/patient_list/"
file_name <- paste0(file_location, "patient_list_", str_replace_all(Sys.Date() , "-", "_"), ".xlsx")

wb = createWorkbook()

sheet = createSheet(wb, "Patient List")
addDataFrame(as.data.frame(patient_list), sheet=sheet,  row.names=FALSE)
saveWorkbook(wb, file_name)



