#'  ***********************************************************************
#'  ***********************************************************************
#'  ***********************************************************************
#'  Script name:
#'  Initially created by: 
#'  Date created: 
#'  Maintainer information: 
#'
#'  Script contents: Extract all outcomes and save to file
#'
#'  ***********************************************************************
#'  ***********************************************************************
#'  ***********************************************************************

library(keyring)
library(chartwatch)
library(odbc)
source("../constants.R")

# Get outcomes - prospective test data ------------------------------------

clinician_predictions <- read.csv(FINAL_PAPER_TEST_CLINICIAN_PREDICTIONS_FILENAME,
                                  stringsAsFactors = FALSE)
options("keyring_backend" = "file",
        stringsAsFactors = F)
keyring_unlock(keyring = "database",
               password = Sys.getenv("R_KEYRING_PASSWORD"))

con_edw <- chartdb::connect_edw(
  username = keyring::key_get(service = "EDW",
                              username = "EDW_USERNAME",
                              keyring = "database"),
  password = keyring::key_get(service = "EDW",
                              username = "EDW_PASSWORD",
                              keyring = "database"))


all_outcomes <- chartwatch::get_outcome_events(con = con_edw,
                                               encounter_vector = unique(clinician_predictions$ENCOUNTER_NUM))
discharge_dispositions <- chartwatch::get_discharge_disposition(con = con_edw, 
                                                                encounter_vector = unique(clinician_predictions$ENCOUNTER_NUM))
deaths_outside_ward <- discharge_dispositions %>%
  filter(outcome == "death") %>%
  filter(!ENCOUNTER_NUM %in% all_outcomes$ENCOUNTER_NUM) %>%
  select(ENCOUNTER_NUM, EVENT_TS = DISCHARGE_TS) %>%
  mutate(EVENT_TYPE = "Death outside ward")


# Different ICU events as step-up vs non step-up
icu_events <- all_outcomes %>%
  filter(EVENT_TYPE == "ICU")
icu_adt <- chartwatch::get_patient_adt(con_edw) %>%
  filter(ENCOUNTER_NUM %in% icu_events$ENCOUNTER_NUM)

step_up_events <- icu_events  %>%
  left_join(icu_adt, by = c("ENCOUNTER_NUM", "EVENT_TS")) %>%
  mutate(step_up = if_else(grepl("STEP UP", UNIT_DESCR), 1, 0)) %>%
  select(ENCOUNTER_NUM, EVENT_TS, EVENT_TYPE, step_up)

odbc::dbDisconnect(con_edw)

outcomes <- rbind(
  all_outcomes,
  deaths_outside_ward
) %>%
  left_join(step_up_events, by = c("ENCOUNTER_NUM", "EVENT_TS", "EVENT_TYPE")) %>%
  
  dplyr::mutate(EVENT_TYPE_UPDATED = if_else(EVENT_TS >= CUTOFF_DATE, "Discharge", EVENT_TYPE))


write.csv(outcomes, file = FINAL_PAPER_TEST_OUTCOMES_FILENAME, row.names = FALSE)
