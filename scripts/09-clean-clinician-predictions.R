#'  ***********************************************************************
#'  ***********************************************************************
#'  ***********************************************************************
#'  Script name:
#'  Initially created by: 
#'  Date created: March 9, 2021
#'  Maintainer information: 
#'
#'  Script contents: Clean clinician predictions
#'
#'  ***********************************************************************
#'  ***********************************************************************
#'  ***********************************************************************

library(dplyr)
library(tableone)
source("constants.R")

clinician_predictions <- read.csv("/mnt/research/LKS-CHART/Projects/gim_ews_preassessment_project/data/practitioner_predictions_outcomes.csv",
                                  stringsAsFactors = FALSE)
test_encounter_nums <- read.csv(FINAL_PAPER_TEST_ENCOUNTERS_FILENAME, 
                            stringsAsFactors = FALSE) %>%
  dplyr::pull(ENCOUNTER_NUM)

test_encounter_outcomes <- read.csv("/mnt/research/LKS-CHART/Projects/gim_ews_project/data/paper-data/test_encounters_with_outcomes_method2.csv", 
                                    stringsAsFactors = FALSE) 

# For patients whose outcome occurs after the prospective test period,
# we update their outcome to be "Discharge". If the patient is still
# in hospital 30 days after the end of the test period (Sept 2019),
# ignore any outcomes that happen after
updated_outcomes <- test_encounter_outcomes %>%
  dplyr::filter(outcome_type != "Discharge" & outcome_type_updated == "Discharge")

updated_clinician_predictions <- clinician_predictions %>%
  dplyr::filter(ENCOUNTER_NUM %in% test_encounter_nums) %>%
  dplyr::select(
    # Variables related to clinician
    clinicianid, professional_role,
    
    # Variables related to time of prediction
    date, timestamp,
    
    # Variables related to outcome
    next48, next48_conf, ever, ever_conf,
    
    # Variables related to outcome
    pal, death, icu, outcome, outcome48,
    
    # Variables related to encounter
    ENCOUNTER_NUM, MRN, patientid
  ) %>%
  
  # Overwrite encounters whose outcome happens outside of test follow-up period
  dplyr::mutate(
    outcome = ifelse(ENCOUNTER_NUM %in% updated_outcomes$ENCOUNTER_NUM, 0, outcome),
    outcome48 = ifelse(ENCOUNTER_NUM %in% updated_outcomes$ENCOUNTER_NUM, 0, outcome48),
    icu = ifelse(ENCOUNTER_NUM %in% updated_outcomes$ENCOUNTER_NUM, 0, icu),
    death = ifelse(ENCOUNTER_NUM %in% updated_outcomes$ENCOUNTER_NUM, 0, death),
    pal = ifelse(ENCOUNTER_NUM %in% updated_outcomes$ENCOUNTER_NUM, 0, pal)
  )


# Sanity check
encounter_level_outcomes <- updated_clinician_predictions %>%
  dplyr::group_by(ENCOUNTER_NUM) %>%
  summarize(
    outcome = max(outcome),
    icu = max(icu),
    palliative = max(pal),
    death = max(death)
  ) %>%
  dplyr::ungroup() 
tableone::CreateTableOne(data = encounter_level_outcomes,
                         vars = c("outcome", "icu", "palliative", "death"),
                         factorVars = c("outcome", "icu", "palliative", "death"))

write.csv(updated_clinician_predictions,
          file = FINAL_PAPER_TEST_CLINICIAN_PREDICTIONS_FILENAME,
          row.names = FALSE)
