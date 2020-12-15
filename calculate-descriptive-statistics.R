
# load the required libraries ---------------------------------------------
library(dplyr)
library(recipes)
library(rsample)
library(tidymodels)
library(tune)
library(workflows)
library(lubridate)

con <- CHART::connect_edw(keyring::key_get("EDW_USERNAME"),
                          keyring::key_get("EDW_PASSWORD"))

inp <- dim_tbl(con, "INPATIENT_ENCOUNTER_FACT") %>% 
  select(ENCOUNTER_NUM, PATIENT_DK, 
         MOST_RESPONSIBLE_DIAGNOSIS_CD_DK,
         ADT_DISCHARGE_DISPOSITION_DK,
         ADMIT_TS, DISCHARGE_TS)
diag <- dim_tbl(con, "DIAGNOSIS_CODE_DIM") %>% 
  select(DIAGNOSIS_CD_DK, DIAGNOSIS_DESCR, DIAGNOSIS_CLASS_LEVEL_4_DESCR)
disp <- dim_tbl(con, "DISCHARGE_DISPOSITION_DIM") %>% 
  select(DISCHARGE_DISPOSITION_DK, DISCHARGE_DISPOSITION_DESCR)
pat <- dim_tbl(con, "PATIENT_DIM") %>% 
  select(PATIENT_DK, DATE_OF_BIRTH, GENDER_CD)
inp <- inp %>% 
  left_join(diag, by = c("MOST_RESPONSIBLE_DIAGNOSIS_CD_DK" = "DIAGNOSIS_CD_DK")) %>% 
  left_join(disp, by = c("ADT_DISCHARGE_DISPOSITION_DK" = "DISCHARGE_DISPOSITION_DK") ) %>% 
  left_join(pat, by = c("PATIENT_DK" = "PATIENT_DK") ) %>% 
  collect() %>% 
  mutate(age = as.numeric(difftime(as_date(ADMIT_TS), DATE_OF_BIRTH, units = "days"))/365.25) %>% 
  mutate(gender = ifelse(GENDER_CD == "F", 1, 0))




# directory to the training data ------------------------------------------
fs <- list.files('Z:/LKS-CHART/Projects/gim_ews_project/data/retraining_2020_0120/train', full.names = T)

# some helper functions
source("static-models/model-helpers.R")


# load the data -----------------------------------------------------------
numeric <- data.table::fread(fs[7])
encounters <- data.table::fread(fs[5])
demos <- data.table::fread(fs[4])
outcomes <- data.table::fread(fs[8])

train_encounters <- encounters %>% 
  filter(as_date(ADMIT_TS) >= ymd('2011-04-02'),
         as_date(ADMIT_TS) <= ymd('2018-10-31'))


valid_encounters <- encounters %>% 
  filter(as_date(ADMIT_TS) > ymd('2018-10-31'),
         as_date(ADMIT_TS) <= ymd('2019-04-30'))

train_numeric <- numeric %>% 
  filter(ENCOUNTER_NUM %in% train_encounters$ENCOUNTER_NUM)
train_demos <- demos %>% 
  filter(ENCOUNTER_NUM %in% train_encounters$ENCOUNTER_NUM)
train_outcomes <- outcomes %>% 
  filter(ENCOUNTER_NUM %in% train_encounters$ENCOUNTER_NUM)

valid_numeric <- numeric %>% 
  filter(ENCOUNTER_NUM %in% valid_encounters$ENCOUNTER_NUM)
valid_demos <- demos %>% 
  filter(ENCOUNTER_NUM %in% valid_encounters$ENCOUNTER_NUM)
valid_outcomes <- outcomes %>% 
  filter(ENCOUNTER_NUM %in% valid_encounters$ENCOUNTER_NUM)

readr::write_csv(train_numeric, "Z:/LKS-CHART/Projects/gim_ews_project/data/paper-data//train_numeric_timeseries.csv")
readr::write_csv(train_demos, "Z:/LKS-CHART/Projects/gim_ews_project/data/paper-data/train_demographics.csv")
readr::write_csv(train_encounters, "Z:/LKS-CHART/Projects/gim_ews_project/data/paper-datar/train_encounters.csv")
readr::write_csv(train_outcomes, "Z:/LKS-CHART/Projects/gim_ews_project/data/paper-data/train_outcome_timeseries.csv")

readr::write_csv(valid_numeric, "Z:/LKS-CHART/Projects/gim_ews_project/data/paper-data/valid_numeric_timeseries.csv")
readr::write_csv(valid_demos, "Z:/LKS-CHART/Projects/gim_ews_project/data/paper-data/valid_demographics.csv")
readr::write_csv(valid_encounters, "Z:/LKS-CHART/Projects/gim_ews_project/data-data/paper/valid_encounters.csv")
readr::write_csv(valid_outcomes, "Z:/LKS-CHART/Projects/gim_ews_project/data/paper-data/valid_outcome_timeseries.csv")




prediction_outcomes <- readr::read_csv('Z:\\LKS-CHART\\Projects\\gim_ews_preassessment_project\\data\\practitioner_predictions_outcomes.csv')
load("Z:\\LKS-CHART\\Projects\\gim_ews_preassessment_project\\data\\model_predictions.Rda")

phys_enc <- unique(prediction_outcomes$ENCOUNTER_NUM)
mod_enc <- unique(model_predictions$ENCOUNTER_NUM)

sd <- setdiff(phys_enc, mod_enc)

clinician <- prediction_outcomes %>% 
  filter(!ENCOUNTER_NUM %in% sd)


test_encounters <- clinician %>% 
  distinct(ENCOUNTER_NUM)

readr::write_csv(test_encounters, "Z:/LKS-CHART/Projects/gim_ews_project/data/paper-data/test_encounters.csv")


# descriptive statistics --------------------------------------------------

# N

# train
nrow(train_encounters)
nrow(valid_encounters)
nrow(test_encounters)

# age
age_sum <- function(data) {
  data %>% 
    summarize(mena_age = mean(age),
              sd_age = sd(age)) %>% 
    as.data.frame()
}

age_sum(train_encounters)
age_sum(valid_encounters)
age_sum(inp %>% filter(ENCOUNTER_NUM %in% test_encounters$ENCOUNTER_NUM))


# gender

gender_sum <- function(data) {
  data %>% 
    summarize(male = sum(gender),
              p = male/n())
  
}

gender_sum(train_encounters)
gender_sum(valid_encounters)
gender_sum(inp %>% filter(ENCOUNTER_NUM %in% test_encounters$ENCOUNTER_NUM))


# load raw data
numeric_variables <- data.table::fread('Z:\\LKS-CHART\\Projects\\gim_ews_project\\data\\raw_data\\numeric_variables.csv')

# diagnosis


diagnosis_sum <- function(data) {
  top5 <- c("Pneumonia unspecified",
            "Congestive heart failure",
            "COPD with acute exacerbation unspecified",
            "Urinary tract infection site not spec",
            "Gastrointestinal haemorrhage NOS")
  
  
  data %>% 
    mutate(DIAGNOSIS_DESCR = ifelse(DIAGNOSIS_DESCR %in% top5,
                                    DIAGNOSIS_DESCR, "other")) %>% 
    count(DIAGNOSIS_DESCR) %>% 
    ungroup() %>% 
    mutate(p = n/sum(n)) %>% 
    arrange(desc(n)) %>% 
    head(8) %>% as.data.frame()
}

diagnosis_sum(inp %>% filter(ENCOUNTER_NUM %in% train_encounters$ENCOUNTER_NUM))
diagnosis_sum(inp %>% filter(ENCOUNTER_NUM %in% valid_encounters$ENCOUNTER_NUM))
diagnosis_sum(inp %>% filter(ENCOUNTER_NUM %in% test_encounters$ENCOUNTER_NUM))

# discharge disposition

# We can collapse this into “home”, 
# “long term care”, “death”, 
# and left against medical advice.
# One major category I don’t see here is
# transfer to a rehabilitation hospital
# (would that go under “other”).
# Same with transfer to other acute care hospital, 
# but that’s a small number.

disp_sum <- function(data) {
  data %>% 
    mutate(disposition = case_when(
      DISCHARGE_DISPOSITION_DESCR %in% c("Home Without Services",
                                         "Home with Support Services") ~ "home",
      DISCHARGE_DISPOSITION_DESCR %in%
        c("Left Against Medical Advice/Walked Out", 
          "Absent Without Leave-staff not notified",
          "Did Not Return From Pass/Leave") ~ "LWBS",
      DISCHARGE_DISPOSITION_DESCR == "Long Term Care/Nursing Home/Detox" ~ "LTC",
      DISCHARGE_DISPOSITION_DESCR == "Expired                             *EXP" ~ "Death",
      DISCHARGE_DISPOSITION_DESCR %in%  c("Other Institution/ED/Day Surg/Ambulatory",
                                          "Supp Housing/Grp/Shelter/Retirement hme",
                                          "Palliative - Acute Hospital",
                                          "Inpt Care - Acute, Rehab, CCC, Psych") ~ "Other institution",
      TRUE ~ "Other"
    )) %>% 
    count(disposition) %>% 
    arrange(desc(n)) %>% 
    mutate(p = n/sum(n)) %>% as.data.frame()
  
}


disp_sum(inp %>% filter(ENCOUNTER_NUM %in% train_encounters$ENCOUNTER_NUM))
disp_sum(inp %>% filter(ENCOUNTER_NUM %in% valid_encounters$ENCOUNTER_NUM))
disp_sum(inp %>% filter(ENCOUNTER_NUM %in% test_encounters$ENCOUNTER_NUM))

# vitals

vitals <- numeric_variables %>% 
  filter(grepl('vital', variable))


vitals_train <- vitals %>% 
  filter(ENCOUNTER_NUM %in% train_encounters$ENCOUNTER_NUM)

vitals_valid <- vitals %>% 
  filter(ENCOUNTER_NUM %in% valid_encounters$ENCOUNTER_NUM)



vitals_sum <- function(data, train_data) {
  
  qs <- train_data %>% 
    group_by(variable) %>% 
    summarize(q1 = quantile(numeric_value, .01),
              q99 = quantile(numeric_value, .99))
  
  data <- data %>% 
    left_join(qs, by = "variable")
  data <- data %>% 
    mutate(numeric_value = ifelse( numeric_value < q1, q1,
                                   ifelse(numeric_value > q99, q99, numeric_value)))
  data %>% 
    group_by(ENCOUNTER_NUM, variable) %>% 
    arrange(timestamp) %>% 
    filter(row_number() == 1) %>% 
    group_by(variable) %>% 
    summarize(mean_value = mean(numeric_value),
              sd_value = sd(numeric_value),
              med_value = median(numeric_value),
              q25 = quantile(numeric_value, .25),
              q75 = quantile(numeric_value, .75)) %>% 
    as.data.frame()
  
}

# THe file soarian obs holds the soarian
# observations for the prospective test set

vitals_data = soarian_obs_test %>% 
  mutate(VALUE= as.character(VALUE), FINDINGABBR = as.character(FINDINGABBR))

# clean up the data using the first part of the clean_vitals function
# from the chartwatch package

blood_pressure <- clean_bp_measures(vitals_data)
vitals_data <- vitals_data %>%
  dplyr::filter(!(FINDINGABBR %in% 'S_BP')) %>%
  dplyr::mutate(numeric_value = as.numeric(VALUE)) %>%
  dplyr::filter(!is.na(numeric_value)) %>%
  dplyr::bind_rows(blood_pressure) %>%
  dplyr::filter(!is.na(numeric_value)) %>%
  dplyr::mutate(numeric_value = ifelse(BASEUNITVALUE %in% c(0, -1), numeric_value, BASEUNITVALUE))

# clean column names
names(vitals_data) <- toupper(names(vitals_data))
vitals_data <- vitals_data %>%
  dplyr::rename(numeric_value = NUMERIC_VALUE) %>%
  dplyr::select(-PATIENTVISIT_OID,
                -OBSERVATIONSTATUS,
                -VALUE,
                -MINVALUE,
                -MAXVALUE,
                -FINDINGDATATYPE,
                -ASSESSMENTSTATUS,
                -ENTEREDDT,
                -ORDERSASWRITTENTEXT,
                -OBJECTID,
                -contains("ASS_OBJECTID"),
                -ASSESSMENT_OID)

vitals_soarian <- vitals_data %>%
  dplyr::mutate(FINDINGNAME = ifelse(is.na(FINDINGNAME), "sbp", FINDINGNAME)) %>%
  dplyr::mutate(numeric_value = ifelse(FINDINGNAME == "Temperature", numeric_value - 273.15, numeric_value)) %>%
  dplyr::mutate(numeric_value = ifelse(FINDINGNAME == "Temperature (c)" & numeric_value > 180, numeric_value - 273.15, numeric_value))

vitals_soarian <- vitals_soarian %>%
  dplyr::rename(variable = FINDINGABBR) %>%
  dplyr::mutate(variable = clean_var_name(variable))


# keep most common vital signs
vital_signs <- vitals_soarian %>%
  dplyr::filter(variable %in% c("SBPSYSTOLIC",
                                "SBPDIASTOLIC",
                                "SPulse",
                                "SRespirations",
                                "SO2Saturation",
                                "STemperature",
                                "SPainIntRest",
                                "SPainIntMove",
                                "SFIO2"))

vitals_test <- vital_signs %>%
  dplyr::select(ENCOUNTER_NUM, variable, timestamp = OBS_CREATIONTIME, numeric_value) %>%
  dplyr::mutate(variable = paste0("vital_", tolower(variable)))%>%
  dplyr::arrange(ENCOUNTER_NUM, timestamp)

# soarian records temp in kelvin
# change to celcius
vitals_test <- vitals_test %>% 
  mutate(numeric_value = ifelse(variable == 'vital_stemperature',
                                numeric_value - 273.15,
                                numeric_value))
vitals_sum(vitals_train, vitals_train)
vitals_sum(vitals_valid, vitals_train)
vitals_sum(vitals_test, vitals_train)

# Length of stay

inp <- inp %>% 
  mutate(los = as.numeric(difftime(DISCHARGE_TS, ADMIT_TS, units = "days")))


los_sum <- function(data) {
  data %>% 
    filter( los < 30,
            los > .25) %>% 
    summarize(mean_los = median(los, na.rm = T),
              q25 = quantile(los, .25, na.rm = T),
              q75 = quantile(los, .75, na.rm = T)) %>% 
    as.data.frame()
  
}

inp <- inp %>% 
  mutate(los = as.numeric(difftime(DISCHARGE_TS, ADMIT_TS, units = 'days')))
los_sum(inp %>% filter(ENCOUNTER_NUM %in% train_encounters$ENCOUNTER_NUM))
los_sum(inp %>% filter(ENCOUNTER_NUM %in% valid_encounters$ENCOUNTER_NUM))
los_sum(inp %>% filter(ENCOUNTER_NUM %in% test_encounters$ENCOUNTER_NUM))



# outcome descriptive statistics -------------------------------------------

# training set
table(train_encounters$death)
prop.table(table(train_encounters$death))

table(train_encounters$post_gim_icu)
prop.table(table(train_encounters$post_gim_icu))

table(train_encounters$palliative_transfer)
prop.table(table(train_encounters$palliative_transfer))


median(train_encounters$gim_to_outcome)
quantile(train_encounters$gim_to_outcome, .25)
quantile(train_encounters$gim_to_outcome, .75)

# validation set
table(valid_encounters$OUTCOME_TYPE)
prop.table(table(valid_encounters$OUTCOME_TYPE))
median(valid_encounters$gim_to_outcome)
quantile(valid_encounters$gim_to_outcome, .25)
quantile(valid_encounters$gim_to_outcome, .75)

prediction_outcomes %>% 
  filter(OUTCOME_TYPE <5) %>%
  distinct(ENCOUNTER_NUM, OUTCOME_TS, OUTCOME_TYPE) %>% 
  count(OUTCOME_TYPE) %>% 
  mutate(p = n/sum(n)) %>% as.data.frame()

source('extract-data-helpers.R')


adt <- get_adt(con_edw)


clinician_tte <- prediction_outcomes %>%
  mutate(ENCOUNTER_NUM = as.character(ENCOUNTER_NUM)) %>% 
  distinct(ENCOUNTER_NUM, OUTCOME_TS, OUTCOME_TYPE) %>% 
  left_join(inp, by = "ENCOUNTER_NUM") %>% 
  mutate(gim_to_outcome = as.numeric(difftime(OUTCOME_TS, ADMIT_TS, units = 'days')))



median(clinician_tte$gim_to_outcome)
quantile(clinician_tte$gim_to_outcome, .25)
quantile(clinician_tte$gim_to_outcome, .75)
