#'  ***********************************************************************
#'  ***********************************************************************
#'  ***********************************************************************
#'  Script name:
#'  Initially created by: Chloe Pou-Prom
#'  Date created: Feb 1, 2021
#'  Maintainer information: 
#'
#'  Script contents: Read correct data files and extract relevant data needed
#'  for creating descriptive stats report for paper.
#'  
#'  Additionally, this code will process the VALIDATION DATA to filter out any 
#'  visits that also appear in the prospective testing set.
#'
#'  ***********************************************************************
#'  ***********************************************************************
#'  ***********************************************************************

library(dplyr)
library(lubridate)
library(keyring)
library(chartdb)


# Establish train/valid/test sets -----------------------------------------

# Pull data from EDW
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

# Load all inpatient data and pull relevant information
inp <- dim_tbl(con_edw, "INPATIENT_ENCOUNTER_FACT") %>% 
  select(ENCOUNTER_NUM, PATIENT_DK, MOST_RESPONSIBLE_DIAGNOSIS_CD_DK,
         ADT_DISCHARGE_DISPOSITION_DK, ADMIT_TS, DISCHARGE_TS)
diag <- dim_tbl(con_edw, "DIAGNOSIS_CODE_DIM") %>% 
  select(DIAGNOSIS_CD_DK, DIAGNOSIS_DESCR, DIAGNOSIS_CLASS_LEVEL_4_DESCR)
disp <- dim_tbl(con_edw, "DISCHARGE_DISPOSITION_DIM") %>% 
  select(DISCHARGE_DISPOSITION_DK, DISCHARGE_DISPOSITION_DESCR)
pat <- dim_tbl(con_edw, "PATIENT_DIM") %>% 
  select(PATIENT_DK, DATE_OF_BIRTH, GENDER_CD)
inp <- inp %>% 
  left_join(diag, by = c("MOST_RESPONSIBLE_DIAGNOSIS_CD_DK" = "DIAGNOSIS_CD_DK")) %>% 
  left_join(disp, by = c("ADT_DISCHARGE_DISPOSITION_DK" = "DISCHARGE_DISPOSITION_DK") ) %>% 
  left_join(pat, by = c("PATIENT_DK" = "PATIENT_DK") ) %>% 
  collect() %>% 
  mutate(age = as.numeric(difftime(as_date(ADMIT_TS), DATE_OF_BIRTH, units = "days"))/365.25) %>% 
  mutate(gender = ifelse(GENDER_CD == "F", 1, 0))

# Find all data files
# From calculate-descriptive-statistics.R
fs <- list.files('/mnt/research/LKS-CHART/Projects/gim_ews_project/data/retraining_2020_0120/train', full.names = T)
test_encounters <- read.csv("/mnt/research/LKS-CHART/Projects/gim_ews_project/data/paper-data/test_encounters_with_outcomes.csv", 
                            stringsAsFactors = FALSE) 


# Load encounters 
# Splits used in paper:
# Train: April 2, 2011 - October 31, 2018
# Validation: November 1, 2018 - April 30, 2019
# Prospective test set: May 1, 2019 - September 30, 2019
encounters <- data.table::fread(fs[5]) 
train_encounters <- encounters %>% 
  filter(as_date(ADMIT_TS) >= ymd('2011-04-02'),
         as_date(ADMIT_TS) <= ymd('2018-10-31'))
valid_encounters <- encounters %>% 
  filter(as_date(ADMIT_TS) > ymd('2018-10-31'),
         as_date(ADMIT_TS) <= ymd('2019-04-30'))

train_encounters_data <- inp %>% filter(ENCOUNTER_NUM %in% train_encounters$ENCOUNTER_NUM)
write.csv(train_encounters_data,
          "/mnt/research/LKS-CHART/Projects/gim_ews_project/data/paper-data/train_encounters_data.csv")

# There are two different ways to split the validation/test data
# METHOD 1: Keep all test encounters. Filter validation encounters
# to remove any test encounters that appeared in it. 
# Since the train/valid split is based on calendar date of the
# admission timestamp, there may be some test encounters that
# show up in the valid encounters dataset.
# METHOD 2: Keep all valid encounters. Filter test encounters
# so that they only include data whose start date was May 1, 2019

# METHOD 1
valid_encounters_data_method1 <-  inp %>% 
  filter(ENCOUNTER_NUM %in% valid_encounters$ENCOUNTER_NUM) %>%
  filter(!ENCOUNTER_NUM %in% test_encounters$ENCOUNTER_NUM)
write.csv(valid_encounters_data_method1,
          "/mnt/research/LKS-CHART/Projects/gim_ews_project/data/paper-data/valid_encounters_data_method1.csv")
valid_encounters_method1 <- valid_encounters %>%
  filter(ENCOUNTER_NUM %in% valid_encounters_data_method1$ENCOUNTER_NUM)
write.csv(valid_encounters_method1,
          "/mnt/research/LKS-CHART/Projects/gim_ews_project/data/paper-data/valid_encounters_method1.csv")
test_encounters_data_method1 <- inp %>% 
  filter(ENCOUNTER_NUM %in% test_encounters$ENCOUNTER_NUM)
write.csv(test_encounters_data_method1,
          "/mnt/research/LKS-CHART/Projects/gim_ews_project/data/paper-data/test_encounters_data_method1.csv")

# METHOD 2
# 74 encounters get filtered out after removing test encounters
valid_encounters_data_method2 <- inp %>% 
  filter(ENCOUNTER_NUM %in% valid_encounters$ENCOUNTER_NUM)
write.csv(valid_encounters_data_method2,
          "/mnt/research/LKS-CHART/Projects/gim_ews_project/data/paper-data/valid_encounters_data_method2.csv")
valid_encounters_method2 <- valid_encounters %>%
  filter(ENCOUNTER_NUM %in% valid_encounters_data_method2$ENCOUNTER_NUM)
write.csv(valid_encounters_method2,
          "/mnt/research/LKS-CHART/Projects/gim_ews_project/data/paper-data/valid_encounters_method2.csv")
test_encounters_data_method2 <- inp %>% 
  filter(ENCOUNTER_NUM %in% test_encounters$ENCOUNTER_NUM) %>%
  filter(lubridate::as_date(ADMIT_TS) >= lubridate::ymd("2019-05-01"))
write.csv(test_encounters_data_method2,
          "/mnt/research/LKS-CHART/Projects/gim_ews_project/data/paper-data/test_encounters_data_method2.csv")



# Update validation data files --------------------------------------------

valid_demographics <- read.csv("/mnt/research/LKS-CHART/Projects/gim_ews_project/data/paper-data/valid_demographics.csv")
valid_numeric_ts <- read.csv("/mnt/research/LKS-CHART/Projects/gim_ews_project/data/paper-data/valid_numeric_timeseries.csv")
valid_outcome_ts <- read.csv("/mnt/research/LKS-CHART/Projects/gim_ews_project/data/paper-data/valid_outcome_timeseries.csv")


valid_demographics_method1 <- valid_demographics %>%
  filter(ENCOUNTER_NUM %in% valid_encounters_data_method1$ENCOUNTER_NUM)
valid_numeric_ts_method1 <- valid_numeric_ts %>%
  filter(ENCOUNTER_NUM %in% valid_encounters_data_method1$ENCOUNTER_NUM)
valid_outcome_ts_method1 <- valid_outcome_ts %>%
  filter(ENCOUNTER_NUM %in% valid_encounters_data_method1$ENCOUNTER_NUM)
write.csv(valid_demographics_method1,
          "/mnt/research/LKS-CHART/Projects/gim_ews_project/data/paper-data/valid_demographics_method1.csv")
write.csv(valid_numeric_ts_method1,
          "/mnt/research/LKS-CHART/Projects/gim_ews_project/data/paper-data/valid_numeric_timeseries_method1.csv")
write.csv(valid_outcome_ts_method1,
          "/mnt/research/LKS-CHART/Projects/gim_ews_project/data/paper-data/valid_outcome_timeseries_method1.csv")
valid_demographics_method2 <- valid_demographics %>%
  filter(ENCOUNTER_NUM %in% valid_encounters_data_method2$ENCOUNTER_NUM)
valid_numeric_ts_method2 <- valid_numeric_ts %>%
  filter(ENCOUNTER_NUM %in% valid_encounters_data_method2$ENCOUNTER_NUM)
valid_outcome_ts_method2 <- valid_outcome_ts %>%
  filter(ENCOUNTER_NUM %in% valid_encounters_data_method2$ENCOUNTER_NUM)
write.csv(valid_demographics_method2,
          "/mnt/research/LKS-CHART/Projects/gim_ews_project/data/paper-data/valid_demographics_method2.csv")
write.csv(valid_numeric_ts_method2,
          "/mnt/research/LKS-CHART/Projects/gim_ews_project/data/paper-data/valid_numeric_timeseries_method2.csv")
write.csv(valid_outcome_ts_method2,
          "/mnt/research/LKS-CHART/Projects/gim_ews_project/data/paper-data/valid_outcome_timeseries_method2.csv")


# Update vitals data ------------------------------------------------------

numeric_variables <- data.table::fread('/mnt/research/LKS-CHART/Projects/gim_ews_project/data/raw_data/numeric_variables.csv')
vitals <- numeric_variables %>% 
  filter(grepl('vital', variable))

# Train data
vitals_train <- vitals %>% 
  filter(ENCOUNTER_NUM %in% train_encounters$ENCOUNTER_NUM)
write.csv(vitals_train,
          "/mnt/research/LKS-CHART/Projects/gim_ews_project/data/paper-data/train_vitals_data.csv")


# Validation data
vitals_valid_method1 <- vitals %>% 
  filter(ENCOUNTER_NUM %in% valid_encounters_data_method1$ENCOUNTER_NUM)
write.csv(vitals_valid_method1, 
          "/mnt/research/LKS-CHART/Projects/gim_ews_project/data/paper-data/valid_vitals_data_method1.csv")
vitals_valid_method2 <- vitals %>% 
  filter(ENCOUNTER_NUM %in% valid_encounters_data_method2$ENCOUNTER_NUM)
write.csv(vitals_valid_method2, 
          "/mnt/research/LKS-CHART/Projects/gim_ews_project/data/paper-data/valid_vitals_data_method2.csv")

# Test data: https://github.com/murrayjw/chartwatch-paper/blob/master/calculate-descriptive-statistics.R#L249
load("/mnt/research/LKS-CHART/Projects/gim_ews_project/data/paper-data/soarian_obs_test.Rda")
vitals_data  <- soarian_obs_test %>% 
  mutate(VALUE= as.character(VALUE), FINDINGABBR = as.character(FINDINGABBR))
blood_pressure <- chartwatch::clean_bp_measures(vitals_data)
vitals_data <- vitals_data %>%
  dplyr::filter(!(FINDINGABBR %in% 'S_BP')) %>%
  dplyr::mutate(numeric_value = as.numeric(VALUE)) %>%
  dplyr::filter(!is.na(numeric_value)) %>%
  dplyr::bind_rows(blood_pressure) %>%
  dplyr::filter(!is.na(numeric_value)) %>%
  dplyr::mutate(numeric_value = ifelse(BASEUNITVALUE %in% c(0, -1), numeric_value, BASEUNITVALUE))
names(vitals_data) <- toupper(names(vitals_data)) # clean column names
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
  dplyr::mutate(numeric_value = ifelse(FINDINGNAME == "Temperature (c)" & numeric_value > 180, numeric_value - 273.15, numeric_value)) %>%
  dplyr::rename(variable = FINDINGABBR) %>%
  dplyr::mutate(variable = chartwatch::clean_var_name(variable))
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
  dplyr::arrange(ENCOUNTER_NUM, timestamp) %>% 
  # soarian records temp in kelvin; change to celcius
  mutate(numeric_value = ifelse(variable == 'vital_stemperature',
                                numeric_value - 273.15,
                                numeric_value))

vitals_test_method1 <- vitals_test %>%
  filter(ENCOUNTER_NUM %in% paste0("00", test_encounters_data_method1$ENCOUNTER_NUM))
write.csv(vitals_test_method1,
          "/mnt/research/LKS-CHART/Projects/gim_ews_project/data/paper-data/test_vitals_data_method1.csv")
vitals_test_method2 <- vitals_test %>%
  filter(ENCOUNTER_NUM %in% paste0("00", test_encounters_data_method2$ENCOUNTER_NUM))
write.csv(vitals_test_method2,
          "/mnt/research/LKS-CHART/Projects/gim_ews_project/data/paper-data/test_vitals_data_method2.csv")


# Update test data outcomes -----------------------------------------------

# Prospective testing set is from May 2019 to September 2019. 
# Follow up encounter for up to 30 days after test period (i.e., 
# up to October 2019). If patient is still in hospital by then,
# label their outcome as "Discharge"
test_encounters <- test_encounters %>% 
  
  mutate(outcome_type_updated = dplyr::if_else(outcome_type != "Discharge" &
                                     # Follow up for 1 month
                                     as.Date(OUTCOME_TS) >= as.Date("2019-10-31"), "Discharge", outcome_type)) 


test_encounters_method1 <- test_encounters %>%
  filter(ENCOUNTER_NUM %in% test_encounters_data_method1$ENCOUNTER_NUM)
write.csv(test_encounters_method1,
          "/mnt/research/LKS-CHART/Projects/gim_ews_project/data/paper-data/test_encounters_with_outcomes_method1.csv")
test_encounters_method2 <- test_encounters %>%
  filter(ENCOUNTER_NUM %in% test_encounters_data_method2$ENCOUNTER_NUM)
write.csv(test_encounters_method2,
          "/mnt/research/LKS-CHART/Projects/gim_ews_project/data/paper-data/test_encounters_with_outcomes_method2.csv")






