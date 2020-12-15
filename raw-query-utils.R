#'   ***********************************************************************
#'   ***********************************************************************
#'   ***********************************************************************
#'   Initially created by: Josh Murray
#'   Date: October 23rd 2018
#'   contact information: murrayj@smh.ca
#'
#'   Script contents: This script contains useful functions for extracting
#'   the GIM early warning system cohort and outcome variables
#'
#'   ***********************************************************************
#'   ***********************************************************************
#'   ***********************************************************************



###############################################################################
# Function to Extract Admit/Discharge/Transfer records

#' Title get_icu_service_loc()
#'
#' @param con a connection to the EDW database
#' 
#'
#' @return A character vector containing the intensive care services as they 
#' appear in the SERVICE_TYPE_DIM
#' @export
#'
#' @examples

get_icu_service_loc <- function(con) {
  res <- dim_tbl(con, 'SERVICE_TYPE_DIM') %>% 
    select(SERVICE_TYPE_DK, 
           SERVICE = SERVICE_TYPE_DESCR,
           SERVICE_CD = SERVICE_TYPE_CD) %>% 
    collect() %>% 
    filter(grepl("inten", SERVICE, ignore.case = T))
  
  s <- res$SERVICE_CD
  
  icu_locs <- dim_tbl(con, 'POINT_OF_CARE_LOCATION_DIM') %>% 
    filter(LOCATION_SERVICE_CD %in% s) %>% 
    collect()
  l <- unique(icu_locs$UNIT_NUM)
  return(l)
  
}
###############################################################################



###############################################################################
#' Title get_patient_adt()
#'
#' @param con a connection to the EDW database
#' 
#'
#' @return a tibble with encounter numbers, event types, event types,
#' the service for the event and the unit of the event
#' @export
#'
#' @examples
get_patient_adt <- function(edw_con, patients) {
  
  patient_adm_event <- dim_tbl(edw_con,  'PATIENT_ADMIN_EVENT_FACT') %>% 
    filter(PATIENT_DK %in% patients)
  
  inpatients <- dim_tbl(edw_con, "INPATIENT_ENCOUNTER_FACT") %>% 
    select(ENCOUNTER_NUM, INPATIENT_ENCOUNTER_DK) 
  
  patient_adm_event <- patient_adm_event %>% 
    inner_join(inpatients, by = c("ENCOUNTER_DK" = "INPATIENT_ENCOUNTER_DK"))
  
  
  service_type <-  dim_tbl(edw_con, 'SERVICE_TYPE_DIM') %>% 
    select(SERVICE_TYPE_DK, 
           SERVICE = SERVICE_TYPE_DESCR,
           SERVICE_CD = SERVICE_TYPE_CD)
  
  location <- dim_tbl(edw_con, 'POINT_OF_CARE_LOCATION_DIM') %>% 
    select(POINT_OF_CARE_LOCATION_DK,
           UNIT = UNIT_NUM,
           LOCATION_SERVICE = LOCATION_SERVICE_DESCR)
  patient_adm_event <- patient_adm_event %>% 
    filter(EVENT_TYPE_CD != "PRE-ADMIT")
  
  patient_adm_event <- patient_adm_event %>% 
    select( ENCOUNTER_NUM,
            EVENT_TYPE_CD, 
            EVENT_TS, SERVICE_TO_DK, 
            SERVICE_FROM_DK,
            LOCATION_TO_DK,
            LOCATION_FROM_DK) %>% 
    left_join(service_type, 
              by = c("SERVICE_FROM_DK" = "SERVICE_TYPE_DK")) %>%
    rename(FROM_SERVICE = SERVICE, FROM_SERVICE_CD = SERVICE_CD) %>% 
    left_join(location, 
              by = c("LOCATION_FROM_DK" = "POINT_OF_CARE_LOCATION_DK")) %>% 
    rename(FROM_UNIT = UNIT, FROM_LOCATION_SERVICE = LOCATION_SERVICE) %>%
    left_join(service_type, 
              by = c("SERVICE_TO_DK" = "SERVICE_TYPE_DK")) %>% 
    left_join(location, 
              by = c("LOCATION_TO_DK" = "POINT_OF_CARE_LOCATION_DK")) %>% 
    select(-SERVICE_TO_DK, -LOCATION_TO_DK, -LOCATION_FROM_DK) %>% 
    collect()
  return(patient_adm_event)
  
}
###############################################################################



###############################################################################
# Function to Extract GIM encounters from the EDW
#' Title extract_gim_enc()
#'
#' @param data a data.frame obtained from `get_patient_adt()`
#' 
#'
#' @return a data.frame which is a subset of the input data containing patients
#' that spent time on the GIM ward
#' @export
#'
#' @examples
#' 
#' 
extract_gim_enc <- function(data) {
  
  # filter the data for GIM services
  data <- data %>% 
    mutate(gim = ifelse((UNIT %in% "14C" |
                           SERVICE %in% c("TMA GENERAL MEDICINE",
                                          "TMB GENERAL MEDICINE",
                                          "TMC GENERAL MEDICINE",
                                          "TMD GENERAL MEDICINE",
                                          "TME GENERAL MEDICINE",
                                          "INTERNAL MEDICINE")), 1,0)) %>% 
    filter(gim == 1)
  
  # take the first transfer to gim as the start time for the encounter
  data <- data %>% 
    group_by(ENCOUNTER_NUM) %>% 
    arrange(EVENT_TS) %>% 
    filter(row_number() == 1) %>% 
    rename(GIM_START_TS = EVENT_TS) %>% 
    ungroup()

  return(data)
  
}
###############################################################################



###############################################################################
# Function to Extract Patient Arrival times to SMH
#' Title get_patient_arrivals()
#'
#' @param data a data.frame obtained from `get_patient_adt()`
#' @encounters a vector of encounters numbers (ENCOUNTER_NUM)
#' 
#'
#' @return a data.frame containing patient REGISTRATION timestamps for each 
#' input encounter
#' @export
#'
#' @examples
#' 
#' 
get_patient_arrivals <- function(data, encounters) {
  arrivals <- data %>% 
    filter(ENCOUNTER_NUM %in% encounters, 
           EVENT_TYPE_CD != "REGISTRATION") %>% 
    group_by(ENCOUNTER_NUM) %>% 
    arrange(EVENT_TS) %>% 
    filter(row_number() == 1) %>% 
    select(ENCOUNTER_NUM, ARRIVAL_TS = EVENT_TS, FIRST_EVENT_TYPE = EVENT_TYPE_CD) %>% 
    ungroup() 
  return(arrivals)
}
###############################################################################


###############################################################################
# Function to Extract Patient Arrival times to SMH
#' Title get_first_icu_non_gim_trans()
#'
#' @param data a data.frame obtained from `get_patient_adt()`
#' @encounters a vector of encounters numbers (ENCOUNTER_NUM)
#' 
#'
#' @return a data.frame containing patient REGISTRATION timestamps for each 
#' input encounter
#' @export
#'
#' @examples
#' 
#' 
get_first_icu_non_gim_trans  <- function(data, encounters, icu_locations,
                                         icu_services) {
  
  first_icu_time <- data %>% 
    filter(ENCOUNTER_NUM %in% encounters) %>% 
    filter((LOCATION_SERVICE %in% icu_locations | SERVICE %in% icu_services | UNIT == "14CU") &
             !(FROM_SERVICE %in% c("TMA GENERAL MEDICINE",
                                 "TMB GENERAL MEDICINE",
                                 "TMC GENERAL MEDICINE",
                                 "TMD GENERAL MEDICINE",
                                 "TME GENERAL MEDICINE",
                                 "INTERNAL MEDICINE"))) %>% 
    group_by(ENCOUNTER_NUM) %>% 
    arrange(EVENT_TS) %>% 
    filter(row_number() == 1) %>% 
    select(ENCOUNTER_NUM, FIRST_ICU_TS = EVENT_TS, 
           FIRST_ICU_FROM_SERVICE = FROM_SERVICE,
             FIRST_ICU_TYPE = SERVICE) %>% 
    ungroup() 
   
  return(first_icu_time)
}
###############################################################################


###############################################################################
get_icu_from_gim_time  <- function(data, encounters, gim_data, 
                                         icu_location_services,
                                         icu_services) {
  
  icu_entry  <- data %>% 
    filter(ENCOUNTER_NUM %in% encounters) %>% 
    filter((LOCATION_SERVICE %in% icu_location_services |
             SERVICE %in% icu_services |
             UNIT == "14CU") &
             FROM_UNIT == "14C") %>% 
    group_by(ENCOUNTER_NUM) %>% 
    arrange(EVENT_TS) %>% 
    filter(row_number() == 1) %>% 
    select(ENCOUNTER_NUM, ICU_TS = EVENT_TS, ICU_SERVICE = SERVICE) %>% 
    ungroup() %>% 
    left_join(gim_data %>% select(ENCOUNTER_NUM, GIM_START_TS), by = "ENCOUNTER_NUM") %>% 
    filter(ICU_TS > GIM_START_TS) %>% 
    select(-GIM_START_TS)
  
  return(icu_entry)
}
###############################################################################


###############################################################################
get_pal_from_gim_time  <- function(data, 
                                   encounters = gim_encounters$ENCOUNTER_NUM,
                                   gim_data = gim_encounters,
                                   pal_location_services = "PALLIATIVE",
                                   pal_services = c("PAL", "PALLIATIVE",
                                                    "PALLIATIVE")) {
  pal_transfer_time <- data %>% 
    filter(ENCOUNTER_NUM %in% encounters) %>% 
    inner_join(gim_data %>% select(ENCOUNTER_NUM, GIM_START_TS), by = "ENCOUNTER_NUM") %>% 
    filter(EVENT_TS > GIM_START_TS) %>% 
    filter(LOCATION_SERVICE %in% pal_location_services |
             SERVICE %in% pal_services) %>% 
    group_by(ENCOUNTER_NUM) %>% 
    summarize(PALLIATIVE_ENTRY = min(EVENT_TS, na.rm = T)) %>% 
    ungroup()
  
  return(pal_transfer_time)
}
###############################################################################


###############################################################################
get_inpatient_data <- function(con, data) {
  inpatient_df <- dim_tbl(con, "INPATIENT_ENCOUNTER_FACT") %>% 
    filter(ENCOUNTER_NUM %in% data$ENCOUNTER_NUM) %>% 
    select(ENCOUNTER_NUM, ADMIT_TS, DISCHARGE_TS,
           PATIENT_DK,
           ADT_DISCHARGE_DISPOSITION_DK,
           CIHI_DISCHARGE_DISPOSITION_DK,
           MOST_RESPONSIBLE_DIAGNOSIS_CD_DK,
           ATTENDING_PRACTITIONER_DK) %>% 
    collect()
  
  disp <- dim_tbl(con, "DISCHARGE_DISPOSITION_DIM") %>% 
    select(DISCHARGE_DISPOSITION_DK, DISCHARGE_DISPOSITION_DESCR)%>% 
    collect()
  diag_cd <- dim_tbl(con, "DIAGNOSIS_CODE_DIM") %>% 
    select(DIAGNOSIS_CD_DK, ICD10 = DIAGNOSIS_CD, 
           MRP_DIAGNOSIS = DIAGNOSIS_DESCR)%>% 
    collect() %>% 
    group_by(DIAGNOSIS_CD_DK) %>% 
    filter(row_number() == 1) %>% 
    ungroup()
  patient <- dim_tbl(con, "PATIENT_DIM") %>% 
    select(PATIENT_DK, GENDER_CD, DATE_OF_BIRTH)%>% 
    collect()
  
  inpatient_df <- inpatient_df %>% 
    left_join(disp, by = c("ADT_DISCHARGE_DISPOSITION_DK" = "DISCHARGE_DISPOSITION_DK")) %>% 
    rename(ADT_DISCHARGE = DISCHARGE_DISPOSITION_DESCR) %>% 
    left_join(disp, by = c("CIHI_DISCHARGE_DISPOSITION_DK" = "DISCHARGE_DISPOSITION_DK")) %>% 
    rename(CIHI_DISCHARGE = DISCHARGE_DISPOSITION_DESCR) %>% 
    left_join(diag_cd, by = c("MOST_RESPONSIBLE_DIAGNOSIS_CD_DK" = "DIAGNOSIS_CD_DK")) %>% 
    left_join(patient, by = "PATIENT_DK") %>% 
    collect() %>% 
  mutate(age = as.numeric(difftime(ADMIT_TS, DATE_OF_BIRTH, units = 'days'))/365.25,
         gender = ifelse(GENDER_CD == 'F', 1, 0),
         PATIENT_DK = as.character(PATIENT_DK),
         PRACT_DK = as.character(ATTENDING_PRACTITIONER_DK)) %>% 
    select(-DATE_OF_BIRTH, -ADT_DISCHARGE_DISPOSITION_DK,
           -CIHI_DISCHARGE_DISPOSITION_DK, -MOST_RESPONSIBLE_DIAGNOSIS_CD_DK, -GENDER_CD)
  
  return(inpatient_df)
}
###############################################################################



# get patient location time series ----------------------------------------

###############################################################################
seq_id <- function(x) {
  cnt <- 1
  res <- c()
  
  for(i in 1:length(x)) {
    if(i == 1) {
      res[i] <- cnt
    } else {
      if(x[i] == x[i-1]) {
        res[i] <- cnt
      } else{
        cnt <- cnt + 1
        res[i] <- cnt
        
      }
    }
  }
  
  return(res)
}
###############################################################################

###############################################################################
get_location_timeseries <- function(data) {
  res<- data %>% 
    arrange(EVENT_TS) %>% 
    group_by(ENCOUNTER_NUM) %>% 
    mutate(lag_unit = lag(UNIT),
           lag_time = lag(EVENT_TS)) %>% 
    select(ENCOUNTER_NUM, EVENT_TYPE_CD, EVENT_TS, FROM_UNIT, FROM_SERVICE, UNIT, lag_unit, lag_time,
           SERVICE) %>% 
    mutate(time_spent = as.numeric(difftime(EVENT_TS, lag_time, units = "days"))) %>% 
    select(ENCOUNTER_NUM, from_time = lag_time, to_time = EVENT_TS, unit = lag_unit, time_spent,
           SERVICE) %>% 
    filter(!is.na(unit)) %>% 
    group_by(ENCOUNTER_NUM) %>% 
    mutate(unit_seq = seq_id(unit)) %>% 
    group_by(ENCOUNTER_NUM, unit_seq) %>% 
    summarize(unit = max(unit),
              service = max(SERVICE),
              start_time = min(from_time),
              end_time = max(to_time),
              los = sum(time_spent)) %>% 
    ungroup()
  
  return(res)
  
}
###############################################################################


###############################################################################
# Function to extract encounter numbers of death timestamps


#' Title extract_gim_enc()
#'
#' @param edw_con a connection to the EDW database
#' 
#'
#' @return a data.frame with encounter numbers and death timestamps
#' @export
#'
#' @examples
#' 
get_death_ts <- function(edw_con){

  
  inpatient_enc <- tbl(edw_con, in_schema("DWM", "INPATIENT_ENCOUNTER_FACT"))
  
  discharge <- tbl(edw_con, in_schema("DWM", "DISCHARGE_DISPOSITION_DIM")) %>% 
    select(DISCHARGE_DISPOSITION_DK, DISCHARGE_DISPOSITION_DESCR)
  
  inpatient_enc <- inpatient_enc %>% 
    left_join(discharge,
              by = c('ADT_DISCHARGE_DISPOSITION_DK' = 'DISCHARGE_DISPOSITION_DK')) %>% 
    rename(ADT_DISP = DISCHARGE_DISPOSITION_DESCR) 

  expired_enc <- inpatient_enc %>% filter(ADT_DISP == 'Expired                             *EXP') %>%   
    select(ENCOUNTER_NUM,
           DEATH_TS = DISCHARGE_TS)
  
  return(expired_enc)  
}
###############################################################################


###############################################################################
# Function to extract demographic information


#' Title get_demographics()
#'
#' @param gim_enc_data a data.frame with encounter numbers
#' @param edw_con a data.frame with encounter numbers
#' 
#'
#' @return a data.frame with demographic variables obtained from the EDW
#' @export
#'
#' @examples
#' 
get_demographics <- function(gim_enc_data, edw_con) {
  
  if(inherits(gim_enc_data, "tbl_dbi")) {
    gim_enc_data <- gim_enc_data %>% 
      select(ENCOUNTER_NUM) %>% 
      collect()
    encounters <- gim_enc_data$ENCOUNTER_NUM
  } else {
    gim_enc_data <-gim_enc_data %>% 
      select(ENCOUNTER_NUM)
    encounters <- gim_enc_data$ENCOUNTER_NUM
  }
  
  cat('downloading inpatient data:\n')
  inpatient <- dim_tbl(edw_con,  'INPATIENT_ENCOUNTER_FACT') %>% 
    filter(ENCOUNTER_NUM %in% encounters)
  
  inpatient <- inpatient %>% group_by(PATIENT_DK) %>% 
    arrange(ADMIT_TS) %>% 
    mutate(lag_admit = lag(ADMIT_TS),
           lag_los = lag(ACTUAL_LOS),
           previous_visit = date_part('days', ADMIT_TS - lag_admit),
           adm_prev30 = ifelse(previous_visit > 0 & previous_visit <=30, 1, 0)) %>% 
    ungroup()
  
  inpatient <- inpatient %>% 
    mutate(prev_los = ifelse(adm_prev30 == 1, lag_los, 0))
  
  all_inpatient <-inpatient %>%
    select(ENCOUNTER_NUM, 
           ADMIT_TS, DISCHARGE_TS, PATIENT_DK,
           adm_prev30, prev_los) 
    
  
  patient_dim <- dim_tbl(edw_con, 'PATIENT_DIM') %>% 
    select(PATIENT_DK, DATE_OF_BIRTH, GENDER_CD, 
           LIVING_STATUS_CD, MEDICAL_RECORD_NUM,
           CONFIDENTIALITY_DESCR, HEALTH_CARD_NUM) %>% 
    mutate(NO_HEALTH_CARD = ifelse(HEALTH_CARD_NUM == '0', 1, 0))
  
  all_inpatient <- all_inpatient %>%
    left_join(patient_dim, by = 'PATIENT_DK')
  
  
  # data cleaning -----------------------------------------------------------
  
  all_inpatient <- all_inpatient %>% 
    collect()

  return(all_inpatient)
}
###############################################################################



###############################################################################


###############################################################################
get_clinical_orders <- function(soarian_encounters, con ) {
  
  soarian_orders <- tbl(con, in_schema("soarian", 'horder_staging')) %>% 
    filter(ORDERTYPEIDENTIFIER == 'DG') %>% 
    select(ORDER_OID = OBJECTID, PATIENTVISIT_OID, ORDERNAME, 
           ORDERTYPEABBR,  ORDERABBREVIATION, ENTEREDDATETIME,
           ORDERDESCASWRITTEN,
           PRIORITY, COMMENT, STARTDATETIME, STOPDATETIME,
           ORDERSUBTYPEABBR,
           COMMONDEFNAME,
           CREATIONTIME,
           ORDERSUPPINFO_OID,
           SRVTYPEBLMNEMONIC)
  
  order_history <- tbl(con, in_schema('soarian', 
                                      'hordertransitionrecord_staging')) %>% 
    select(ORDER_OID, TOORDERSTATUSMODIFIER, CHANGEDDATETIME)
  
  
  # order_history <- order_history %>% 
  #   filter(TOORDERSTATUSMODIFIER %in% 
  #            c('Active', 'Complete','Discontinue','Suspend'))
  
  soarian_orders <- soarian_orders %>% 
    inner_join(order_history, 
               by = 'ORDER_OID') 
  
  patient_visit <- tbl(con, in_schema("soarian", "HPatientVisit_staging")) %>% 
    select(PATIENTVISIT_OID = OBJECTID, 
           ENCOUNTER_NUM = PATIENTACCOUNTID, 
           PATIENT_OID)
  
  supp_info <- tbl(con, in_schema("soarian", "hordersuppinfo_staging")) %>% 
    select(OBJECTID ,
           CONSULTREQUESTVALUES,
           REASONFORREQUEST,
           DIETMODIFIER1,
           DIETMODIFIER2,
           DIETMODIFIER3,
           ORDERSOURCEABBR,
           CONDITION)
  soarian_orders <- soarian_orders %>% 
    left_join(supp_info, by = c('ORDERSUPPINFO_OID' = 'OBJECTID'))
  
  
  soarian_orders <- soarian_orders %>% 
    inner_join(patient_visit, 
               by = 'PATIENTVISIT_OID')
  
  soarian_orders <- soarian_orders %>% 
    filter(ENCOUNTER_NUM %in% soarian_encounters) %>% 
    collect() %>% 
    select(-PATIENTVISIT_OID, -PATIENT_OID, -ORDERNAME,
           -ORDERSUPPINFO_OID) %>% 
    select(ENCOUNTER_NUM, ORDERTYPEABBR,
           ORDERABBREVIATION, COMMONDEFNAME,
           ENTEREDDATETIME, 
           CHANGEDDATETIME, 
           everything()) 
  
  
  
  return(soarian_orders)
}
###############################################################################


###############################################################################
# Function to extract lab results from soarian


#' Title extract_soarian_labs()
#'
#' @param con a database connection to soarian
#' @param encounter_list a vector of soarian encounters
#' 
#'
#' @return a data.frame with soarian lab results
#' @export
#'
#' @examples
#' 
#' 
extract_soarian_labs <- function(con, encounter_list) {
  
  labs <- tbl(con, in_schema("", 'HinvestigationResult')) %>% 
    filter(IsDeleted == 0, ResultStatus == 'F',
           ResultDataType %in% c('AN', 'NM')) %>% 
    select(PatientVisit_oid, Patient_oid,
           ServiceProviderOrderID, InvalidationDateTime,
           FindingAbbreviation, ResultValue, 
           CreationTime, ResultDateTime,
           ObservationDateTime, 
           AbnormalFlag, ReferenceRange,
           ResultSuppInfo_oid)
  
  
  supp_info <- tbl(con, in_schema('', 'HInvestigationResultSuppInfo_Active')) %>% 
    select(ObjectID, FindingName)
  
  
  patvisit <- tbl(con, in_schema("", "HPatientVisit")) %>% 
    filter(alternatevisitid == '',
           PatientAccountID %in% encounter_list) %>% 
    select(PatientVisit_oid = ObjectID, PatientAccountID,
           Patient_oid) 
  
  
  labs <- labs %>% inner_join(patvisit, 
                              by = c('PatientVisit_oid' = 'PatientVisit_oid',
                                     'Patient_oid' = 'Patient_oid')) %>% 
    left_join(supp_info, by = c('ResultSuppInfo_oid' = 'ObjectID'))
}
###############################################################################


###############################################################################
# Function to extract soarian observations


#' Title extract_soarian_obs()
#'
#' @param con a database connection to soarian
#' @param encounter_list a vector of soarian encounters
#' 
#'
#' @return a data.frame with demographic variables cleaned for analysis
#' @export
#'
#' @examples
#' 
#' 
extract_soarian_obs <- function(encounter_list, con){
  
  observations <- tbl(con, in_schema("soarian","HObservation_staging")) %>% 
    filter(is.na(ENDDT)) %>% 
    select(OBJECTID,PATIENT_OID, ASSESSMENT_OID, ASSESSMENTID,
           OBS_CREATIONTIME = CREATIONTIME,
           PATIENT_OID,  FINDINGABBR, FINDINGDATATYPE,
           FINDINGNAME,OBSERVATIONSTATUS,
           VALUE,  MINVALUE, MAXVALUE, BASEUNITVALUE,
           ENDDT_OBS = ENDDT) %>% 
    filter(substr(OBSERVATIONSTATUS, 1, 1) == 'A')
  
  assessments <- tbl(con, in_schema("soarian", "HAssessment_staging"))  %>% 
    filter(is.na(ENDDT)) %>% 
    select(ass_ObjectID = OBJECTID, ASSESSMENTID, 
           PATIENTVISIT_OID,
           PATIENT_OID,
           ASSESSMENTSTATUS,
           ENTEREDDT, 
           FORMUSAGEDISPLAYNAME, 
           ORDERSASWRITTENTEXT,
           FORMUSAGE,
           ENDDT_ASS = ENDDT) %>% 
    mutate(erroneous = ifelse(ASSESSMENTSTATUS == 'Erroneous', 1, 0)) %>% 
    group_by(ASSESSMENTID) %>% 
    mutate(erroneous = max(erroneous)) %>% 
    filter(erroneous == 0) %>% 
    ungroup()
    
  
  soarian_data <- assessments %>% 
    inner_join(observations,
               by = c("PATIENT_OID" = "PATIENT_OID",
                      "ASSESSMENTID" = 'ASSESSMENTID'))
  
  patient_visit <- tbl(con, in_schema("soarian", "HPatientVisit_staging")) %>% 
    select(PATIENTVISIT_OID = OBJECTID, 
           ENCOUNTER_NUM = PATIENTACCOUNTID) %>% 
    filter(ENCOUNTER_NUM %in% encounter_list)
  
  soarian_data <- soarian_data %>% 
    inner_join(patient_visit,
               by = c("PATIENTVISIT_OID" = "PATIENTVISIT_OID")) 
  
  
  soarian_data <- soarian_data 
  
  return(soarian_data)                  
}
###############################################################################


###############################################################################
# Function to extract soarian medication order history


#' Title extract_medication_orders()
#'
#' @param con a database connection to soarian
#' @param encounter_list a vector of soarian encounters
#' @param num_splits the number of partitions of the data. this is used for 
#' extracting large amounts of data at one time.
#' 
#'
#' @return a remote data.frame with medication order history
#' @export
#'
#' @examples
#' 
#' 
extract_medication_orders <- function(con, encounter_list) {

  
  patient_visit <- tbl(con, in_schema("soarian", "HPatientVisit_staging")) %>% 
    select(PATIENTVISIT_OID = OBJECTID, 
           ENCOUNTER_NUM = PATIENTACCOUNTID, 
           PATIENT_OID) %>% 
    filter(ENCOUNTER_NUM %in% encounter_list)
  
  med_ordersh <- tbl(con, in_schema("soarian", 'HmedOrderh_staging')) %>% 
    filter(ISDELETED == 0) %>% 
    select(ORDER_OID = OBJECTID, PATIENTVISIT_OID, 
           ORDERASWRITTEN, REASON, DIRECTIONS, PRNREASON,
           STARTDATETIME, STOPDATETIME,
           MEDORDERSTATUSDATETIME, MEDORDERSTATUSMODIFIER,
           IVMETHODSTRING)
  
  med_order <- tbl(con, in_schema("soarian", 'HmedOrder_staging')) %>% 
    filter(ISDELETED == 0) %>% 
    select(ORDER_OID = OBJECTID, PATIENTVISIT_OID)
  
  med_comp <-  tbl(con, in_schema("soarian", 'HmedOrderComponent_staging')) %>% 
    filter(ISDELETED == 0) %>% 
    dplyr::select(MEDORDER_OID, GENERICDRUGNAME, BRANDDRUGNAME,
                  DOSEFORM, DRUGVOLUME, DRUGVOLUMEUNIT)
  
  meds <- med_ordersh %>% 
    inner_join(med_order, by = c("ORDER_OID", "PATIENTVISIT_OID")) %>% 
    inner_join(med_comp, by = c("ORDER_OID" = "MEDORDER_OID"))
  
  meds <- meds %>% 
    inner_join(patient_visit, by = c('PATIENTVISIT_OID'))

  return(meds)
  
}
###############################################################################


###############################################################################
# get ed start times ------------------------------------------------------

get_start_time <- function(con, encounter_list) {
  
  ed_new <- dim_tbl(con, "EMERGENCY_ENCOUNTER_FACT") %>% 
    select(ENCOUNTER_NUM, TRIAGE_TS = PRESENTING_TS) %>% 
    collect()
  ed_old <- dim_tbl(con, "LEGACY_EDIS_ENCOUNTER_FACT") %>% 
    select(ENCOUNTER_NUM, TRIAGE_TS) %>% 
    collect()
  
  ed_start <- ed_new %>% 
    bind_rows(ed_old)
  
  ed_start <- ed_start %>% 
    filter(ENCOUNTER_NUM %in% encounter_list)
  ed_start <- ed_start %>% 
    group_by(ENCOUNTER_NUM) %>% 
    filter(row_number() == 1) %>% 
    ungroup()
  return(ed_start)
}
###############################################################################


###############################################################################
# discharge disposition ---------------------------------------------------

get_palliative_dc <- function(con, encounter_list) {
  
  inp <- dim_tbl(con, "INPATIENT_ENCOUNTER_FACT") %>% 
    select(ENCOUNTER_NUM, 
           ADT_DISCHARGE_DISPOSITION_DK, DISCHARGE_TS)
  
  disp <- dim_tbl(con, "DISCHARGE_DISPOSITION_DIM") %>% 
    select(DISCHARGE_DISPOSITION_DK, 
           DISCHARGE_DISPOSITION_CD)
  
  inp <- inp %>% 
    left_join(disp,
              by = c("ADT_DISCHARGE_DISPOSITION_DK" = "DISCHARGE_DISPOSITION_DK")) %>% 
    rename(ADT_DISP_CD = DISCHARGE_DISPOSITION_CD) %>% 
    filter(ADT_DISP_CD %in% c("P", "Q")) %>% 
    collect()
  
 
  inp <- inp %>% 
    filter(ENCOUNTER_NUM %in% encounter_list) %>% 
    select(ENCOUNTER_NUM, PALLIATIVE_DC_TS = DISCHARGE_TS)
  
  return(inp)
}
###############################################################################



###############################################################################
# Function to extract a homelessness indicator for each patient

#' Title get_housing_indicator
#' 
#' The following is a list of current shelter addresses
#' 
#' 43 CHRISTIE ST is refugee welcome center
#' 101 ontario street is sojourne house
#' 191 SPADINA ROAD is toronto community hostel
#' 129 PETER ST is streets to homes assessment center
#' 100 LIPPINCOTT is COSTI Reception Centre 
#' 2714 DANFORTH AVE is dixon hall
#' 1161 CALEDONIA ST is fred victor bethlehem united shelter
#' 3576 ST. CLAIR AVE E is homes first society
#' 973 LANDSDOWNE AVE is christie ossington men's shelter
#' 616 VAUGHAN RD is cornerstone place
#' 349 GEORGE ST is dixon hall schoolhouse
#' 14 VAUGHAN RD is native men's hosue
#' 107 JARVIS ST. is salvation army-gateway
#' 135 SHERBOURNE ST is salvation army-maxwell meighen
#' 502 SPADINA is scott mission overnight shelter
#' 525 BLOOR ST is st simon's shelter
#' 1322 BLOOR ST WEST is homes first society
#' 2808 DUNDAS STREET WEST" is salvation army evangeline residence
#' 723 QUEEN ST WEST is salvation army florence booth
#' 70 GERRARD ST is st vincent de paul-mary's home
#' 87 PEMBROKE is Street haven at the crossroads
#' 647 DUNDAS STREET is women's residence main site
#' 80 WOODLAWN AVE is YWCA first stop woodlawn
#' 20 GERRARD ST E is Covenant house
#' 360 LESMILL is eva's place youth shelter
#' 25 CANTERBURY PLACE is eva's satellite
#' 422 GILBERT AVE is horizons for youth
#' 1076 PAPE AVE is kennedy house youth services
#' 95 WELLESLEY ST E is turning point emergency shelter
#' 7 VANAULEY ST is the ymca house
#' 6 WARRENDALE CRT is yourh without shelter
#'
#' @param data a data.frame with the following three variables: 
#' ADDRESS_LINE_1_DESCR, ADDRESS_LINE_2_DESCR. CITY_NM These variables are found in 
#' the PATIENT_DIM table
#'
#' @return tbl with with encounter number and a 
#' binary indicator variable (1 == shelter/homeless, 0 = no)
#' @export
#'

get_housing_indicator <- function(data) {
  shelter_list <- c("RED DOOR SHELTER", "HOMELESS SHELTER", "HOPE SHELTER",
                    "YOUTH SHELTER-702 KENNEDY", "525 BLOOR ST SHELTER",
                    "NO FIXED ADDRESS", "1-508 PARLIMENT NFA",
                    "147 GERRARD ST E - NFA", "NFA - PLS VERIFY",
                    "NFA-REGENT PK COMM HEALTH", "NFA CAMH", "NFA NOT 405 WESTMOUNT",
                    "NFA - 339 GEORGE ST", "NFA- COVENANT HOUSE", "NFA          .",
                    "NFA -CTCH", "NFA-467 JARVIS", "NFA-GATEWAY", "NFA - 339 GEORGE STREET",
                    "NFA-339 GEORGE STREET", "NFA", "NFA- 107 JARVIS ST",
                    "NFA-BED 3", "43 CHRISTIE ST-REFU.CENTR",
                    "43 CHRISTIE ST", "43 CHRISTIE STREET",
                    "SEATON HOUSE", "339 SEATON HOUSE", "KENNEDY HOUSE",
                    "SOUJOURN HOUSE", "BAILEY HOUSE", "SOJOURN HOUSE",
                    "CASEY HOUSE", "CASEY HOUSE HOSPICE", "STRONG HOPE HOUSE",
                    "COVENANT HOUSE", "FREDERICK HOUSE", "ROBERTSON HOUSE",
                    "JOHN GIBSON HOUSE", "295 SEATON HOUSE",
                    "339 GEORGE STREET 226", "2ND FLR-339 GEORGE ST",
                    "339 GEORGE ST.", "339 GEORGE STREET", "339 GEORGE ST",
                    "4TH FLOOR-339 GEORGE ST","HOSTEL INTERNATIONAL",
                    "191 SPADINA ROAD", "129 PETER ST", "129 PETER ST.",
                    "129 PETER STREET", "25-129 PETER ST", "3-129 PETER ST",
                    "100 LIPPINCOTTST", "2714 DANFORTH AVE",
                    "2714 DANFORTH AVENUE", "1161 CALEDONIA ST",
                    "1161 CALEDONIA ROAD", "1161 CALEDONIA RD",
                    "3576 ST. CLAIR AVE E", "3576 ST CLAIR AVENUE EAST",
                    "3576 ST CLAIR AVE E", "973 LANDSDOWNE AVE",
                    "302-973 LANDSDOWNE AVE",
                    "616 VAUGHAN RD",  "1-349 GEORGE ST",
                    "349 GEORGE ST", "3-349 GEORGE ST", "349 GEORGE STREET",
                    "3-349 GEORGE ST N", "14 VAUGHAN RD",
                    "1-14 VAUGHAN RD", "14 VAUGHAN ROAD", "18-14 VAUGHAN RD", "5-14 VAUGHAN RD",
                    "GATEWAY SALVATION ARMY", "107 JARVIS ST THE GATEWAY",
                    "107 JARVIS ST.", "107 JARVIS ST", "107 JARVIS STREET",
                    "36-107 JARVIS ST", "30-107 JARVIS ST", "2B-107 JARVIS ST",
                    "42-107 JARVIS ST", "70-107 JARVIS ST", "GATEWAY 107 JARVIS STREET",
                    "9-107 JARVIS ST", "12-525 BLOOR ST", "525 BLOOR STREET EAST",
                    "525 BLOOR ST EAST", "525 BLOOR ST", "525 BLOOR ST E",
                    "86 LOMBARD STREET", "86 LOMBARD ST", "86 LOMBARD",
                    "13-1322 BLOOR ST W", "1322 BLOOR ST W", "1322 BLOOR ST WEST",
                    "209-2808 DUNDAS ST WEST", "2808 DUNDAS ST W", "2808 DUNDAS ST WEST",
                    "2808 DUNDAS STREET W", "2808 DUNDAS ST. W", "13-723 QUEEN ST W",
                    "723 QUEEN ST WEST", "723 QUEEN ST W", "723 QUEEN STREET WEST",
                    "647 DUNDAS ST W", "647 DUNDAS STREET", "25 CANTERBURY PLACE",
                    "25 CANTERBURY PL", "25 CANTERBURY PLACE", "422 GILBERT AVE",
                    "422 GILBERT AVE","422 GILBERT AV", "1076 PAPE AVE",
                    "1076 PAPE AVENUE", "7 VANAULEY ST", "6 WARRENDALE CRT",
                    "145 QUEEN ST E", "26 CALLENDER ST",
                    "SEATON", "COMH",
                    "COTA", "291 GEORGE ST")
  
  # list of houses
  house <- unique(grep("HOUSE", data$ADDRESS_LINE_2_DESCR,value = T))
  shelter <- unique(grep("SHELTER", data$ADDRESS_LINE_2_DESCR, value = T))
  #remove those that are not shelters
  house <- house[!(house %in% c("FUDGER HOUSE", "THE BRITON HOUSE RET CTR",
                                "BRITON HOUSE", "FUDGER HOUSE LONG TERM C",
                                "FUDGER HOUSE NURSING HOME", "BELMONT HOUSE RETIREMENT",
                                "THE BRITON HOUSE", "BELMONT HOUSE RET HOE",
                                "BRITON HOUSE RETIREMENT", "BELMONT HOUSE", "BELMONT HOUSE LTC",
                                "FUDGERHOUSE", "BELMOUNT HOUSE", "EMILY'S HOUSE",
                                "TOWN HOUSE 22", "HOUSE", "FUDGER HOUSE (DECEASED)",
                                "FUDGER HOUSE, #303 WEST", "THE PENTHOUSE", 
                                "BELMONT HOUSE LNG TRM CAR", "BELMONT HOUSE LTC 5W",
                                "BELMONT RETIREMENT HOUSE", "BRACENDALE HOUSE",
                                "BRITAIN HOUSE RETIRMENT", "ALPHA HOUSE",
                                "ALPHA OMEGA HOUSE", "KEBELE:12 HOUSE#728",
                                "RD TOWN HOUSE 188", "UOFT GRADUATE HOUSE",
                                "MILLHOUSES", "HOUSE 18", "NORWOOD HOUSE LONG TERM",
                                "FUDGER HOUSE - 3WEST", "COACH HOUSE", 
                                "RD TOWN HOUSE 188"))]
  
  other_shelters <- "80 WOODLAWN AVE E|15 PAPE AVE|2 TREEWOOD ST|291 GEORGE ST|300 SHERBOURNE ST|252 SHERBOURNE ST|230 OAK ST|275 BLEECKER ST|61 PELHAM PARK GARD|101 ONTARIO ST|110 THE ESPLANADE|339 GEORGE STREET|261 JARVIS ST"
  
  data <- data %>% 
    mutate(housing_indicator = ifelse(ADDRESS_LINE_1_DESCR %in% shelter_list |
                                        ADDRESS_LINE_2_DESCR %in% shelter_list |
                                        ADDRESS_LINE_2_DESCR %in% house |
                                        ADDRESS_LINE_2_DESCR %in% shelter |
                                        grepl(other_shelters, ADDRESS_LINE_1_DESCR)|
                                        grepl(other_shelters, ADDRESS_LINE_2_DESCR)|
                                        grepl("101 ONTARIO ST", ADDRESS_LINE_1_DESCR)|
                                        grepl("100 LIPPINCOTT ST", ADDRESS_LINE_1_DESCR)|
                                        grepl("135 SHERBOURNE ST", ADDRESS_LINE_1_DESCR) |
                                        grepl("502 SPADINA", ADDRESS_LINE_1_DESCR) |
                                        grepl("70 GERRARD ST", ADDRESS_LINE_1_DESCR)|
                                        grepl("87 PEMBROKE ST", ADDRESS_LINE_1_DESCR)|
                                        grepl("80 WOODLAWN A", ADDRESS_LINE_1_DESCR) |
                                        grepl("-20 GERRARD ST", ADDRESS_LINE_1_DESCR) |
                                        grepl("^20 GERRARD ST", ADDRESS_LINE_1_DESCR)|
                                        grepl("360 LESMILL R", ADDRESS_LINE_1_DESCR) |
                                        grepl("^360 LESMILL$", ADDRESS_LINE_1_DESCR)|
                                        grepl("-95 WELLESLEY ST", ADDRESS_LINE_1_DESCR)|
                                        grepl("^95 WELLESLEY ST", ADDRESS_LINE_1_DESCR) |
                                        grepl("291 GEORGE ST", ADDRESS_LINE_1_DESCR)|
                                        grepl("107 JARVIS ST", ADDRESS_LINE_1_DESCR)|
                                        grepl("61 PELHAM PARK", ADDRESS_LINE_1_DESCR)|
                                        grepl("15 PAPE", ADDRESS_LINE_1_DESCR)|
                                        grepl("80 WOODLAWN AVE", ADDRESS_LINE_1_DESCR)|
                                        grepl("300 SHERBOURNE ST", ADDRESS_LINE_1_DESCR)|
                                        grepl("61 PELHAM PARK", ADDRESS_LINE_1_DESCR)|
                                        grepl("61 PELHAM PARK", ADDRESS_LINE_1_DESCR)|
                                        grepl("SHELTER", ADDRESS_LINE_2_DESCR) |
                                        grepl("NFA", CITY_NM) |
                                        grepl("NO ADDRESS", CITY_NM) |
                                        grepl("407-15 PAPE AVE ", ADDRESS_LINE_1_DESCR) |
                                        grepl("GATEWAY", ADDRESS_LINE_2_DESCR)|
                                        grepl("SALVATION ARMY", ADDRESS_LINE_2_DESCR) |
                                        grepl("SOJOURN HOUSE", ADDRESS_LINE_2_DESCR) |
                                        grepl("HARBOUR LIGHT MINISTRIES", ADDRESS_LINE_2_DESCR)|
                                        grepl("CASEY HOUSE", ADDRESS_LINE_2_DESCR)|
                                        grepl("ST. ANNES PLACE", ADDRESS_LINE_2_DESCR)|
                                        grepl("FRED VICTOR RM 521", ADDRESS_LINE_2_DESCR)|
                                        grepl("KENNEDY RESIDENCE", ADDRESS_LINE_2_DESCR)|
                                        grepl("GOOD SHEPPHERD", ADDRESS_LINE_2_DESCR)|
                                        grepl("ROOMING HOUSE", ADDRESS_LINE_2_DESCR) |
                                        grepl("SEATON HOUSE HOSTEL", ADDRESS_LINE_2_DESCR) |
                                        grepl("HABOUR LIGHT MINISTRIES", ADDRESS_LINE_2_DESCR)|
                                        grepl("MINISTRIES", ADDRESS_LINE_2_DESCR)|
                                        grepl("SHELTER", ADDRESS_LINE_2_DESCR)|
                                        grepl("SEATON", ADDRESS_LINE_2_DESCR) |
                                        grepl("SEATON", ADDRESS_LINE_1_DESCR) |
                                        grepl("WOMEN'S RESIDENCE", ADDRESS_LINE_2_DESCR)|
                                        grepl("WOMEN'S RESIDENCE", ADDRESS_LINE_2_DESCR)|
                                        grepl("WOMEN'S RESIDENCE", ADDRESS_LINE_2_DESCR),1,0))
  
  return(data$housing_indicator)
  
}
###############################################################################


###############################################################################
clean_demographics <- function(data) {
  
  # has family doc 
  no_doc <- c('NO FAM DR', 'INACTIVE   *NONE')
  
  data$has_family_doc <- ifelse(data$FAMILY_DOCTOR_NM %in% c('    NO FAM DR', '    UNKNOWN',
                                                             'CLINIC   WALK-IN', 'INFORMATION   INSUFFICIENT') |
                                  grepl('INACTIVE', data$FAMILY_DOCTOR_NM), 0, 1)
  
  
  # gender 
  gender <- data$GENDER_CD
  gender[gender == 'n/a'] <-'OTHER'
  gender[gender == 'U'] <- 'OTHER'
  gender[gender == 'F'] <- 'FEMALE'
  gender[gender == 'M'] <- 'MALE'
  gender <- ifelse(gender == 'MALE', 1, 0)
  
  data$GENDER_CD <- gender
  
  # marital status
  marital <- data$MARITAL_STATUS_CD
  marital[marital == 'S'] <- 'marital_single'
  marital[marital == 'M' | marital == 'C' | marital == 'P'] <- 'marital_parterned'
  
  marital[marital == 'D' | marital == 'W'] <- 'marital_divorced_widowed'
  marital[marital == ' ' | marital == 'n/a' | 
            marital == 'U' | marital == 'X'] <- 'marital_unknown_other'
  
  data <- data %>% 
    mutate(marital = marital)
  
  # language 
  
  language <- data$LANGUAGE_DESCR
  language[language == 'CHINESE (CANTONESE, MANDARIN)'] <- 'language_chinese'
  language[language == 'ENGLISH'] <- 'language_english'
  language[language == '0' | language == 'UNDETERMINED (OTHER, UNKNOWN)'] <- 'language_unknown'
  language[!(language %in% c('language_english', 'language_chinese', 'language_unknown'))] <- 'language_other'
  
  data <- data %>% 
    mutate(language = language)
  
  # religion
  religion <- data$RELIGION_CD
  religion[religion == 'NOR'] <- 'religion_nor'
  religion[religion == 'RC'] <- 'religion_rc'
  religion[religion == 'CHI'] <- 'religion_chi'
  religion[religion == 'PRT'] <- 'religion_prt'
  religion[religion == 'ANG'] <- 'religion_ang'
  religion[religion == 'MUS'] <- 'religion_mus'
  religion[religion == 'REF'] <- 'religion_ref'
  religion[religion == 'JEW'] <- 'religion_jew'
  religion[religion == 'HIN'] <- 'religion_hin'
  religion[religion == 'GO'] <- 'religion_go'
  
  religion <- ifelse(religion %in% c('religion_nor', 'religion_rc', 
                                     'religion_chi', 'religion_prt', 'religion_ang', 'religion_mus',
                                     'religion_ref', 'religion_jew', 'religion_hin', 'religion_go'),
                     religion, 'religion_other')
  
  data <- data %>% 
    mutate(religion = religion) %>% 
    select(PATIENT_DK, POSTAL_CD, ADDRESS_LINE_1_DESCR, ADDRESS_LINE_2_DESCR,
           CITY_NM, RESIDENCE_DESCR, 
           STATE_PROVINCE_CD, POSTAL_CD, marital,
           language, religion) %>% 
    mutate(POSTAL_CD = gsub(" ", "", POSTAL_CD))
  
  return(data)
}
###############################################################################
