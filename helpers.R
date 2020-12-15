get_adt <- function (edw_con, location_or_service = "location") 
{
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

extract_soarian_features <- function(con, census) {
  
  
  # Soarian query
  soarian_qry <- "select ha.OBJECTID as ass_ObjectID, ha.AssessmentID, ha.PatientVisit_oid,
		ha.EnteredDT, ha.Patient_oid, ha.OrdersAsWrittenText,
		isnull(ha.FormUsageDisplayName,'') as FormUsageDisplayName,
	    isnull(ha.FormUsage,'') as FormUsage, -- add for index use
        ho.ObjectID, ho.Assessment_oid, ha.AssessmentStatus,
        ho.CreationTime, ha.CollectedDT, ho.FindingAbbr,
		ho.FindingDataType, ho.FindingName, ho.ObservationStatus,
		ho.Value, ho.MinValue, ho.MaxValue, ho.BaseUnitValue, ho.EndDT
  from HObservation_Active ho
  join HAssessment ha
	on ho.AssessmentID = ha.AssessmentID
		and ho.EndDT is NULL
		and ha.EndDT is NULL
		and AssessmentStatus <> 'Erroneous'
		and ho.Value <> ''
		and ha.FormUsage is not NULL
  where ha.Patient_oid = ?  and ha.PatientVisit_oid = ?"
  
  # Store patient_oid and PatientVisit_oid
  visits_df <- unique(data.frame(census$Patient_oid, census$PatientVisit_oid))
  
  # Patient_oid
  soarian_obs <- RODBCext::sqlExecute(con,
                                      soarian_qry,
                                      visits_df,
                                      fetch = T, na.strings = "")
  
  chartwatch_soarian_obs <- soarian_obs %>%
    dplyr::inner_join(census, by = c("PatientVisit_oid" = "PatientVisit_oid",
                                     "Patient_oid" = "Patient_oid")) %>%
    dplyr::select(ass_ObjectID,
                  ASSESSMENTID = AssessmentID,
                  PATIENTOID =  Patient_oid,
                  PATIENTVISIT_OID = PatientVisit_oid,
                  ASSESSMENTSTATUS = AssessmentStatus,
                  ENTEREDDT = EnteredDT,
                  FORMUSAGEDISPLAYNAME = FormUsageDisplayName,
                  ORDERSASWRITTENTEXT = OrdersAsWrittenText,
                  FORMUSAGE = FormUsage,
                  OBJECTID = ObjectID,
                  ASSESSMENT_OID = Assessment_oid,
                  OBS_CREATIONTIME = CollectedDT,
                  FINDINGABBR = FindingAbbr,
                  FINDINGDATATYPE = FindingDataType,
                  FINDINGNAME = FindingName,
                  OBSERVATIONSTATUS = ObservationStatus,
                  BASEUNITVALUE = BaseUnitValue,
                  VALUE = Value,
                  MINVALUE = MinValue,
                  MAXVALUE = MaxValue,
                  ENDDT_OBS = EndDT,
                  ENCOUNTER_NUM = PatientAccountID)
  
  return(chartwatch_soarian_obs)
}

extract_soarian_labs <- function(con, census) {
  
  # soarian query
  soarian_qry <-  "SELECT
      PatientVisit_oid,
      Patient_oid,
      ServiceProviderOrderID,
      InvalidationDateTime,
      FindingAbbreviation,
      ResultValue,
      HInvestigationResultSuppInfo.ObjectID,
      FindingName,
		  HinvestigationResult.CreationTime,
		  ResultDateTime,
      ObservationDateTime,
      AbnormalFlag,
      ReferenceRange,
      ResultSuppInfo_oid,
      Comments
  FROM HinvestigationResult JOIN HInvestigationResultSuppInfo
	ON HInvestigationResultSuppInfo.ObjectID = HinvestigationResult.ResultSuppInfo_oid
        AND HinvestigationResult.IsDeleted = 0
        AND HInvestigationResultSuppInfo.IsDeleted = 0
        AND ResultStatus in ('F', 'C')
        AND ResultDataType in ('AN', 'NM')
        AND HInvestigationResultSuppInfo.OriginID = 'LAB'
  WHERE Patient_oid = ? AND PatientVisit_oid = ?"
  
  
  # Store patient_oid and PatientVisit_oid
  visits_df <- unique(data.frame(census$Patient_oid, census$PatientVisit_oid))
  
  # Connect to table HInvestigationResult and HInvestigationResultSuppInfo to get lab results
  lab_results <- RODBCext::sqlExecute(
    con,
    soarian_qry,
    visits_df,
    fetch = T, na.strings = ""
  )
  
  # Transform data results to numeric
  lab_results <- lab_results %>%
    dplyr::filter(!is.na(CreationTime)) %>%
    dplyr::filter(InvalidationDateTime >  Sys.Date()) %>%
    dplyr::filter(!grepl("wrong patient", Comments, ignore.case = T)) %>%
    dplyr::mutate(ResultValue = gsub('>', '', ResultValue)) %>%
    dplyr::mutate(ResultValue = gsub('<', '', ResultValue)) %>%
    # "cancelled" in ResultValue will be transferred to NAs
    dplyr::mutate(numeric_value = as.numeric(ResultValue)) %>%
    dplyr::filter(!is.na(numeric_value)) %>%
    dplyr::select(-ResultValue)
  
  
  chartwatch_soarian_labs <- lab_results %>%
    dplyr::inner_join(census, by = c("PatientVisit_oid" = "PatientVisit_oid",
                                     "Patient_oid" = "Patient_oid")) %>%
    dplyr::select(ENCOUNTER_NUM = PatientAccountID,
                  PATIENTVISIT_OID = PatientVisit_oid,
                  SERVICEPROVIDERORDERID = ServiceProviderOrderID,
                  INVALIDATIONDATETIME = InvalidationDateTime,
                  FINDINGABBREVIATION = FindingAbbreviation,
                  CREATIONTIME = CreationTime,
                  RESULTDATETIME = ResultDateTime,
                  OBSERATIONDATETIME = ObservationDateTime,
                  ABNORMALFLAG = AbnormalFlag,
                  REFERENCERANGE = ReferenceRange,
                  FINDINGNAME = FindingName,
                  NUMERIC_VALUE = numeric_value
    )
  
  return(chartwatch_soarian_labs)
}


extract_census <- function(con) {
  
  # Get current patients query
  census_qry <- "SELECT
  	HPatientVisit.Patient_oid,
  	HPatientVisit.ObjectID as PatientVisit_oid,
  	HStaff.StaffSignature,
  	HPatientVisit.PatientAccountID,
  	HPatientVisit.UnitContactedName,
  	isnull(HBedTransfer.FromBedName,'') as FromBedName,
  	isnull(HFrom.Description,'') as FromBed,
  	isnull(HFrom.HealthcareUnitName,'') as FromSvc,
  	HBedTransfer.FromDate,
  	isnull(HBedTransfer.ToBedName,'') as ToBedName,
  	isnull(HTO.Description,'') as ToBed,
  	isnull(HTO.HealthcareUnitName,'') as ToSvc,
  	HBedTransfer.ToDate,
  	HPerson.MaritalStatus,
  	HPatient.PrimaryLanguage,
  	HPatientVisit.VisitStartDateTime,
  	HPerson.BirthDate,
  	HPerson.Sex,
  	HPatient.StreetAddress,
  	HPatient.City,
  	isnull(HPatientVisit.IsolationIndicator,'') as IsolationIndicator,
  	HPatient.FirstName,
  	HPatient.LastName,
  	HPatientVisit.LatestBedName,
  	HPatientVisit.PatientLocationName,
  	HPatientIdentifiers.Value as MRN
  FROM
  	HPatientVisit
  	JOIN HPatient
  	    ON HPatientVisit.Patient_oid = HPatient.ObjectID
  	        AND (HPatientVisit.Entity_oid = 3)  -- St. Michael's site and index use
  		    AND (HPatientVisit.VisitTypeCode in ('IP', 'EOP')) -- inpatients/eop only
  		    AND (HPatientVisit.IsDeleted = 0) -- visit is not deleted/invalid
  		    AND (HPatient.Isdeleted = 0) -- right version of patient due to merges
  	JOIN HPerson
  	    ON HPatientVisit.Patient_oid = HPerson.ObjectID
  			AND (Hperson.isdeleted = 0) -- right version of person info
  	JOIN HPatientIdentifiers
  	    ON HPatientIdentifiers.Patient_oid = HPatient.ObjectID
  			AND (HPatientIdentifiers.TYpe = 'MR') -- only MRNs and no other identifiers
  			AND  (Hpatientidentifiers.isdeleted = 0) -- right version of identifier
  	LEFT OUTER JOIN HStaffAssociations
  	    ON HstaffAssociations.PatientVisit_oid =   HPatientVisit.ObjectID
  			AND HStaffAssociations.RelationType= 0 -- Attending dr
  	LEFT OUTER JOIN HBedTransfer
  	    ON HPatientVisit.ObjectID = HBedTransfer.PatientVisit_oid
  			AND (isnull(HBedTransfer.FromLocation_oid,'') <> isnull(HBedTransfer.ToLocation_oid,''))
      LEFT OUTER JOIN HHealthCareUnit HTO
          ON HBedTransfer.ToLocation_oid = HTO.objectID
      LEFT OUTER JOIN HStaff
          ON HStaffAssociations.Staff_oid = HStaff.ObjectID
  	LEFT OUTER JOIN HHealthCareUnit HFrom
          ON HBedTransfer.FromLocation_oid = HFrom.objectID"

  
  census <- DBI::dbGetQuery(con,census_qry)
  
  # List of GIM services
  gim_services <- c("14C")
  
  # List of ICU services
  icu_services <-c("MSIC", "TNIC", "CCU", "NICU", "CICU", "5DCI", "5DNI")
  
  # Create gim and icu indicators for each row
  census <- census %>%
    dplyr::mutate(gim = ifelse(ToSvc %in% gim_services, 1, 0)) %>%
    dplyr::mutate(icu = ifelse(ToSvc %in% icu_services, 1, 0))
  
  # Most recent transfer to GIM as GIM_START_TS
  gim_start <- census %>%
    dplyr::filter(gim == 1) %>%
    dplyr::group_by(PatientVisit_oid) %>%
    dplyr::arrange(dplyr::desc(ToDate)) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::mutate(GIM_START_TS = ToDate) %>%
    dplyr::ungroup() %>%
    dplyr::select(PatientVisit_oid, GIM_START_TS)
  
  # In the case of multiple transfers to GIM, get GIM_FIRST_TS
  gim_first_trasnfer <- census %>%
    dplyr::filter(gim == 1) %>%
    dplyr::group_by(PatientVisit_oid) %>%
    dplyr::arrange(ToDate) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::mutate(GIM_FIRST_TS = ToDate) %>%
    dplyr::ungroup() %>%
    dplyr::select(PatientVisit_oid, GIM_FIRST_TS)
  
  # Get first ICU transfer and most recent ICU transfer
  gim_icu <- census %>%
    dplyr::filter(icu == 1) %>%
    dplyr::group_by(PatientVisit_oid) %>%
    dplyr::summarize(ICU_TS = min(ToDate),
                     MOST_RECENT_ICU_TS = max(ToDate)) %>%
    dplyr::ungroup() %>%
    dplyr::select(PatientVisit_oid, ICU_TS, MOST_RECENT_ICU_TS)
  
  
  # Merge and filter so that only one row per visit
  data <- census %>%
    dplyr::left_join(gim_start, by = "PatientVisit_oid") %>%
    dplyr::left_join(gim_first_trasnfer, by = "PatientVisit_oid") %>%
    dplyr::left_join(gim_icu, by = "PatientVisit_oid") %>%
    dplyr::select(-dplyr::starts_with("From"), -dplyr::starts_with("To"), -gim, -icu) %>%
    unique()
  
  gim_census <- data %>%
    dplyr::select(Patient_oid,
                  PatientVisit_oid,
                  PatientAccountID,
                  VisitStartDateTime,
                  PatientLocationName,
                  UnitContactedName,
                  MRN,
                  StreetAddress,
                  City,
                  Sex,
                  BirthDate,
                  FirstName,
                  LastName,
                  IsolationIndicator,
                  StaffSignature,
                  LatestBedName,
                  GIM_START_TS,
                  GIM_FIRST_TS,
                  ICU_TS,
                  MOST_RECENT_ICU_TS) %>%
    dplyr::mutate(
      pre_gim_icu = ifelse(ICU_TS < GIM_START_TS, 1, 0),
      age = as.numeric(difftime(VisitStartDateTime, BirthDate, units = "days")) / 365.25,
      Sex = if_else(Sex == 1, "F", "M")
    )
  
  return(gim_census)
}


extract_soarian_feat <- function (con, census) 
{
  soarian_qry <- "select ha.OBJECTID as ass_ObjectID, ha.AssessmentID, ha.PatientVisit_oid,\n\t\tha.EnteredDT, ha.Patient_oid, ha.OrdersAsWrittenText,\n\t\tisnull(ha.FormUsageDisplayName,'') as FormUsageDisplayName,\n\t    isnull(ha.FormUsage,'') as FormUsage, -- add for index use\n        ho.ObjectID, ho.Assessment_oid, ha.AssessmentStatus,\n        ho.CreationTime, ha.CollectedDT, ho.FindingAbbr,\n\t\tho.FindingDataType, ho.FindingName, ho.ObservationStatus,\n\t\tho.Value, ho.MinValue, ho.MaxValue, ho.BaseUnitValue, ho.EndDT\n  from HObservation_Active ho\n  join HAssessment ha\n\ton ho.AssessmentID = ha.AssessmentID\n\t\tand ho.EndDT is NULL\n\t\tand ha.EndDT is NULL\n\t\tand AssessmentStatus <> 'Erroneous'\n\t\tand ho.Value <> ''\n\t\tand ha.FormUsage is not NULL\n  where ha.Patient_oid = ?  and ha.PatientVisit_oid = ?"
  visits_df <- unique(data.frame(census$Patient_oid, census$PatientVisit_oid))
  
  #
  
  soarian_obs <- RODBCext::sqlExecute(con, soarian_qry, visits_df, 
                                      fetch = T, na.strings = "")
  
  chartwatch_soarian_obs <- soarian_obs %>% dplyr::inner_join(census, 
                                                              by = c(PatientVisit_oid = "PatientVisit_oid", Patient_oid = "Patient_oid")) %>% 
    dplyr::select(ass_ObjectID, ASSESSMENTID = AssessmentID, 
                  PATIENTOID = Patient_oid, PATIENTVISIT_OID = PatientVisit_oid, 
                  ASSESSMENTSTATUS = AssessmentStatus, ENTEREDDT = EnteredDT, 
                  FORMUSAGEDISPLAYNAME = FormUsageDisplayName, ORDERSASWRITTENTEXT = OrdersAsWrittenText, 
                  FORMUSAGE = FormUsage, OBJECTID = ObjectID, ASSESSMENT_OID = Assessment_oid, 
                  OBS_CREATIONTIME = CollectedDT, FINDINGABBR = FindingAbbr, 
                  FINDINGDATATYPE = FindingDataType, FINDINGNAME = FindingName, 
                  OBSERVATIONSTATUS = ObservationStatus, BASEUNITVALUE = BaseUnitValue, 
                  VALUE = Value, MINVALUE = MinValue, MAXVALUE = MaxValue, 
                  ENDDT_OBS = EndDT, ENCOUNTER_NUM = PatientAccountID)
  return(chartwatch_soarian_obs)
}


create_ts <- function (data, time = as.character(Sys.time())) {
  time_series <- list()
  for (i in 1:nrow(data)) {
    tmp <- data %>% dplyr::slice(i)
    
    start <- lubridate::ceiling_date(lubridate::ymd_hms(as.character(tmp$start_ts)), 
                                     unit = "hour")
    
    end <- lubridate::ceiling_date(lubridate::ymd_hms(time), 
                                   unit = "hour")
    
    tte <- as.numeric(difftime(end, start, units = "day"))
    
    if(tte > 30) {
      start <- end - ddays(30)
    }
    
    
    timestamps <- seq(start, end, by = "hour")
    time_elapsed <- as.numeric(difftime(timestamps, start, 
                                        units = "hours"))
    encounter_timeseries <- dplyr::tibble(ENCOUNTER_NUM = tmp$ENCOUNTER_NUM, 
                                          timestamp = timestamps, time_elapsed = time_elapsed)
    n <- nrow(encounter_timeseries)
    time_series[[i]] <- encounter_timeseries
  }
  encounter_ts <- data.table::rbindlist(time_series) %>% tibble::as_tibble()
  encounter_ts <- encounter_ts %>% dplyr::group_by(ENCOUNTER_NUM) %>% 
    dplyr::mutate(timestamp_6hr = cumsum((time_elapsed + 
                                            1)%%6 == 1)) %>% dplyr::ungroup()
  return(encounter_ts)
}


process_vital <- function (vitals_data) 
{
  blood_pressure <- chartwatch::clean_bp_measures(vitals_data)
  vitals_data <- vitals_data %>% filter(!(FINDINGABBR %in% 
                                            "S_BP")) %>% mutate(numeric_value = as.numeric(VALUE)) %>% 
    filter(!is.na(numeric_value)) %>% bind_rows(blood_pressure) %>% 
    filter(!is.na(numeric_value)) %>% mutate(numeric_value = ifelse(BASEUNITVALUE %in% 
                                                                      c(0, -1), numeric_value, BASEUNITVALUE))
  names(vitals_data) <- toupper(names(vitals_data))
  vitals_data <- vitals_data %>% rename(numeric_value = NUMERIC_VALUE) %>% 
    dplyr::select(-PATIENTVISIT_OID, -OBSERVATIONSTATUS, 
                  -VALUE, -MINVALUE, -MAXVALUE, -FINDINGDATATYPE, -ASSESSMENTSTATUS, 
                  -ENTEREDDT, -ORDERSASWRITTENTEXT, -OBJECTID, -contains("ASS_OBJECTID"), 
                  -ASSESSMENT_OID)
  vitals_soarian <- vitals_data %>% mutate(FINDINGNAME = ifelse(is.na(FINDINGNAME), 
                                                                "sbp", FINDINGNAME)) %>% mutate(numeric_value = ifelse(FINDINGNAME == 
                                                                                                                         "Temperature", numeric_value - 273.15, numeric_value)) %>% 
    mutate(numeric_value = ifelse(FINDINGNAME == "Temperature (c)" & 
                                    numeric_value > 180, numeric_value - 273.15, numeric_value))
  vitals_soarian <- vitals_soarian %>% rename(variable = FINDINGABBR) %>% 
    mutate(variable = clean_var_name(variable))
  vital_signs <- vitals_soarian %>% filter(variable %in% c("SBPSYSTOLIC", 
                                                           "SBPDIASTOLIC", "SPulse", "SRespirations", 
                                                           "SO2Saturation", "STemperature", "SPainIntRest", 
                                                           "SPainIntMove", "SFIO2"))
  vital_signs <- vital_signs %>% select(ENCOUNTER_NUM, variable, 
                                        timestamp = OBS_CREATIONTIME, numeric_value) %>% mutate(variable = paste0("vital_", 
                                                                                                                  tolower(variable))) %>% arrange(ENCOUNTER_NUM, timestamp)
  shift_assessment <- vitals_soarian %>% filter(FORMUSAGE == 
                                                  "Initial Shift Assessment" | variable == "SRpFiO2b")
  shift_assessment <- shift_assessment %>% filter(variable %in% 
                                                    c("SPNIntstyRest1", "SPNInstyMov1", "SRpO2LMin", 
                                                      "SPNIntstyRest2", "SPNInstyMov2", "SCVHrtRate", 
                                                      "SRpFiO2b"))
  shift_assessment <- shift_assessment %>% select(ENCOUNTER_NUM, 
                                                  variable, timestamp = OBS_CREATIONTIME, numeric_value) %>% 
    mutate(variable = paste0("shift_assess_", tolower(variable))) %>% 
    arrange(ENCOUNTER_NUM, timestamp)
  in_out <- vitals_soarian %>% filter(FORMUSAGE == "Intake and Output")
  in_out <- in_out %>% filter(variable %in% c("SIVNormalSaline", 
                                              "AIVPB1", "ACatheter", "SOtherIntake", 
                                              "AOthOutput", "SIV23and13", "ATmsIncontinent"))
  in_out <- in_out %>% select(ENCOUNTER_NUM, variable, timestamp = OBS_CREATIONTIME, 
                              numeric_value) %>% mutate(variable = paste0("in_out_", 
                                                                          tolower(variable))) %>% arrange(ENCOUNTER_NUM, timestamp)
  skin <- vitals_soarian %>% filter(FORMUSAGE == "Skin Assessment" | 
                                      (FORMUSAGE == "ADMISSION" & variable == "ABradenScore"))
  skin <- skin %>% select(ENCOUNTER_NUM, variable, timestamp = OBS_CREATIONTIME, 
                          numeric_value) %>% mutate(variable = paste0("skin_", 
                                                                      tolower(variable))) %>% arrange(ENCOUNTER_NUM, timestamp)
  db <- vitals_soarian %>% filter(FORMUSAGE == "Diabetic Protocol" | 
                                    (FORMUSAGE == "ADMISSION" & variable == "Diabetic Protocol"))
  db <- db %>% select(ENCOUNTER_NUM, variable, timestamp = OBS_CREATIONTIME, 
                      numeric_value) %>% mutate(variable = paste0("diabetic_", 
                                                                  tolower(variable))) %>% arrange(ENCOUNTER_NUM, timestamp)
  alcohol <- vitals_soarian %>% filter(FORMUSAGE == "CIWA Assessment", 
                                       variable == "SCIWAScore")
  alcohol <- alcohol %>% select(ENCOUNTER_NUM, variable, timestamp = OBS_CREATIONTIME, 
                                numeric_value) %>% mutate(variable = paste0("alcohol_", 
                                                                            tolower(variable))) %>% arrange(ENCOUNTER_NUM, timestamp)
  vitals_processed <- vital_signs %>% bind_rows(shift_assessment) %>% 
    bind_rows(in_out) %>% bind_rows(skin) %>% bind_rows(db) %>% 
    bind_rows(alcohol)
  vitals_hourly <- vitals_processed %>% mutate(timestamp = lubridate::ceiling_date(timestamp, 
                                                                                   unit = "hour")) %>% group_by(ENCOUNTER_NUM, variable, 
                                                                                                                timestamp) %>% summarize(numeric_value = mean(numeric_value, 
                                                                                                                                                              na.rm = TRUE)) %>% ungroup()
  vitals_soarian <- vitals_hourly %>% tidyr::pivot_wider(names_from = variable, 
                                                         values_from = numeric_value, values_fn = list(numeric_value = mean))
  vitals_soarian <- vitals_soarian %>% mutate(ENCOUNTER_NUM = as.integer(ENCOUNTER_NUM)) %>% 
    mutate(timestamp = lubridate::ymd_hms(as.character(timestamp)))
  return(vitals_soarian)
}

clean_bp <- function (data) 
{
  nm <- names(data)
  names(data) <- toupper(nm)
  bp <- data %>% filter(FINDINGABBR %in% "S_BP")
  bps <- strsplit(bp$VALUE, "/")
  sys <- sapply(bps, function(x) {
    if (length(x) == 0) {
      return(NA)
    }
    else {
      return(as.numeric(x[[1]]))
    }
  })
  dia <- sapply(bps, function(x) {
    if (length(x) < 2) {
      return(NA)
    }
    else {
      return(as.numeric(x[[2]]))
    }
  })
  diastolic <- bp %>% select(ENCOUNTER_NUM, OBS_CREATIONTIME) %>% 
    mutate(FINDINGABBR = "S_BP_DIASTOLIC", numeric_value = dia)
  systolic <- bp %>% select(ENCOUNTER_NUM, OBS_CREATIONTIME) %>% 
    mutate(FINDINGABBR = "S_BP_SYSTOLIC", numeric_value = sys)
  bp <- diastolic %>% bind_rows(systolic) %>% mutate(BASEUNITVALUE = -1) %>% 
    filter(!is.na(numeric_value))
  return(bp)
}


get_risk_groups <- function(data) {
  YELLOW_THRESHOLD <- 0.080
  RED_THRESHOLD <- 0.140
  risk_groups <- data %>%
    dplyr::mutate(risk_group = dplyr::case_when(score > RED_THRESHOLD ~ "High",
                                                score >= YELLOW_THRESHOLD & score <= RED_THRESHOLD ~ "Medium",
                                                TRUE ~ "Low"))
  return(risk_groups)
}

