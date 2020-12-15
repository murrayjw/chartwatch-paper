
extract_clinical_orders <- function(con_soarian, encounters, time) {
  
  soarian_orders <- tbl(con_soarian, in_schema("", 'horder')) %>% 
    filter(OrderTypeIdentifier == 'DG') %>% 
    select(Order_oid = ObjectID, 
           PatientVisit_oid, 
           OrderName, 
           OrderTypeAbbr, 
           OrderAbbreviation,
           EnteredDateTime,
           OrderDescAsWritten,
           Priority,
           Comment,
           StartDateTime,
           StopDateTime,
           OrderSubTypeAbbr,
           CommonDefName,
           CreationTime,
           OrderSuppInfo_oid,
           SrvTypeBLMnemonic)
  order_history <- tbl(con_soarian, in_schema('', 
                                              'hordertransitionrecord')) %>% 
    select(Order_oid, ToOrderStatusModifier, ChangedDateTime)
  
  soarian_orders <- soarian_orders %>% 
    inner_join(order_history, 
               by = 'Order_oid') 
  
  patient_visit <- tbl(con_soarian, in_schema("", "HPatientVisit")) %>% 
    select(PatientVisit_oid = ObjectID, 
           ENCOUNTER_NUM = PatientAccountID, 
           Patient_oid) %>% 
    filter(ENCOUNTER_NUM %in% encounters)
  
  supp_info <- tbl(con_soarian, in_schema("", "hordersuppinfo")) %>% 
    select(ObjectID ,
           ConsultRequestValues,
           ReasonForRequest,
           DietModifier1,
           DietModifier2,
           DietModifier3,
           OrderSourceAbbr,
           Condition)
  soarian_orders <- soarian_orders %>% 
    left_join(supp_info, by = c('OrderSuppInfo_oid' = 'ObjectID'))
  
  
  soarian_orders <- soarian_orders %>% 
    inner_join(patient_visit, 
               by = 'PatientVisit_oid')
  
  clinical_orders <- soarian_orders %>%  
    collect() %>% 
    select(-PatientVisit_oid, -Patient_oid, -OrderName,
           -OrderSuppInfo_oid)  %>% 
    select(ENCOUNTER_NUM, Order_oid, OrderSourceAbbr,
           OrderTypeAbbr,
           OrderSubTypeAbbr, 
           OrderAbbreviation, 
           CommonDefName, 
           Priority, 
           EnteredDateTime,
           CreationTime,
           ChangedDateTime,
           ToOrderStatusModifier,
           OrderDescAsWritten,
           Comment,
           Condition,
           ConsultRequestValues,
           ReasonForRequest,
           DietModifier1,
           DietModifier2, 
           DietModifier3)
  
  return(clinical_orders)
}


extract_labs <- function(con_soarian, encounter_list, time) {
  
  labs <- tbl(con_soarian, in_schema("", 'HinvestigationResult')) %>% 
    filter(IsDeleted == 0, ResultStatus == 'F',
           ResultDataType %in% c('AN', 'NM')) %>% 
    select(PatientVisit_oid, Patient_oid,
           ServiceProviderOrderID, InvalidationDateTime,
           FindingAbbreviation, ResultValue, 
           CreationTime, ResultDateTime,
           ObservationDateTime, 
           AbnormalFlag, ReferenceRange,
           ResultSuppInfo_oid)
  
  
  supp_info <- tbl(con_soarian, in_schema('', 'HInvestigationResultSuppInfo_Active')) %>% 
    select(ObjectID, FindingName)
  
  
  patvisit <- tbl(con_soarian, in_schema("", "HPatientVisit")) %>% 
    filter(alternatevisitid == '',
           PatientAccountID %in% encounter_list) %>% 
    select(PatientVisit_oid = ObjectID, PatientAccountID,
           Patient_oid) 
  
  
  labs <- labs %>% inner_join(patvisit, 
                              by = c('PatientVisit_oid' = 'PatientVisit_oid',
                                     'Patient_oid' = 'Patient_oid')) %>% 
    left_join(supp_info, by = c('ResultSuppInfo_oid' = 'ObjectID')) %>% 
    collect() %>% 
    rename(ENCOUNTER_NUM = PatientAccountID)
  
  labs <- labs %>% 
    filter(!is.na(CreationTime)) %>% 
    mutate(ResultValue = gsub('>', '', ResultValue)) %>% 
    mutate(ResultValue = gsub('<', '', ResultValue)) %>% 
    mutate(numeric_value = as.numeric(ResultValue)) %>% 
    filter(!is.na(numeric_value)) %>% 
    select(-ResultValue) %>% 
    filter(year(InvalidationDateTime) >  2050 ) %>% 
    mutate(d = as.numeric(difftime(CreationTime, ResultDateTime, units = 'hours'))) %>% 
    filter(d < 1 & d > -1) %>% 
    filter(ResultDateTime < time) %>% 
    select(-d)
  
  return(labs)
}


extract_soarian_obs <- function(con_soarian, encounter_list){
  
  patient_visit <- tbl(con_soarian, in_schema("", "HPatientVisit")) %>% 
    select(PatientVisit_oid = ObjectID, 
           ENCOUNTER_NUM = PatientAccountID) %>% 
    filter(ENCOUNTER_NUM %in% encounter_list)
  
  assessments <- tbl(con_soarian, in_schema("", "HAssessment"))  %>% 
    select(ass_ObjectID = ObjectID,
           AssessmentID, 
           PatientVisit_oid,
           Patient_oid,
           AssessmentStatus,
           EnteredDT, 
           FormUsageDisplayName, 
           OrdersAsWrittenText,
           FormUsage,
           ENDDT_ASS = EndDT) 
  
  
  assessments <- assessments %>% 
    inner_join(patient_visit,
               by = c("PatientVisit_oid" = "PatientVisit_oid"))
  
  
  
  observations <- tbl(con_soarian, in_schema("","HObservation")) %>% 
    select(ObjectID,
           Patient_oid,
           Assessment_oid, 
           AssessmentID,
           OBS_CREATIONTIME = CreationTime,
           Patient_oid, 
           FindingAbbr,
           FindingDataType,
           FindingName,
           ObservationStatus,
           Value,  
           MinValue,
           MaxValue, 
           BaseUnitValue,
           ENDDT_OBS = EndDT)
  
  soarian_data <- assessments %>% 
    inner_join(observations,
               by = c("Patient_oid" = "Patient_oid",
                      "AssessmentID" = 'AssessmentID')) 
  
  soarian_data <- soarian_data %>% 
    collect()
  
  soarian_data <- soarian_data %>% 
    mutate(erroneous = AssessmentStatus == 'Erroneous') %>% 
    group_by(AssessmentID) %>% 
    mutate(erroneous = max(erroneous)) %>% 
    filter(erroneous == 0) %>% 
    ungroup() %>% 
    filter(is.na(ENDDT_OBS)) %>% 
    filter(is.na(ENDDT_ASS)) %>% 
    filter(substr(ObservationStatus, 1, 1) == 'A')
  
  return(soarian_data)                  
}




clean_bp_measures <- function(data) {
  
  bp <- data %>% filter(FindingAbbr %in% 'S_BP')
  bps <- strsplit(bp$Value, '/')
  
  sys <- sapply(bps, function(x) {
    if(length(x) == 0) {
      return(NA)
    } else {
      return(as.numeric(x[[1]]))
    }
  })
  
  dia <- sapply(bps, function(x) {
    if(length(x) < 2) {
      return(NA)
    } else {
      return(as.numeric(x[[2]]))
    }
  })
  
  diastolic <- bp %>% select(ENCOUNTER_NUM, OBS_CREATIONTIME) %>% 
    mutate(FindingAbbr = 'S_BP_DIASTOLIC',
           numeric_value = dia)
  systolic <- bp %>% select(ENCOUNTER_NUM, OBS_CREATIONTIME) %>% 
    mutate(FindingAbbr = 'S_BP_SYSTOLIC',
           numeric_value = sys)
  
  bp <- diastolic %>%
    bind_rows(systolic) %>% 
    mutate(BaseUnitValue = -1) %>% 
    filter(!is.na(numeric_value))
  
  return(bp)
}


get_patient_data <- function(data, con_soarian, time) {
  

  
  current_patients_enc <- data %>% 
    distinct(ENCOUNTER_NUM)
  
  encounters <- paste0("00", current_patients_enc$ENCOUNTER_NUM)
  
  
  # clinical orders ---------------------------------------------------------
  tic <- Sys.time()
  orders <- extract_clinical_orders(con_soarian, encounters = encounters, time = time)
  toc <- Sys.time()
  print(toc - tic)
  
  # lab results
  tic <- Sys.time()
  labs <- extract_labs(con_soarian, encounter_list = encounters, time = time)
  toc <- Sys.time()
  print(toc - tic)
  
  # soarian observations
  tic <- Sys.time()
  soarian_obs <- extract_soarian_obs(con_soarian, encounter_list = encounters)
  toc <- Sys.time()
  print(toc - tic)
  
  numeric_soarian <- soarian_obs %>% 
    filter(FindingDataType == 0)
  
  
  blood_pressure <- clean_bp_measures(numeric_soarian)
  numeric_soarian <- numeric_soarian %>% 
    filter(!(FindingAbbr %in% 'S_BP')) %>% 
    mutate(numeric_value = as.numeric(Value)) %>% 
    filter(!is.na(numeric_value)) %>% 
    bind_rows(blood_pressure) %>% 
    filter(!is.na(numeric_value)) %>% 
    mutate(numeric_value = ifelse(BaseUnitValue %in% c(0, -1), numeric_value, BaseUnitValue))
  
  
  nurse_notes <- soarian_obs %>% 
    filter(FormUsageDisplayName == "Interprofessional Note") %>% 
    filter(FindingName %in% c("Data",
                              "Focus",
                              "Discipline",
                              "Plan",
                              "Action",
                              "Response")) %>% 
    group_by(ass_ObjectID) %>% 
    summarize(ENCOUNTER_NUM = max(ENCOUNTER_NUM),
              timestamp = max(OBS_CREATIONTIME),
              note = paste(Value, collapse = "\r"))
  
  
  all_data <- list(patient_data = data,
                   clinical_orders = orders,
                   numeric_soarian = numeric_soarian,
                   labs = labs,
                   nurse_notes = nurse_notes)
  
  return(all_data)
}



get_all_gim_patients <- function(con) {
  adt <- get_patient_adt(con_edw)
  service <- get_location_timeseries(adt, location_or_service = "service")
  
  gim_services <- c("TMA GENERAL MEDICINE",
                    "TMB GENERAL MEDICINE",
                    "TMC GENERAL MEDICINE",
                    "TMD GENERAL MEDICINE",
                    "TME GENERAL MEDICINE")
  gim <- service %>% 
    filter(service %in% gim_services)
  
  return(gim)
}


create_patient_timeseries <- function(data, time) {
  time_series <- list()
  
  for(i in 1:nrow(data)) {
    
    tmp <- data %>% 
      dplyr::slice(i)
    
    start <- ceiling_date(tmp$start_time, unit = "hour")
    end <- ceiling_date(time, unit = "hour")
    timestamps <- seq(start, end, by = "hour")
    time_elapsed <- as.numeric(difftime(timestamps, start, units = "hours"))
    
    encounter_timeseries <- tibble(ENCOUNTER_NUM = tmp$ENCOUNTER_NUM,
                                   timestamp = timestamps,
                                   time_elapsed = time_elapsed)
    
    n <- nrow(encounter_timeseries)
    
    time_series[[i]] <- encounter_timeseries
    
    
  }
  
  encounter_ts <- data.table::rbindlist(time_series) %>% 
    as_tibble()
  
  
  # add grouping variable for 2, 4, and 8 hour windows ----------------------
  
  encounter_ts <- encounter_ts %>% 
    group_by(ENCOUNTER_NUM) %>% 
    mutate(timestamp_6hr = cumsum((time_elapsed + 1) %% 6 == 1)) %>% 
    ungroup()
  
  return(encounter_ts)
  
  
}



extract_baseline_data <- function(location_data, numeric_data) {
  
  load("numeric_descriptive_statistics.R")
  
  baseline <- numeric_data %>% 
    mutate(ENCOUNTER_NUM = stringr::str_sub(ENCOUNTER_NUM, 3, -1)) %>% 
    left_join(location_data, by = "ENCOUNTER_NUM") %>% 
    filter(timestamp < start_time)
  
  
  var_list <- c("alcohol_sciwascore", "diabetic_spocglucresult", "in_out_acatheter",
                "in_out_aivpb1", "in_out_aothoutput", "in_out_atmsincontinent", 
                "in_out_siv23and13", "in_out_sivnormalsaline", "in_out_sotherintake", 
                "lab_abaso", "lab_abe", "lab_acet", "lab_aeos", "lab_agap", "lab_ahion", 
                "lab_alact", "lab_alb", "lab_alp", "lab_alt", "lab_alymp",
                "lab_amono", "lab_amy", "lab_aneut", "lab_ao", "lab_apco2", 
                "lab_aph", "lab_apo2", "lab_asa", "lab_ast", "lab_atco2",
                "lab_b12", "lab_bc", "lab_bnps", "lab_ca", "lab_cacra", "lab_cai", 
                "lab_caicr", "lab_ck", "lab_cl", "lab_co2", "lab_cr", 
                "lab_crp", "lab_esr1", "lab_etoh", "lab_fe", "lab_fer",
                "lab_glob", "lab_glpoc", "lab_glur", "lab_hba1", "lab_hct", 
                "lab_hgb", "lab_ical", "lab_igab", "lab_ivsd", "lab_iwbcr", 
                "lab_k", "lab_la", "lab_ld", "lab_lip", "lab_lvedd", "lab_lvesd", 
                "lab_masa", "lab_mch", "lab_mchc", "lab_mcv", "lab_metaa", "lab_mg",
                "lab_mpv", "lab_mvsa", "lab_myela", "lab_na", "lab_orcai",
                "lab_orglu", "lab_orhc", "lab_ork", "lab_orna", "lab_osm", 
                "lab_ph", "lab_plt", "lab_po4", "lab_ptemp", "lab_pwd", 
                "lab_rbc", "lab_rdw", "lab_reta", "lab_rinr", "lab_rpt", 
                "lab_rptt", "lab_sat", "lab_spg", "lab_tbil", "lab_tibc", 
                "lab_tni", "lab_tpr", "lab_tsh", "lab_ucl", "lab_uk", "lab_una",
                "lab_uosm", "lab_urea", "lab_uuro", "lab_vbe", "lab_vhion", 
                "lab_vlact", "lab_vpco2", "lab_vph", "lab_vpo2", "lab_vtco2", 
                "shift_assess_scvhrtrate", "shift_assess_spninstymov1", 
                "shift_assess_spninstymov2", "shift_assess_spnintstyrest1",
                "shift_assess_spnintstyrest2", "shift_assess_srpfio2b", 
                "shift_assess_srpo2lmin", "skin_abradenscore", "vital_sbpdiastolic", 
                "vital_sbpsystolic", "vital_sfio2", "vital_so2saturation",
                "vital_spainintmove", "vital_spainintrest", "vital_spulse", 
                "vital_srespirations", "vital_stemperature")
  
  
  baseline <- baseline %>% 
    filter(variable %in% var_list)
  
  # trim data
  
  nms <- names(numeric_descriptive_statistics)
  
  for(i in nms) {
    
    tmp <- numeric_descriptive_statistics[[i]]
    
    
    baseline <- baseline %>% 
      mutate(numeric_value = ifelse( variable == i & numeric_value > tmp$q99, tmp$q99,
                                     ifelse(variable == i & numeric_value < tmp$q01, tmp$q01,
                                            numeric_value)))
    
  }
  
  
  # take the top non-missing variables (including all vital signs)
  # keep_baseline_vars <- names(sort(apply(raw_baseline, 2, function(x) sum(is.na(x)/length(x)))))[1:56]
  
  keep_baseline_vars <- c("ENCOUNTER_NUM", "lab_co2", "lab_cl", "lab_na", 
                          "lab_agap", "lab_hct", "lab_mch", "lab_mchc", 
                          "lab_rbc", "lab_iwbcr", "lab_mcv", "lab_rdw", 
                          "lab_hgb", "lab_cr", "lab_plt", "lab_glur", "lab_k",
                          "lab_mpv", "lab_alymp", "lab_aeos", "lab_abaso", 
                          "lab_amono", "lab_aneut", "lab_alb", "lab_rpt", "lab_rinr",
                          "lab_rptt", "lab_tbil", "lab_ast", "lab_alt", "lab_alp",
                          "lab_urea", "lab_ca", "lab_mg", "lab_po4", "lab_tni", 
                          "lab_lip", "lab_ck", "lab_uuro", "lab_spg", "lab_ph", 
                          "lab_amy", "lab_vlact", "lab_vhion", "lab_mvsa",
                          "lab_vpo2", "lab_vbe", "lab_vtco2", "lab_vpco2",
                          "lab_vph", "vital_spulse", "vital_sbpsystolic", 
                          "vital_sbpdiastolic", 
                          "vital_srespirations", 
                          "vital_so2saturation", 
                          "vital_stemperature")
  
  
  
  raw_baseline_mean <- baseline %>% 
    group_by(ENCOUNTER_NUM, variable) %>% 
    summarize(baseline = mean(numeric_value)) %>% 
    group_by(ENCOUNTER_NUM) %>% 
    spread(variable, baseline) %>% 
    ungroup()%>% 
    select(keep_baseline_vars) %>% 
    right_join(location_data %>% select(ENCOUNTER_NUM), by = "ENCOUNTER_NUM")
  
  nms <- names(raw_baseline_mean)
  
  setd <- setdiff(keep_baseline_vars, nms)
  
  if(length(setd) > 0) {
    for(i in 1:length(setd)) {
      raw_baseline_mean[[setd[i]]] <- NA
    }
    
    
  }
  
  # impute scores -----------------------------------------------------------
  
  for(i in keep_baseline_vars[keep_baseline_vars != "ENCOUNTER_NUM"]) {
    
    tmp <- numeric_descriptive_statistics[[i]]
    
    
    # impute
    raw_baseline_mean[[i]] <- ifelse(is.na(raw_baseline_mean[[i]]),
                                     tmp$q50, raw_baseline_mean[[i]] )
    
    #normalize
    raw_baseline_mean[[i]] <- (raw_baseline_mean[[i]] - tmp$q01)/(tmp$q99 - tmp$q01)
    
  }
  
  return(raw_baseline_mean)
}


clean_clinical_orders <- function(clinical_orders) {
  names(clinical_orders) <- toupper(names(clinical_orders))
  
  img <- clinical_orders %>% 
    filter(ORDERTYPEABBR == "Imaging")
  
  img <- img %>% 
    group_by(ORDER_OID) %>% 
    arrange(CHANGEDDATETIME) %>% 
    filter(row_number() == 1) %>% 
    ungroup()
  
  # take the top 100 image orders
  # top_img_orders <- img %>%
  #   count(COMMONDEFNAME, sort = T) %>%
  #   head(100)
  # cat(paste(shQuote(top_img_orders$COMMONDEFNAME, type="cmd"), collapse=", "))
  top_orders <- c("PORTABLE Chest", "XRAY Chest PA and LAT", 
                  "CT Head no Contrast", "PORTABLE Abdomen",
                  "US Abdomen Complete", "MRI Head",
                  "Interventional Radiological Procedure Request",
                  "CT Thorax", "CT Abdomen and Pelvis", "XRAY Abdomen AP and LAT",
                  "CT Abdomen Pelvis with Contrast", "XRAY Chest", 
                  "XRAY Abdomen", "US Abdomen and  Pelvis",
                  "CT Thorax Rule Out Pulmonary Embolus", 
                  "US Doppler  Vein and Extremity Bilateral", "US Abdomen Limited",
                  "US Abdomen Limited and Pelvis Limited", 
                  "PICC Insertion Double Lumen", "US Abdomen and Pelvis Limited",
                  "CT Abdomen", "US Doppler Vein and Extremity Unilateral",
                  "MRI0700", "CT Thorax with Contrast", "MRI Spine", 
                  "CT Thorax Abdomen Pelvis with Contrast", "CT Perfusion Stroke",
                  "CT Thorax Abdomen Pelvis", "XRAY Lumbar Spine 3 Views",
                  "XRAY Pelvis AP", "PORTABLE XRAY Abdomen AP and LAT",
                  "CT Head with Contrast", "MRI Abdomen", "CT Head and Carotids",
                  "PICC Insertion Single Lumen", "MRI0701",
                  "US Doppler Portal Vein", "CT Extremity", "Bone Scan Whole Body",
                  "MRI Extremity Unilateral", "XRAY Foot Left 3 Views",
                  "CT Head with and without Contrast", "MRA Brain",
                  "XRAY Foot Right 3 Views", "XRAY Hip Unilateral Left AP and LAT",
                  "CT Head Angio (Circle of Willis)",
                  "XRAY Hip Unilateral Right AP and LAT",
                  "US Extremity Right (Soft Tissue)", "US Face and Neck",
                  "CT Pelvis", "XRAY Deglutition Study", 
                  "CT Head and Cervical Spine", "CT Spine Lumbar without contrast",
                  "US Extremity Left (Soft Tissue)",
                  "US Doppler Abdomen Renal Arterial or Venous", "CT Neck", 
                  "XRAY Knee Left AP and LAT", "XRAY ERCP", 
                  "XRAY Knee Right AP and LAT", "MRA Carotids",
                  "XRAY Shoulder Left 3 Views", "CT Abdomen Rule Out Renal Colic",
                  "NVA Abscess Drain Thorax", "XRAY Shoulder Right 3 Views",
                  "XRAY Thoracic Spine 2 Views", "CT Abdominal Aneurysm", "MRI0733",
                  "US Doppler  Venous Lower Extremity Bilateral- R/O DVT", 
                  "Cardiolite Persantine", "Angiography Procedure Request",
                  "XRAY Cervical Spine AP, Lateral and Open Mouth Odontoid View",
                  "CT Thoarax Low Dose", "CT Abdomen Pelvis Triphasic Study", 
                  "XRAY Skull Orbits", "CT Spine Cervical without contrast", 
                  "V/Q Scan", "XRAY Hip Bilateral 4 Views", "CT Neck with Contrast",
                  "US Guided Biopsy", "XRAY Metastatic Survey", 
                  "US Doppler Liver Disease Screening", "MRI0824",
                  "XRAY Ankle Left 3 Views", "XRAY Ankle Right 3 Views",
                  "MRI Pelvis", "US Pelvis Transvaginal (First Trimester)", 
                  "CT Enterography", "US Soft Tissue Unilateral", 
                  "Hemodialysis Catheter Insertion", "XRAY Knee Right 4 Views",
                  "XRAY NG Tube Insertion", "XRAY Lumbar Puncture", 
                  "NVA Abscess Drain Abdomen", "XRAY Chest 3 Views", 
                  "MRI0832", "MRI0759", "XRAY Knee Left 4 Views", 
                  "Cardiolite and Persantine Scan with Rest", 
                  "XRAY Knee Bilateral 4 Views", "XRAY Tib Fib Right 2 Views")
  
  image_orders <- clinical_orders %>% 
    filter(COMMONDEFNAME %in% top_orders)
  
  
  
  image_orders <- image_orders %>%
    mutate(order_name = clean_var_name(ORDERABBREVIATION))
  
  #order_name_mappings <- image_orders %>% 
  #  distinct(ORDERABBREVIATION, order_name)
  # write.csv(order_name_mappings, file = paste0(file_path, "order_mappings/order_name_mappings.csv"), row.names = F)
  
  
  image_orders <- image_orders %>% 
    group_by(ORDER_OID) %>% 
    summarize(ENCOUNTER_NUM = max(ENCOUNTER_NUM),
              order_time_min = min(ENTEREDDATETIME),
              order_time_max = max(CHANGEDDATETIME),
              order_name = max(order_name)) %>% 
    mutate(value = 1) %>% 
    ungroup()
  
  
  
  ################################################################################
  
  #   -----------------------------------------------------------------------
  # clinical nutrition ------------------------------------------------------
  #   -----------------------------------------------------------------------
  
  nutrition_orders <- clinical_orders %>% 
    filter(ORDERTYPEABBR == "Clinical Nutrition") %>% 
    mutate(order_name = ifelse(ORDERSUBTYPEABBR %in% c('Oral', "NPO", "Nutrition Supplement",
                                                       "Enteral Feeding"), ORDERSUBTYPEABBR,
                               ifelse(grepl('Regular', DIETMODIFIER1, ignore.case = T), "regular",
                                      ifelse(grepl('NPO', DIETMODIFIER1, ignore.case = T), "npo",
                                             ifelse(grepl('Diabetic', DIETMODIFIER1, ignore.case = T), "diabetic",
                                                    ifelse(grepl('Clear Fluids', DIETMODIFIER1, ignore.case = T), "clear_fluids",
                                                           ifelse(grepl('Tube Feed', DIETMODIFIER1, ignore.case = T), "tube_feed",
                                                                  ifelse(grepl('Renal', DIETMODIFIER1, ignore.case = T), "renal",
                                                                         ifelse(grepl("Cardiac", DIETMODIFIER1, ignore.case = T), 
                                                                                "cardiac", "regular_other"))))))))) %>% 
    mutate(order_name = tolower(gsub(" ", "_", order_name))) %>% 
    filter(order_name != "enteral_feeding")
  
  
  nutrition_orders <- nutrition_orders %>% 
    group_by(ORDER_OID) %>% 
    summarize(ENCOUNTER_NUM = max(ENCOUNTER_NUM),
              order_time_min = min(ENTEREDDATETIME),
              order_time_max = max(CHANGEDDATETIME),
              order_name = max(order_name)) %>% 
    mutate(value = 1) %>% 
    ungroup()
  
  
  ################################################################################
  
  #   -----------------------------------------------------------------------
  # telemetry ---------------------------------------------------------------
  #   -----------------------------------------------------------------------
  
  
  telemetry_orders <- clinical_orders %>% 
    filter(ORDERTYPEABBR == "Assessments & Monitoring",
           ORDERSUBTYPEABBR == "Telemetry") %>% 
    mutate(order_name = "telemetry")
  
  telemetry_orders <- telemetry_orders %>% 
    group_by(ORDER_OID) %>% 
    summarize(ENCOUNTER_NUM = max(ENCOUNTER_NUM),
              order_time_min = min(ENTEREDDATETIME),
              order_time_max = max(CHANGEDDATETIME),
              order_name = max(order_name)) %>% 
    mutate(value = 1) %>% 
    ungroup()
  
  
  ################################################################################
  
  #   -----------------------------------------------------------------------
  # cardiovascular diagnostics ----------------------------------------------
  #   -----------------------------------------------------------------------
  
  
  cardio_diagnostics <- clinical_orders %>% 
    filter(ORDERTYPEABBR == "Cardiovascular Diagnostics") %>% 
    mutate(order_name = clean_var_name(ORDERSUBTYPEABBR))
  
  
  cardio_diagnostics <- cardio_diagnostics %>% 
    group_by(ORDER_OID) %>% 
    summarize(ENCOUNTER_NUM = max(ENCOUNTER_NUM),
              order_time_min = min(ENTEREDDATETIME),
              order_time_max = max(CHANGEDDATETIME),
              order_name = max(order_name)) %>% 
    mutate(value = 1) %>% 
    ungroup() %>% 
    mutate(order_name = clean_var_name(order_name),
           order_name = tolower(order_name))
  
  
  # take the top 5 image orders
  # top_cvd_orders <- cardio_diagnostics %>%
  #   count(order_name, sort = T) %>%
  #   head(5)
  
  top_cvd_orders <- c("ecg", "echo",
                      "vascularlab",
                      "holter",
                      "peripheralvascular")
  
  cardio_diagnostics <- cardio_diagnostics %>% 
    filter(order_name %in% top_cvd_orders)
  
  
  ################################################################################
  
  #   -----------------------------------------------------------------------
  # respiratory -------------------------------------------------------------
  #   -----------------------------------------------------------------------
  
  # USE ORDERSUBTYPEABBR for all ORDERTYPEABBR  == "Respiratory"
  respiratory <- clinical_orders %>% 
    filter(ORDERTYPEABBR == "Respiratory") %>% 
    mutate(order_name = clean_var_name(ORDERSUBTYPEABBR),
           order_name = tolower(order_name))
  
  
  respiratory <- respiratory %>% 
    group_by(ORDER_OID) %>% 
    summarize(ENCOUNTER_NUM = max(ENCOUNTER_NUM),
              order_time_min = min(ENTEREDDATETIME),
              order_time_max = max(CHANGEDDATETIME),
              order_name = max(order_name)) %>% 
    mutate(value = 1) %>% 
    ungroup()
  
  # take the top 6 image orders
  # top_resp_orders <- respiratory %>%
  #  count(order_name, sort = T) %>%
  #   head(6)
  
  top_resp_orders <- c("oxygen", "respiratoryintervention", "ventilator", "pulmonaryfunctiontest", "chesttube", "bipapcpap")
  
  respiratory <- respiratory %>% 
    filter(order_name %in% top_resp_orders)
  
  ################################################################################
  
  #   -----------------------------------------------------------------------
  # Infection Control Status ------------------------------------------------
  #   -----------------------------------------------------------------------
  
  # use order as written for "Infection Control Status" (airborne, respiratory, contact, droplet, routine)
  
  
  infection <- clinical_orders %>% 
    filter(ORDERTYPEABBR == "Infection Control Status") %>% 
    mutate(respiratory = ifelse(grepl('respiratory', ORDERDESCASWRITTEN, ignore.case = T), "respiratory", ""),
           airborne = ifelse(grepl('airborne', ORDERDESCASWRITTEN, ignore.case = T), "airborne", ""),
           contact = ifelse(grepl('contact', ORDERDESCASWRITTEN, ignore.case = T), "contact", ""),
           droplet = ifelse(grepl('droplet', ORDERDESCASWRITTEN, ignore.case = T), "droplet", ""),
           routine = ifelse(grepl('routine', ORDERDESCASWRITTEN, ignore.case = T), "routine", "")) 
  
  inf_resp <- infection %>% 
    filter(respiratory == "respiratory") %>% 
    mutate(order_name = 'infection_resp') %>% 
    group_by(ORDER_OID) %>% 
    summarize(ENCOUNTER_NUM = max(ENCOUNTER_NUM),
              order_time_min = min(ENTEREDDATETIME),
              order_time_max = max(CHANGEDDATETIME),
              order_name = max(order_name)) %>% 
    mutate(value = 1) %>% 
    ungroup()
  
  inf_air <- infection %>% 
    filter(respiratory == "airborne") %>% 
    mutate(order_name = 'infection_air') %>% 
    group_by(ORDER_OID) %>% 
    summarize(ENCOUNTER_NUM = max(ENCOUNTER_NUM),
              order_time_min = min(ENTEREDDATETIME),
              order_time_max = max(CHANGEDDATETIME),
              order_name = max(order_name)) %>% 
    mutate(value = 1) %>% 
    ungroup()
  
  inf_contact <- infection %>% 
    filter(respiratory == "contact") %>% 
    mutate(order_name = 'infection_contact') %>% 
    group_by(ORDER_OID) %>% 
    summarize(ENCOUNTER_NUM = max(ENCOUNTER_NUM),
              order_time_min = min(ENTEREDDATETIME),
              order_time_max = max(CHANGEDDATETIME),
              order_name = max(order_name)) %>% 
    mutate(value = 1) %>% 
    ungroup()
  
  inf_routine <- infection %>% 
    filter(respiratory == "routine") %>% 
    mutate(order_name = 'infection_routine') %>% 
    group_by(ORDER_OID) %>% 
    summarize(ENCOUNTER_NUM = max(ENCOUNTER_NUM),
              order_time_min = min(ENTEREDDATETIME),
              order_time_max = max(CHANGEDDATETIME),
              order_name = max(order_name)) %>% 
    mutate(value = 1) %>% 
    ungroup()
  
  inf_drop <- infection %>% 
    filter(respiratory == "droplet") %>% 
    mutate(order_name = 'infection_drop') %>% 
    group_by(ORDER_OID) %>% 
    summarize(ENCOUNTER_NUM = max(ENCOUNTER_NUM),
              order_time_min = min(ENTEREDDATETIME),
              order_time_max = max(CHANGEDDATETIME),
              order_name = max(order_name)) %>% 
    mutate(value = 1) %>% 
    ungroup()
  
  infection_control <- inf_resp %>% 
    bind_rows(inf_air) %>% 
    bind_rows(inf_contact) %>% 
    bind_rows(inf_routine)  %>% 
    bind_rows(inf_drop) 
  
  
  ################################################################################
  
  #   -----------------------------------------------------------------------
  # consults ----------------------------------------------------------------
  #   -----------------------------------------------------------------------
  
  # take first word after consult in orderaswritten for consults
  
  consults <- clinical_orders %>% 
    filter(ORDERTYPEABBR == "Consults and Referrals") %>% 
    filter(grepl("^Consult", ORDERDESCASWRITTEN, ignore.case = T) | 
             grepl("^wound", ORDERDESCASWRITTEN, ignore.case = T))
  
  a <- stringr::str_split(consults$ORDERDESCASWRITTEN, ":")
  consults <- consults %>% 
    mutate(order_name = ifelse(grepl("^Consult", ORDERDESCASWRITTEN, ignore.case = T),
                               tolower(stringr::str_extract(ORDERDESCASWRITTEN, '(?<=:\\s)\\w+')),
                               "wound"))
  
  # replace the following order names
  # james = first word afer request
  # vivan = occupational (Vivian Ng is an occupational therapist)
  consults <- consults %>% 
    mutate(order_name = case_when(
      order_name == "james" ~ tolower(stringr::str_extract(ORDERDESCASWRITTEN, '(?<=Request:\\s)\\w+')),
      order_name == "vivian" ~ tolower(stringr::str_extract(ORDERDESCASWRITTEN, '(?<=Request:\\s)\\w+')),
      order_name == "trevor" ~ tolower(stringr::str_extract(ORDERDESCASWRITTEN, '(?<=Request:\\s)\\w+')),
      TRUE ~ order_name
      
    ))
  
  
  
  # take consults that appear more than 100 times
  # top_consult_orders <- consults %>%
  #   count(order_name, sort = T) %>%
  #   filter (n > 100)
  
  top_consult_orders <- c("physiotherapist", "general", "occupational", 
                          "dietitian", "speech", "wound", "research",
                          "acute", "stroke", "pharmacist", "social", 
                          "gastroenterology", "physio", "psychiatry",
                          "respiratory", "addiction", "chaplain", 
                          "geriatric", "chiropodist", "physiotherapy")
  
  
  consults <- consults %>% 
    filter(order_name %in% top_consult_orders)
  
  consults <- consults %>% 
    group_by(ORDER_OID) %>% 
    summarize(ENCOUNTER_NUM = max(ENCOUNTER_NUM),
              order_time_min = min(ENTEREDDATETIME),
              order_time_max = max(CHANGEDDATETIME),
              order_name = max(order_name)) %>% 
    mutate(value = 1) %>% 
    ungroup()
  
  consults <- consults %>% 
    filter(!is.na(order_name))
  
  ################################################################################
  
  #   -----------------------------------------------------------------------
  # Activity & Limitations --------------------------------------------------
  #   -----------------------------------------------------------------------
  
  # taking specified commondefname
  
  act_list <- c("O_PCO_PhysRestr", "Sitter",
                "Constant Care", "Restrictions")
  
  activity_limitations <- clinical_orders %>% 
    filter(ORDERTYPEABBR == "Activity & Limitations") %>% 
    filter(COMMONDEFNAME %in% act_list) %>% 
    mutate(order_name = clean_var_name(COMMONDEFNAME))
  
  activity_limitations <- activity_limitations %>% 
    group_by(ORDER_OID) %>% 
    summarize(ENCOUNTER_NUM = max(ENCOUNTER_NUM),
              order_time_min = min(ENTEREDDATETIME),
              order_time_max = max(CHANGEDDATETIME),
              order_name = max(order_name)) %>% 
    mutate(value = 1) %>% 
    ungroup()
  
  
  ################################################################################
  
  #   -----------------------------------------------------------------------
  # code status -------------------------------------------------------------
  #   -----------------------------------------------------------------------
  
  
  # ORDERTYPEABBR == "Code Status" use condition (on or off)
  
  # codes
  # Full code ~ code_full
  # No CPR: Advanced Life Support ~ code_als
  # No CPR: Comprehensive Comfort Care ~ code_ccc
  # No CPR: General Medical Care ~ code_gmc
  
  code_status <- clinical_orders %>% 
    filter(ORDERTYPEABBR == "Code Status") %>% 
    mutate(order_name = case_when(
      CONDITION == "Full Code" ~ "code_full",
      CONDITION == "No CPR: Advanced Life Support" ~ "code_als",
      CONDITION == "No CPR: Comprehensive Comfort Care" ~ "code_ccc",
      CONDITION == "No CPR: General Medical Care" ~ "code_gmc",
      TRUE ~ "other"
    )) %>% 
    filter(order_name != 'other')
  
  code_status <- code_status %>% 
    group_by(ORDER_OID) %>% 
    summarize(ENCOUNTER_NUM = max(ENCOUNTER_NUM),
              order_time_min = min(ENTEREDDATETIME),
              order_time_max = max(CHANGEDDATETIME),
              order_name = max(order_name)) %>% 
    mutate(value = 1) %>% 
    ungroup()
  
  ################################################################################
  
  #   -----------------------------------------------------------------------
  # code status -------------------------------------------------------------
  #   -----------------------------------------------------------------------
  
  #filter(ORDERTYPEABBR == "Tubes, Drains & Elimination") use commondefname
  
  tubes <- clinical_orders %>% 
    filter(ORDERTYPEABBR == "Tubes, Drains & Elimination") %>% 
    filter(!COMMONDEFNAME %in% c("O_PCO_ED_2Foley", # REMOVE EMERGENCY DEPT ORDERS
                                 "O_PCO_ED_TapEnem",
                                 "O_PCO_ED_InOutC",
                                 "O_PCO_ED_UrineO")) %>% 
    mutate(order_name = clean_var_name(COMMONDEFNAME))
  
  
  
  # take consults that appear more than 100 times
  # top_tubes_orders <- tubes %>%
  #   count(order_name, sort = T) %>%
  #   filter (n > 100)
  
  top_tubes_orders <- c("inserturinarycatheter", "monitoroutput", "catheterizationintermittent",
                        "flushenteraltube", "opcogitube",
                        "enematapwater", "inserturinarycathetericu",
                        "changecatheter")
  
  tubes <- tubes %>% 
    filter(order_name %in% top_tubes_orders)
  
  tubes <- tubes %>% 
    group_by(ORDER_OID) %>% 
    summarize(ENCOUNTER_NUM = max(ENCOUNTER_NUM),
              order_time_min = min(ENTEREDDATETIME),
              order_time_max = max(CHANGEDDATETIME),
              order_name = max(order_name)) %>% 
    mutate(value = 1) %>% 
    ungroup()
  
  
  ################################################################################
  
  #   -----------------------------------------------------------------------
  # Protocol Orders ---------------------------------------------------------
  #   -----------------------------------------------------------------------
  
  
  
  # filter(ORDERTYPEABBR == "Protocol Orders") use COMMONDEFNAME
  
  protocol_list <- c("O_PCO_CIWAcare",
                     "O_PCO_Hep_HiaPTT",
                     "O_PCO_IVIns_Low",
                     "O_PCO_CHFDiuret",
                     "O_PCO_IVIns_Hi")
  
  protocol_orders <- clinical_orders %>% 
    filter(ORDERTYPEABBR == "Protocol Orders") %>% 
    filter(COMMONDEFNAME %in% protocol_list) %>% 
    mutate(order_name = clean_var_name(COMMONDEFNAME))
  
  protocol_orders <- protocol_orders %>% 
    group_by(ORDER_OID) %>% 
    summarize(ENCOUNTER_NUM = max(ENCOUNTER_NUM),
              order_time_min = min(ENTEREDDATETIME),
              order_time_max = max(CHANGEDDATETIME),
              order_name = max(order_name)) %>% 
    mutate(value = 1) %>% 
    ungroup()
  
  ################################################################################
  
  #   -----------------------------------------------------------------------
  # Transfusion -------------------------------------------------------------
  #   -----------------------------------------------------------------------
  
  transfusion_list <- c("Transfuse Packed Red Blood Cells",
                        "Infuse Albumin 25%",
                        "Transfuse Platelets",
                        "Infuse IV Immune Globulin",
                        "Infuse Frozen Plasma",
                        "Prothrombin Complex Concentrate (PCC)")
  transfusion <- clinical_orders %>% 
    filter(ORDERTYPEABBR == "Transfusion") %>% 
    mutate(order_name = ifelse(COMMONDEFNAME %in% transfusion_list,
                               COMMONDEFNAME, 'transfusion_other'),
           order_name = clean_var_name(order_name))
  
  
  transfusion <- transfusion %>% 
    group_by(ORDER_OID) %>% 
    summarize(ENCOUNTER_NUM = max(ENCOUNTER_NUM),
              order_time_min = min(ENTEREDDATETIME),
              order_time_max = max(CHANGEDDATETIME),
              order_name = max(order_name)) %>% 
    mutate(value = 1) %>% 
    ungroup()
  
  
  ################################################################################
  
  #   -----------------------------------------------------------------------
  # Dressings & Wound/Skin Care ---------------------------------------------
  #   -----------------------------------------------------------------------
  
  woundcare_list <- c("Dressings & Wound Care",
                      "Skin Care")
  
  woundcare <- clinical_orders %>% 
    filter(ORDERSUBTYPEABBR %in% woundcare_list) %>% 
    mutate(order_name = clean_var_name(ORDERSUBTYPEABBR))
  
  
  woundcare <- woundcare %>% 
    group_by(ORDER_OID) %>% 
    summarize(ENCOUNTER_NUM = max(ENCOUNTER_NUM),
              order_time_min = min(ENTEREDDATETIME),
              order_time_max = max(CHANGEDDATETIME),
              order_name = max(order_name)) %>% 
    mutate(value = 1) %>% 
    ungroup()
  
  
  ################################################################################
  
  #   -----------------------------------------------------------------------
  # Dressings & Wound/Skin Care ---------------------------------------------
  #   -----------------------------------------------------------------------
  # ORDERTYPEABBR == "Neurophysiology" use ORDERSUBTYPEABBR
  neuro_list <- c("E.E.G.",
                  "E.M.G.")
  
  neuro <- clinical_orders %>% 
    filter(ORDERTYPEABBR == "Neurophysiology",
           ORDERSUBTYPEABBR %in% neuro_list) %>% 
    mutate(order_name = clean_var_name(ORDERSUBTYPEABBR))
  
  
  neuro <- neuro %>% 
    group_by(ORDER_OID) %>% 
    summarize(ENCOUNTER_NUM = max(ENCOUNTER_NUM),
              order_time_min = min(ENTEREDDATETIME),
              order_time_max = max(CHANGEDDATETIME),
              order_name = max(order_name)) %>% 
    mutate(value = 1) %>% 
    ungroup()
  
  
  # Recombine all of the order sets -----------------------------------------
  
  image_orders <- image_orders %>% 
    mutate(type = 'Imaging') %>% 
    mutate(order_name = paste0('img_',order_name))
  
  nutrition_orders <- nutrition_orders %>% 
    mutate(type = 'Diets') %>% 
    mutate(order_name = paste0('diet_',order_name))
  
  telemetry_orders <- telemetry_orders %>% 
    mutate(type = 'Monitoring')
  
  consults <- consults %>% 
    mutate(type = 'Consult') %>% 
    mutate(order_name = paste0('consult_',order_name))
  
  cardio_diagnostics <- cardio_diagnostics %>% 
    mutate(type = 'Cardio Diagnostics') %>% 
    mutate(order_name = paste0('cardio_',order_name))
  
  respiratory <- respiratory %>% 
    mutate(type = 'Respiratory') %>% 
    mutate(order_name = paste0('resp_',order_name))
  
  activity_limitations <- activity_limitations %>% 
    mutate(type = 'Activity Limitations') %>% 
    mutate(order_name = paste0('act_',order_name))
  
  code_status <- code_status %>% 
    mutate(type = 'Code Status') 
  
  
  tubes <- tubes %>% 
    mutate(type = 'Tubes') %>% 
    mutate(order_name = paste0('tube_',order_name))
  
  protocol_orders <- protocol_orders %>% 
    mutate(type = 'Protocol Orders') 
  
  transfusion <- transfusion %>% 
    mutate(type = 'Transfusion') %>% 
    mutate(order_name = paste0('trans_',order_name))
  
  woundcare <- woundcare %>% 
    mutate(type = 'Wound Care') %>% 
    mutate(order_name = paste0('wound_',order_name))
  
  neuro <- neuro %>% 
    mutate(type = 'Neurophysiology') %>% 
    mutate(order_name = paste0('neuro_',order_name))
  
  clinical_orders_clean <- image_orders %>% 
    bind_rows(nutrition_orders) %>% 
    bind_rows(telemetry_orders) %>% 
    bind_rows(consults) %>% 
    bind_rows(cardio_diagnostics) %>% 
    bind_rows(respiratory) %>% 
    bind_rows(activity_limitations) %>% 
    bind_rows(code_status) %>% 
    bind_rows(protocol_orders) %>% 
    bind_rows(transfusion) %>% 
    bind_rows(woundcare) %>% 
    bind_rows(neuro)
  
  return(clinical_orders_clean)
  
}

prepare_numeric_timeseries <- function(data, patient_ts) {
  
  patient_ts <- patient_ts %>% 
    mutate(ENCOUNTER_NUM = as.character(ENCOUNTER_NUM))
  data <- data %>% 
    mutate(ENCOUNTER_NUM = stringr::str_sub(ENCOUNTER_NUM, 3, -1))
  load("numeric_descriptive_statistics.R")
  var_list <- c("alcohol_sciwascore", "diabetic_spocglucresult", "in_out_acatheter",
                "in_out_aivpb1", "in_out_aothoutput", "in_out_atmsincontinent", 
                "in_out_siv23and13", "in_out_sivnormalsaline", "in_out_sotherintake", 
                "lab_abaso", "lab_abe", "lab_acet", "lab_aeos", "lab_agap", "lab_ahion", 
                "lab_alact", "lab_alb", "lab_alp", "lab_alt", "lab_alymp",
                "lab_amono", "lab_amy", "lab_aneut", "lab_ao", "lab_apco2", 
                "lab_aph", "lab_apo2", "lab_asa", "lab_ast", "lab_atco2",
                "lab_b12", "lab_bc", "lab_bnps", "lab_ca", "lab_cacra", "lab_cai", 
                "lab_caicr", "lab_ck", "lab_cl", "lab_co2", "lab_cr", 
                "lab_crp", "lab_esr1", "lab_etoh", "lab_fe", "lab_fer",
                "lab_glob", "lab_glpoc", "lab_glur", "lab_hba1", "lab_hct", 
                "lab_hgb", "lab_ical", "lab_igab", "lab_ivsd", "lab_iwbcr", 
                "lab_k", "lab_la", "lab_ld", "lab_lip", "lab_lvedd", "lab_lvesd", 
                "lab_masa", "lab_mch", "lab_mchc", "lab_mcv", "lab_metaa", "lab_mg",
                "lab_mpv", "lab_mvsa", "lab_myela", "lab_na", "lab_orcai",
                "lab_orglu", "lab_orhc", "lab_ork", "lab_orna", "lab_osm", 
                "lab_ph", "lab_plt", "lab_po4",  "lab_pwd", 
                "lab_rbc", "lab_rdw", "lab_reta", "lab_rinr", "lab_rpt", 
                "lab_rptt", "lab_sat", "lab_spg", "lab_tbil", "lab_tibc", 
                "lab_tni", "lab_tpr", "lab_tsh", "lab_ucl", "lab_uk", "lab_una",
                "lab_uosm", "lab_urea", "lab_uuro", "lab_vbe", "lab_vhion", 
                "lab_vlact", "lab_vpco2", "lab_vph", "lab_vpo2", "lab_vtco2", 
                "shift_assess_scvhrtrate", "shift_assess_spninstymov1", 
                "shift_assess_spninstymov2", "shift_assess_spnintstyrest1",
                "shift_assess_spnintstyrest2", "shift_assess_srpfio2b", 
                "shift_assess_srpo2lmin", "skin_abradenscore", "vital_sbpdiastolic", 
                "vital_sbpsystolic", "vital_sfio2", "vital_so2saturation",
                "vital_spainintmove", "vital_spainintrest", "vital_spulse", 
                "vital_srespirations", "vital_stemperature")
  
  
  data_hr <- data %>% 
    mutate(timestamp = ceiling_date(timestamp, unit = 'hour')) %>% 
    data.table::as.data.table() %>% 
    .[, lapply(.SD, mean, na.rm = T), by = .(ENCOUNTER_NUM, variable, timestamp)] %>% 
    as_tibble()
  
  data_hr_sp <- data_hr %>% 
    group_by(ENCOUNTER_NUM) %>% 
    spread(variable, numeric_value)
  
  data_hr_sp <- patient_ts %>% 
    left_join(data_hr_sp, by = c("ENCOUNTER_NUM", "timestamp"))
  
  current_names <- names(data_hr_sp)
  
  setd <- setdiff(var_list, current_names)
  
  
  if(length(setd) > 0) {
    
    for(i in 1:length(setd)) {
      data_hr_sp[[setd[i]]] <- NA
    }
  }
  
  trimmed_numeric <- trim(data_hr_sp %>% 
                            select(names(numeric_descriptive_statistics)),
                          numeric_descriptive_statistics)
  
  trimmed_numeric <- data_hr_sp %>% select( "ENCOUNTER_NUM", 
                                            "timestamp", "time_elapsed","timestamp_6hr") %>% 
    bind_cols(trimmed_numeric) %>% 
    rename(time_window = timestamp_6hr)
  
  
  ids <- trimmed_numeric %>% 
    select(ENCOUNTER_NUM,
           time_window,
           time_elapsed,
           timestamp) %>% 
    group_by(ENCOUNTER_NUM, time_window) %>% 
    arrange(desc(timestamp)) %>% 
    filter(row_number() == 1) %>% 
    ungroup() %>% 
    arrange(ENCOUNTER_NUM, time_window) 
  
  trimmed_numeric_agg <- trimmed_numeric %>% 
    data.table::as.data.table() %>% 
    .[, lapply(.SD, mean, na.rm=TRUE), by=.(ENCOUNTER_NUM, time_window), .SDcols= var_list ] 
  
  
  trimmed_numeric_agg <- as.data.frame(trimmed_numeric_agg)
  trimmed_numeric_agg[is.nan(trimmed_numeric_agg)] <- NA
  
  trimmed_numeric_agg <- trimmed_numeric_agg %>% 
    mutate_at(vars(var_list), funs(measured = ifelse(is.na(.),0,1)))
  
  cols_change <- colnames(trimmed_numeric_agg)[grepl("measured", colnames(trimmed_numeric_agg))]
  
  trimmed_numeric_agg <- trimmed_numeric_agg %>%
    group_by(ENCOUNTER_NUM) %>% 
    mutate_at(vars(cols_change), funs(last = counter_function(.))) %>% 
    group_by(ENCOUNTER_NUM) %>% 
    fill(var_list) %>% 
    ungroup()
  
  cat("Filling in last observation carried forward...")
  data_filled <- trimmed_numeric_agg %>% 
    group_by(ENCOUNTER_NUM) %>% 
    fill(var_list) %>% 
    ungroup()
  cat("COMPLETE  \n")
  
  
  tmp_ids <- data_filled %>% 
    select(ENCOUNTER_NUM, time_window)
  tmp_measures <- data_filled %>% 
    select(contains("measure"))
  
  tmp_data <- data_filled %>% 
    select(names(numeric_descriptive_statistics)) %>% 
    normalization_minmax(numeric_descriptive_statistics) %>% 
    impute_mean(numeric_descriptive_statistics)
  
  data_filled <- tmp_ids %>% 
    bind_cols(tmp_data) %>% 
    bind_cols(tmp_measures)
  
  
  numeric_timeseries <- data_filled %>% 
    left_join(ids, by = c("ENCOUNTER_NUM", "time_window")) %>% 
    select( ENCOUNTER_NUM,
            timestamp, time_window, 
            everything())
  
  
  return(numeric_timeseries)
}

prepare_orders_timeseries <- function(data, patient_ts) {
  
  patient_ts <-patient_ts %>% 
    mutate(ENCOUNTER_NUM = as.character(ENCOUNTER_NUM))
  data <- data %>% 
    mutate(ENCOUNTER_NUM = stringr::str_sub(ENCOUNTER_NUM, 3, -1))
  order_names <- c("img_porchest1v", "img_ctthrabdplc",
                   "img_radchest2v", "img_usabdlimited", 
                   "img_ctthoraxpe", "img_mriheado", 
                   "img_radabdo2v", "img_ctthoraxnoc", 
                   "img_usabdcomplete", "img_ctperfusionstrok", 
                   "img_ctabdoplcont", "img_ctheadnocont",
                   "img_usdopvenextb", "img_ctabdonocont", 
                   "img_radpelvisap", "img_radabdo1v",
                   "img_ctabdoplwcon", "img_usabdpelvis",
                   "img_cardioliteprrs", "img_radlumspin3v",
                   "img_porabdomen1v", "img_int0180",
                   "img_ctabdopelphas", "img_mrispineo",
                   "img_usdopportal", "img_radchest1v", 
                   "img_mriabdomeno", "img_ctheadwithcon", 
                   "img_radhipunilt2v", "img_usdopabdoart",
                   "img_ctheadangio", "img_ctthoraxc", 
                   "img_porabdomen2v", "img_gastricdeglut", 
                   "img_bonewholebody", "img_radkneert2v", 
                   "img_ctneckc", "img_ctextremity", 
                   "img_spllumbarpunct", "img_usfaceneck", 
                   "img_ctneckcont", "img_ctheadwwocon",
                   "img_ctheadcarotids", "img_ctrenalcolic",
                   "img_radkneelt2v", "img_radcervspin3v",
                   "img_usguidedbiopsy", "img_radhipunirt2v", 
                   "img_ctpelvisc", "img_radfootleft3v",
                   "img_splercp", "img_usabdpelltd", 
                   "img_uspeltrvagftri", "img_radskulorbits",
                   "img_ctspinelcont", "img_ussofttissueuni",
                   "img_usextremityl", "img_radchest3v", 
                   "img_usextremityr", "img_hemodiacathins",
                   "img_ang0090", "img_nva3", "img_radfootrt3v",
                   "img_radshoulderrt2v", "img_ctheadcspnc", 
                   "img_survmetastatic", "img_radhipbilat4v", 
                   "img_cardiolitepr", "img_lungvq", 
                   "img_radshoulderlt3v", "img_radanklert3v",
                   "img_radkneelt4v", "img_splngtubeins",
                   "img_usdopliver", "img_radkneert4v", 
                   "img_radtibfibrt2v", "img_radanklelt3v", 
                   "img_radthorspin2v", "img_mrabrain", 
                   "img_ctenterography", "img_ctspinecervical",
                   "img_radkneebil4v", "img_usdopvnexuni", 
                   "img_usdopvnexbi", "img_mriextremityo", 
                   "img_mripelviso", "img_nva9", "img_mracarotids",
                   "img_ctthld", "img_ctaaa", "img_intpcin",
                   "img_intpcsl", "img_usabdltdpelvltd", 
                   "img_mri0733", "img_mri0759", "img_mri0701",
                   "img_mri0700", "img_mri0824", "img_mri0832",
                   "diet_tube_feed", "diet_regular_other", 
                   "diet_oral", "diet_renal", "diet_diabetic", 
                   "diet_cardiac", "diet_npo", "diet_regular",
                   "diet_clear_fluids", "diet_nutrition_supplement",
                   "telemetry", "consult_physio", "consult_general",
                   "consult_stroke", "consult_social", "consult_speech",
                   "consult_dietitian", "consult_chaplain",
                   "consult_physiotherapist", "consult_acute",
                   "consult_gastroenterology", "consult_respiratory",
                   "consult_occupational", "consult_psychiatry", 
                   "consult_wound", "consult_physiotherapy", 
                   "consult_geriatric", "consult_pharmacist",
                   "consult_chiropodist", "consult_addiction", 
                   "consult_research", "cardio_ecg",
                   "cardio_vascularlab", "cardio_echo",
                   "cardio_holter", "cardio_peripheralvascular",
                   "resp_oxygen", "resp_pulmonaryfunctiontest", 
                   "resp_bipapcpap", "resp_respiratoryintervention",
                   "resp_chesttube", "resp_ventilator", "act_sitter", 
                   "act_constantcare", "act_opcophysrestr", "act_restrictions",
                   "code_gmc", "code_ccc", "code_full", "code_als",
                   "opcociwacare", "opcohephiaptt", "opcoivinslow",
                   "opcoivinshi", "opcochfdiuret", "trans_infusefrozenplasma",
                   "trans_transfusepackedredbloodcells", "trans_transfuseplatelets", 
                   "trans_infusealbumin25", "trans_transfusionother", 
                   "trans_infuseivimmuneglobulin", 
                   "trans_prothrombincomplexconcentratepcc",
                   "wound_dressingswoundcare", "wound_skincare",
                   "neuro_eeg", "neuro_emg")
  
  
  clinical_orders_clean <- data %>% 
    mutate(order_time_min = ceiling_date(order_time_min, "hour"),
           order_time_max = ceiling_date(order_time_max, "hour")) %>% 
    filter(order_time_max >= order_time_min) 
  
  
  for(i in order_names) {
    patient_ts[[i]] <- 0
    
  }
  
  seq2 <- Vectorize(seq.POSIXt, vectorize.args = c("from", "to"))
  clinical_order_on_off <- list()
  unique_encounters <- unique(patient_ts$ENCOUNTER_NUM)
  for(i in unique_encounters) {
    
    tmp <- patient_ts %>% 
      filter(ENCOUNTER_NUM == i)
    
    for(j in order_names) {
      
      tmp_orders <- clinical_orders_clean %>% 
        filter(ENCOUNTER_NUM == i, order_name == j)
      
      if(nrow(tmp_orders) == 0) {
        next
      } else if(nrow(tmp_orders) == 1) {
        
        s <- seq(tmp_orders$order_time_min, tmp_orders$order_time_max, by = "hour")
        
        tmp <- tmp %>% 
          mutate(!!j := ifelse(timestamp %in% s, 1, 0))
        
      } else {
        
        d <- unique(as_datetime(do.call(c, as.list(seq2(tmp_orders$order_time_min, tmp_orders$order_time_max, by = "hour"))),
                                tz = "UTC"))
        
        tmp <- tmp %>% 
          mutate(!!j := ifelse(timestamp %in% d, 1, 0))
      }
      
    }
    
    clinical_order_on_off[[i]] <- tmp
    
  }
  clinical_order_on_off_ts <- data.table::rbindlist(clinical_order_on_off) %>% 
    as_tibble()
  
  return(clinical_order_on_off_ts)  
  
}




# connect to the edw ------------------------------------------------------
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

get_demographics <- function(con, pat, gim_encounters) {
  pat <- pat %>% 
    mutate(PATIENT_DK = as.character(PATIENT_DK))
  # extract patient dimension -----------------------------------------------
  gim_encounters <- gim_encounters %>% 
    mutate(ENCOUNTER_NUM = as.character(ENCOUNTER_NUM))
  inp <- dim_tbl(con, "INPATIENT_ENCOUNTER_FACT") %>% 
    select(ENCOUNTER_NUM, PATIENT_DK) %>% 
    filter(ENCOUNTER_NUM %in% local(gim_encounters$ENCOUNTER_NUM)) %>% 
    collect() 
  
  gim_encounters <- gim_encounters %>% 
    left_join(inp, by = "ENCOUNTER_NUM")
  
  gim_encounters <- gim_encounters %>% 
    mutate(PATIENT_DK = as.character(PATIENT_DK))
  pat_clean <- clean_demographics(pat)
  pat_clean <- pat_clean %>% 
    mutate(PATIENT_DK = as.character(PATIENT_DK))
  patients <- pat_clean %>% 
    filter(PATIENT_DK %in% unique(gim_encounters$PATIENT_DK))
  
  patients <- patients %>% 
    mutate(no_housing = get_housing_indicator(patients)) %>% 
    mutate(fsa = stringr::str_sub(POSTAL_CD, 1, 3)) %>% 
    select(-POSTAL_CD, -ADDRESS_LINE_1_DESCR, -ADDRESS_LINE_2_DESCR)
  
  
  gim_encounters <- gim_encounters %>% 
    left_join(patients, by = "PATIENT_DK")
  
  
  demographics <- gim_encounters %>% 
    select(ENCOUNTER_NUM, marital,
           province = STATE_PROVINCE_CD,
           language,
           religion,
           no_housing, 
           fsa,
           city = CITY_NM)
  
  return(demographics)
}


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


extract_nurse_notes <- function(notes, patient_ts) {
 
  patient_ts <- patient_ts %>% 
    mutate(ENCOUNTER_NUM = as.character(ENCOUNTER_NUM))
  notes <- notes %>% 
    mutate(ENCOUNTER_NUM = substr(ENCOUNTER_NUM ,3, 11)) %>% 
    mutate(timestamp = lubridate::ceiling_date(timestamp, unit = 'hours'))
  
  notes <- patient_ts %>% 
    left_join(notes, by = c("ENCOUNTER_NUM", "timestamp"))
  
  notes <- notes %>% 
    mutate(note = ifelse(is.na(note), "", note))
  
  clean_notes <- notes %>% 
    group_by(ENCOUNTER_NUM, time_window = timestamp_6hr) %>% 
    summarize(timestamp = max(timestamp),
              note = paste(note, collapse = " ")) %>% 
    ungroup()
  
  clean_notes <- clean_notes %>% 
    mutate(note_id = 1:nrow(clean_notes)) %>% 
    select(note_id, ENCOUNTER_NUM, timestamp, note)
  
  return(clean_notes)
}

clean_numeric_data <- function(lab_results, numeric_soarian) {
  
  names(lab_results) <- toupper(names(lab_results))
  names(numeric_soarian) <- toupper(names(numeric_soarian))
  
  labs_soarian <- lab_results %>% 
    rename(variable = FINDINGABBREVIATION,
           timestamp =  RESULTDATETIME) %>% 
    mutate(variable = clean_var_name(variable)) %>% 
    mutate(variable = paste0("lab_", tolower(variable)))
  
  numeric_soarian <- numeric_soarian %>% 
    rename(numeric_value = NUMERIC_VALUE) %>% 
    select(-PATIENTVISIT_OID, 
           -OBSERVATIONSTATUS, -VALUE, 
           -MINVALUE, -MAXVALUE, -FINDINGDATATYPE,
           -ASSESSMENTSTATUS, -ENTEREDDT,
           -ORDERSASWRITTENTEXT, -OBJECTID,
           -ASS_OBJECTID, -ASSESSMENT_OID)
  
  numeric_soarian <- numeric_soarian %>% 
    mutate(FINDINGNAME = ifelse(is.na(FINDINGNAME), "sbp", FINDINGNAME)) %>% 
    mutate(numeric_value = ifelse(FINDINGNAME == "Temperature", numeric_value - 273.15, numeric_value)) %>% 
    mutate(numeric_value = ifelse(FINDINGNAME == "Temperature (c)" & numeric_value > 180, numeric_value - 273.15, numeric_value))
  
  numeric_soarian <- numeric_soarian %>% 
    rename(variable = FINDINGABBR) %>% 
    mutate(variable = clean_var_name(variable))
  
  # break soarian data into piecs -------------------------------------------
  
  # vital signs
  
  # keep most common vital signs
  vital_signs <- numeric_soarian %>% 
    filter(variable %in% c("SBPSYSTOLIC",
                           "SBPDIASTOLIC",
                           "SPulse",
                           "SRespirations",
                           "SO2Saturation",
                           "STemperature",
                           "SPainIntRest",
                           "SPainIntMove",
                           "SFIO2"))
  
  
  vital_signs <- vital_signs %>% 
    select(ENCOUNTER_NUM, variable, timestamp = OBS_CREATIONTIME, numeric_value) %>% 
    mutate(variable = paste0("vital_", tolower(variable)))%>% 
    arrange(ENCOUNTER_NUM, timestamp)
  
  # Initial shift assessment
  shift_assessment <- numeric_soarian %>% 
    filter(FORMUSAGE == "Initial Shift Assessment" |
             variable == "SRpFiO2b")
  
  # select top occuring shift assessment variables:
  shift_assessment <- shift_assessment %>% 
    filter(variable %in% c("SPNIntstyRest1",
                           "SPNInstyMov1",
                           "SRpO2LMin",
                           "SPNIntstyRest2",
                           "SPNInstyMov2",
                           "SCVHrtRate",
                           "SRpFiO2b"))
  
  shift_assessment <- shift_assessment %>% 
    select(ENCOUNTER_NUM, variable, timestamp = OBS_CREATIONTIME, numeric_value) %>% 
    mutate(variable = paste0("shift_assess_", tolower(variable))) %>% 
    arrange(ENCOUNTER_NUM, timestamp)
  
  # intake outake
  
  in_out <- numeric_soarian %>% 
    filter(FORMUSAGE == "Intake and Output")
  
  # select top occuring intake outake variables:
  in_out <- in_out %>% 
    filter(variable %in% c("SIVNormalSaline",
                           "AIVPB1",
                           "ACatheter",
                           "SOtherIntake",
                           "AOthOutput",
                           "SIV23and13",
                           "ATmsIncontinent"))
  in_out <- in_out %>% 
    select(ENCOUNTER_NUM, variable, timestamp = OBS_CREATIONTIME, numeric_value) %>% 
    mutate(variable = paste0("in_out_", tolower(variable))) %>% 
    arrange(ENCOUNTER_NUM, timestamp)
  
  
  # skin assessment (BRADEN SCALE)
  
  skin <- numeric_soarian %>% 
    filter(FORMUSAGE == "Skin Assessment" |
             (FORMUSAGE == "ADMISSION" & variable == "ABradenScore"))
  
  skin <- skin %>% 
    select(ENCOUNTER_NUM, variable, timestamp = OBS_CREATIONTIME, numeric_value) %>% 
    mutate(variable = paste0("skin_", tolower(variable))) %>% 
    arrange(ENCOUNTER_NUM, timestamp)
  
  # Diabetic Control
  
  # intake outake
  
  db <- numeric_soarian %>% 
    filter(FORMUSAGE == "Diabetic Protocol" |
             (FORMUSAGE == "ADMISSION" & variable == "Diabetic Protocol"))
  
  db <- db %>% 
    select(ENCOUNTER_NUM, variable, timestamp = OBS_CREATIONTIME, numeric_value) %>% 
    mutate(variable = paste0("diabetic_", tolower(variable))) %>% 
    arrange(ENCOUNTER_NUM, timestamp)
  
  
  # alcohol
  
  alcohol <- numeric_soarian %>% 
    filter(FORMUSAGE == "CIWA Assessment",
           variable == "SCIWAScore")
  
  alcohol <- alcohol %>% 
    select(ENCOUNTER_NUM, variable, timestamp = OBS_CREATIONTIME, numeric_value) %>% 
    mutate(variable = paste0("alcohol_", tolower(variable))) %>% 
    arrange(ENCOUNTER_NUM, timestamp)
  
  
  # recombine data
  numeric_variables <- vital_signs %>% 
    bind_rows(shift_assessment) %>% 
    bind_rows(in_out) %>% 
    bind_rows(skin) %>% 
    bind_rows(db) %>% 
    bind_rows(alcohol)
  
  
  
  # clean labs data ---------------------------------------------------------
  
  labs_soarian <- labs_soarian %>% 
    select(ENCOUNTER_NUM, variable, timestamp, numeric_value = NUMERIC_VALUE) %>% 
    ungroup()
  
  # remove labs that are no longer measured
  model_labs <- c("lab_abaso", "lab_abe", "lab_acet", 
                  "lab_aeos", "lab_agap", "lab_ahion", 
                  "lab_alact", "lab_alb", "lab_alp", 
                  "lab_alt", "lab_alymp", "lab_amono", 
                  "lab_amy", "lab_aneut", "lab_ao", 
                  "lab_apco2", "lab_aph", "lab_apo2", 
                  "lab_asa", "lab_ast", "lab_atco2", 
                  "lab_b12", "lab_bc", "lab_bnps", 
                  "lab_ca", "lab_cacra", "lab_cai", 
                  "lab_caicr", "lab_ck", "lab_cl", 
                  "lab_co2", "lab_cr", "lab_crp", 
                  "lab_esr1", "lab_etoh", "lab_fe", 
                  "lab_fer", "lab_glob", "lab_glpoc", 
                  "lab_glur", "lab_hba1", "lab_hct", 
                  "lab_hgb", "lab_ical", "lab_igab", 
                  "lab_ivsd", "lab_iwbcr", "lab_k", 
                  "lab_la", "lab_ld", "lab_lip", 
                  "lab_lvedd", "lab_lvesd", "lab_masa",
                  "lab_mch", "lab_mchc", "lab_mcv", 
                  "lab_metaa", "lab_mg", "lab_mpv",
                  "lab_mvsa", "lab_myela", "lab_na", 
                  "lab_orcai", "lab_orglu", "lab_orhc",
                  "lab_ork", "lab_orna", "lab_osm", 
                  "lab_ph", "lab_plt", "lab_po4", 
                  "lab_pwd", "lab_rbc", "lab_rdw", 
                  "lab_reta", "lab_rinr", "lab_rpt", 
                  "lab_rptt", "lab_sat", "lab_spg", 
                  "lab_tbil", "lab_tibc", "lab_tni",
                  "lab_tpr", "lab_tsh", "lab_ucl", 
                  "lab_uk", "lab_una", "lab_uosm", 
                  "lab_urea", "lab_uuro", "lab_vbe",
                  "lab_vhion", "lab_vlact", "lab_vpco2", 
                  "lab_vph", "lab_vpo2", "lab_vtco2")
  
  
  labs_soarian <- labs_soarian %>% 
    filter(variable %in% model_labs)
  # combine labs and soarian numeric data
  
  numeric_variables <- numeric_variables %>% 
    bind_rows(labs_soarian) %>% 
    arrange(ENCOUNTER_NUM, timestamp)
  
  return(numeric_variables)
}


clean_var_name <- function(x) {
  #x <- tolower(x) # set to lowercase
  x <- gsub( '[[:punct:]]', '', x) # punctuation
  x <- gsub( '[ ]+', '', x)# remove spaces
  x <- stringr::str_replace(x, '[-]+', '_') # remove spaces
  x <- stringr::str_replace(x, '\\+', '')
  x <- gsub( '[/]+', '_', x) # forward slashes
  x <- stringr::str_replace(x, '[[:space:][:blank:]]+', '')
  x <- gsub("\\)|\\(", "", x)# brackets
  x <- gsub("\\%", "", x) # remove %
  return(x)
}

is.nan.data.frame <- function(x) {
  do.call(cbind, lapply(x, is.nan))
}

normalization_minmax <- function(x, desc) {
  map2_dfc(x, desc, ~(.x - .y$q01)/(.y$q99 - .y$q01))
}

impute_mean <- function(x, desc) {
  map2_dfc(x, desc, ~ifelse(is.na(.x), (.y$q50 - .y$q01)/(.y$q99 - .y$q01), .x))
}

# add time since measured variable.

counter_function <- function(x) {
  x[1] <- -1
  x <- ifelse(is.na(x), 0, x)
  x <- cumsum(x)
  r <- sequence(rle(as.character(x))$lengths) - 1
  return(r)
}

trim <- function(x, desc) {
  map2_dfc(x, desc, ~ ifelse(.x > .y$q99, .y$q99,
                             ifelse( .x < .y$q01, .y$q01, .x)))
}
