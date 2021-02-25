

# load the require libraries ----------------------------------------------
library(pROC)
library(tidyverse)

prediction_outcomes <- readr::read_csv('/mnt/research/LKS-CHART/Projects/gim_ews_preassessment_project/data/practitioner_predictions_outcomes.csv')
load("/mnt/research/LKS-CHART/Projects/gim_ews_preassessment_project/data/model_predictions.Rda")

phys_enc <- unique(prediction_outcomes$ENCOUNTER_NUM)
mod_enc <- unique(model_predictions$ENCOUNTER_NUM)

sd <- setdiff(phys_enc, mod_enc)

clinician <- prediction_outcomes %>% 
  filter(!ENCOUNTER_NUM %in% sd)

distinct_outcomes <- prediction_outcomes %>% 
  distinct(ENCOUNTER_NUM, timestamp, OUTCOME, outcome48)

model <- model_predictions %>% 
  inner_join(distinct_outcomes, by = c("timestamp", "ENCOUNTER_NUM"))


clinician_sum <- clinician %>% 
  group_by(ENCOUNTER_NUM, timestamp) %>% 
  summarize(mp = max(ever),
            mp48 = max(next48),
            outcome = max(outcome),
            outcome_48 = max(outcome48)) %>% 
  ungroup()

model <- model %>% 
  left_join(clinician_sum, by = c("ENCOUNTER_NUM", "timestamp"))


roc_obj <- roc(clinician_sum$outcome_48, clinician_sum$mp48)
roc_obj <- roc(clinician_sum$outcome, clinician_sum$mp)

# all clinicians
roc_obj <- roc(clinician$ever, clinician$outcome)
auc(roc_obj)

# all clinicians
roc_obj <- roc( model_predictions$OUTCOME,  model_predictions$last_score)
auc(roc_obj)


prediction_outcomes_sum <- prediction_outcomes %>% 
  group_by(ENCOUNTER_NUM, timestamp) %>% 
  summarize(ever = max(ever),
            next48 = max(next48),
            OUTCOME = max(OUTCOME),
            outcome48 = max(outcome48)) %>% 
  ungroup()

roc_obj <- roc( prediction_outcomes_sum$OUTCOME, prediction_outcomes_sum$ever)
auc(roc_obj)

table(prediction_outcomes_sum$OUTCOME, prediction_outcomes_sum$ever)

table(prediction_outcomes_sum$OUTCOME, prediction_outcomes_sum$ever)


clinician_performance <- prediction_outcomes %>% 
  group_by(clinicianid, timestamp) %>% 
  summarize(ever = max(ever),
            next48 = max(next48),
            OUTCOME = max(OUTCOME),
            outcome48 = max(outcome48)) %>% 
  ungroup()



# unexpected death
prediction_outcomes <- prediction_outcomes %>% 
  mutate(unexpected = ifelse((death_ever == 1 & palliative == 1) , 0, outcome_ever))

prediction_outcomes <- prediction_outcomes %>% 
  mutate(any_ever = ifelse(death_ever == 1 | icu_ever == 1 |pal_ever == 1, 1, 0))


tab <- table(prediction_outcomes$ever, prediction_outcomes$outcome)
tab[2,2]/(tab[2,2] + tab[1,2]) # sens
tab[2,2]/(tab[2,2] + tab[2,1]) # ppv


prediction_outcomes %>% 
  filter(ever == 1 & outcome_ever == 1) %>% 
  mutate(outcome_time = if_else(is.na(icu_time), death_time, icu_time)) %>% 
  mutate(tte = as.numeric(difftime(outcome_time,
                                   timestamp, 
                                   units = 'hours'))) %>% 
    summarize(med_tte = median(tte)/24,
              q25 = quantile(tte, .25)/24,
              q75 = quantile(tte, .75)/24)


# physician AUC
physician <- prediction_outcomes %>% 
  filter(grepl("Physician|Pyysician|Phsycian", professional_role) &
           grepl("Team", professional_role))

roc_obj <- roc(physician$outcome_ever, physician$ever)
auc(roc_obj)

tab <- table(physician$ever, physician$outcome_ever)
tab[2,2]/(tab[2,2] + tab[1,2]) # sens
tab[2,2]/(tab[2,2] + tab[2,1]) # ppv
physician %>% 
  filter(ever == 1 & outcome_ever == 1) %>% 
  mutate(outcome_time = if_else(is.na(icu_time), death_time, icu_time)) %>% 
  mutate(tte = as.numeric(difftime(outcome_time,
                                   timestamp, 
                                   units = 'hours'))) %>% 
  summarize(med_tte = median(tte)/24,
            q25 = quantile(tte, .25)/24,
            q75 = quantile(tte, .75)/24)



# resident AUC
resident <- prediction_outcomes %>% 
  filter(grepl("Resident", professional_role))

roc_obj <- roc(resident$outcome_ever, resident$ever)
auc(roc_obj)

tab <- table(resident$ever, resident$outcome_ever)
tab[2,2]/(tab[2,2] + tab[1,2]) # sens
tab[2,2]/(tab[2,2] + tab[2,1]) # ppv
resident %>% 
  filter(ever == 1 & outcome_ever == 1) %>% 
  mutate(outcome_time = if_else(is.na(icu_time), death_time, icu_time)) %>% 
  mutate(tte = as.numeric(difftime(outcome_time,
                                   timestamp, 
                                   units = 'hours'))) %>% 
  summarize(med_tte = median(tte)/24,
            q25 = quantile(tte, .25)/24,
            q75 = quantile(tte, .75)/24)

## all MD
resident <- prediction_outcomes %>% 
  filter(professional_role != "Nurse")

roc_obj <- roc(resident$outcome_ever, resident$ever)
auc(roc_obj)

tab <- table(resident$ever, resident$outcome_ever)
tab[2,2]/(tab[2,2] + tab[1,2]) # sens
tab[2,2]/(tab[2,2] + tab[2,1]) # ppv
resident %>% 
  filter(ever == 1 & outcome_ever == 1) %>% 
  mutate(outcome_time = if_else(is.na(icu_time), death_time, icu_time)) %>% 
  mutate(tte = as.numeric(difftime(outcome_time,
                                   timestamp, 
                                   units = 'hours'))) %>% 
  summarize(med_tte = median(tte)/24,
            q25 = quantile(tte, .25)/24,
            q75 = quantile(tte, .75)/24)

# staff MD
staff <- prediction_outcomes %>% 
  filter(professional_role == "Physician")

roc_obj <- roc(staff$outcome_ever, staff$ever)
auc(roc_obj)

staff %>% 
  filter(ever == 1 & outcome_ever == 1) %>% 
  mutate(outcome_time = if_else(is.na(icu_time), death_time, icu_time)) %>% 
  mutate(tte = as.numeric(difftime(outcome_time,
                                   timestamp, 
                                   units = 'hours'))) %>% 
  summarize(med_tte = median(tte)/24,
            q25 = quantile(tte, .25)/24,
            q75 = quantile(tte, .75)/24)
# nurses 

nurse <- prediction_outcomes %>% 
  filter(professional_role == "Nurse")

roc_obj <- roc(nurse$outcome_ever, nurse$ever)
auc(roc_obj)

tab <- table(nurse$ever, nurse$outcome_ever)
tab[2,2]/(tab[2,2] + tab[1,2]) # sens
tab[2,2]/(tab[2,2] + tab[2,1]) # ppv
nurse %>% 
  filter(ever == 1 & outcome_ever == 1) %>% 
  mutate(outcome_time = if_else(is.na(icu_time), death_time, icu_time)) %>% 
  mutate(tte = as.numeric(difftime(outcome_time,
                                   timestamp, 
                                   units = 'hours'))) %>% 
  summarize(med_tte = median(tte)/24,
            q25 = quantile(tte, .25)/24,
            q75 = quantile(tte, .75)/24)
  

phys_per_pred <- table(physician$ever, physician$unexpected)



phys_per_pred <- table(resident$ever, resident$outcome_ever)



phys_per_pred <- table(nurse$ever, nurse$outcome_ever)

roc_obj <- roc(nurse$outcome_ever, nurse$ever)
auc(roc_obj)

roc_obj <- roc(resident$outcome_ever, resident$ever)
auc(roc_obj)


roc_obj <- roc(physician$outcome_ever, physician$ever)
auc(roc_obj)


physician_sum <- physician %>% 
  group_by(ENCOUNTER_NUM) %>% 
  summarize(me = max(ever),
            mo = max(outcome_ever))

resident_sum <- resident %>% 
  group_by(ENCOUNTER_NUM) %>% 
  summarize(me = max(ever),
            mo = max(outcome_ever))

nurse_sum <- nurse %>% 
  group_by(ENCOUNTER_NUM) %>% 
  summarize(me = max(ever),
            mo = max(outcome_ever))

table(physician_sum$me, physician_sum$mo)
table(resident_sum$me, resident_sum$mo)
table(nurse_sum$me, nurse_sum$mo)




model
table(clinician_sum$mp, clinician_sum$outcome)
table(model$score > .16, model$outcome)

roc_obj <- roc(model$outcome_48, model$last_score)
auc(roc_obj)


roc_obj <- roc(clinician_sum$outcome_48, clinician_sum$next48)

auc(roc_obj)

