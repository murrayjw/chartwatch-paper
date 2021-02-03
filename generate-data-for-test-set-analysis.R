
# CHARTwatch predictions --------------------------------------------------

load("/mnt/research/LKS-CHART/Projects/gim_ews_preassessment_project/data/model_predictions.Rda")
load("/mnt/research/LKS-CHART/Projects/gim_ews_preassessment_project/data/model_predictions_full.Rda")
test_model_predictions <- model_predictions
test_model_predictions_all <- model_predictions_full

test_chartwatch_predictions <- test_model_predictions %>%
  select(ENCOUNTER_NUM, timestamp,
         CHARTwatch_score = score,
         CHARTwatch_group = risk_group)

test_predictions <- test_model_predictions %>%
  select(ENCOUNTER_NUM, timestamp)

# Clinician predictions ---------------------------------------------------

clinician_predictions <- readr::read_csv('/mnt/research/LKS-CHART/Projects/gim_ews_preassessment_project/data/practitioner_predictions_outcomes.csv')

# Different ways to merge the clinician predictions...

test_clinician_predictions <- test_predictions %>% 
  left_join(clinician_predictions, by = c("ENCOUNTER_NUM", "timestamp")) %>%
  group_by(ENCOUNTER_NUM, timestamp) %>%
  
  # If any clinician makes a prediction, count as YES
  summarize(
    clinician_next48 = max(next48),
    clinician_ever = max(ever)
  ) %>%
  ungroup()


# HEWS and NEWS predictions -----------------------------------------------

load('/mnt/research/LKS-CHART/Projects/gim_ews_preassessment_project/data/data-analysis/hews-news-pred.Rda.R')
split_path <- function(x) {
  if (dirname(x)==x) {
    x
  } else {
    c(basename(x),split_path(dirname(x)))
  }
}
n <- names(all_hews_news)
for(i in n) {
  
  tmp <- all_hews_news[[i]]
  d <- split_path(i)[1]
  time <- paste(d, "15:00:00")
  tmp <- tmp %>% 
    mutate(timestamp = ymd_hms(time))
  all_hews_news[[i]] <- tmp
}

all_hews_news <- do.call(rbind, all_hews_news)
test_hews_news_predictions <- test_predictions %>%
  left_join(all_hews_news, by = c("ENCOUNTER_NUM", "timestamp")) %>%
  select(ENCOUNTER_NUM, timestamp,
         HEWS_score = HEWS,
         HEWS_group,
         NEWS_score = NEWS,
         NEWS_group)


# Get outcomes ------------------------------------------------------------




library(chartwatch)
library(keyring)
# Pull data from EDW
options("keyring_backend" = "file",
        stringsAsFactors = F)
keyring_unlock(keyring = "database",
               password = Sys.getenv("R_KEYRING_PASSWORD"))

con_edw <- chartwatch::connect_edw(
  username = keyring::key_get(service = "EDW",
                              username = "EDW_USERNAME",
                              keyring = "database"),
  password = keyring::key_get(service = "EDW",
                              username = "EDW_PASSWORD",
                              keyring = "database"))

# Potential bugs
# Palliative transfer should include "TRANSITION"
all_test_outcomes <- chartwatch::get_outcome_events(con_edw, 
                                                encounter_vector = unique(test_predictions$ENCOUNTER_NUM))


test_outcomes <- test_predictions %>%
  mutate(ENCOUNTER_NUM = as.character(ENCOUNTER_NUM)) %>%
  left_join(all_test_outcomes, by = "ENCOUNTER_NUM") %>%
  filter(!is.na(EVENT_TS)) %>%
  filter(EVENT_TS >= timestamp) %>%
  group_by(ENCOUNTER_NUM, timestamp) %>%
  
  arrange(EVENT_TS) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(ENCOUNTER_NUM = as.integer(ENCOUNTER_NUM)) %>%
  
  select(ENCOUNTER_NUM, timestamp,
         outcome_ts = EVENT_TS,
         outcome_type = EVENT_TYPE)

test_outcomes_clinicians <- clinician_predictions %>%
  select(ENCOUNTER_NUM, timestamp, death, death_ts, icu, icu_ts, pal, pal_ts,
         contains("outcome"), contains("OUTCOME")) %>%
  unique()


combined_test_predictions <- test_predictions %>%
  left_join(test_outcomes_clinicians, by = c("ENCOUNTER_NUM", "timestamp")) %>% 
  left_join(test_chartwatch_predictions, by = c("ENCOUNTER_NUM", "timestamp")) %>%
  left_join(test_hews_news_predictions, by = c("ENCOUNTER_NUM", "timestamp")) %>%
  left_join(test_clinician_predictions, by = c("ENCOUNTER_NUM", "timestamp")) %>%
  select(ENCOUNTER_NUM, timestamp,
         outcome, outcome48, 
         contains("score"), 
         contains("group"),
         contains("clinician"))  %>%
  mutate(outcome = if_else(is.na(outcome), 0, outcome),
         outcome48 = if_else(is.na(outcome48), 0, outcome48))

get_metrics <- function(df, alarm_group, class_label) {
  tp <- df %>% filter(group == alarm_group, class_label == 1)
  fp <- df %>% filter(group == alarm_group, class_label == 0)
  tn <- df %>% filter(group != alarm_group, class_label == 0)
  fn <- df %>% filter(group != alarm_group, class_label == 1)
  
  tibble(
    "ppv" = nrow(tp) / (nrow(tp) + nrow(fp)),
    "sensitivity" = nrow(tp) / (nrow(tp) + nrow(fn))
  )
  
}

compute_results <- function(df, alarm_group) {
  tp <- df %>% filter(group == alarm_group, outcome == 1)
  fp <- df %>% filter(group == alarm_group, outcome == 0)
  tn <- df %>% filter(group != alarm_group, outcome == 0)
  fn <- df %>% filter(group != alarm_group, outcome == 1)
  
  df_encounter_level <- df %>%
    group_by(ENCOUNTER_NUM) %>%
    summarize(outcome = max(outcome),
              group = if_else(any(group == alarm_group), alarm_group, ""))
  
  metrics_ever_ <- get_metrics(df %>% mutate(class_label = outcome), alarm_group)
  metrics_48 <- get_metrics(df %>% mutate(class_label = outcome48), alarm_group)
  metrics_ever <- get_metrics(df_encounter_level %>% mutate(class_label = outcome), alarm_group)
  
  
  tibble(
    "auc_48" = pROC::auc(df$score, response = df$outcome),
    "auc_ever" = pROC::auc(df$score, response = df$outcome48),
    "sensitivity_ever" = metrics_ever$sensitivity,
    "ppv_ever" = metrics_ever$ppv,
    #"sensitivity_ever_" = metrics_ever_$sensitivity,
    #"ppv_ever_" = metrics_ever_$ppv,
    "sensitivity_48" = metrics_48$sensitivity,
    "ppv_48" = metrics_48$ppv
  )
}

compute_results(combined_test_predictions %>%
                  mutate(score = CHARTwatch_score,
                         group = CHARTwatch_group),
                alarm_group = "High")

compute_results(combined_test_predictions %>%
                  mutate(score = HEWS_score,
                         group = HEWS_group),
                alarm_group = "very high risk")

compute_results(combined_test_predictions %>%
                  mutate(score = NEWS_score,
                         group = NEWS_group),
                alarm_group = "very high risk")


pROC::roc(combined_test_predictions$CHARTwatch_score,
          response = combined_test_predictions$outcome, plot=F, ci=TRUE)

pROC::roc(combined_test_predictions$CHARTwatch_score, 
          response = combined_test_predictions$outcome48, plot=F, ci=TRUE)

pROC::roc(combined_test_predictions$HEWS_score, 
          response = combined_test_predictions$outcome48, plot=F, ci=TRUE)

pROC::auc(combined_test_predictions$clinician_next48, 
          response = combined_test_predictions$outcome48)
