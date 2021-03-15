#load('Z:\\LKS-CHART\\Projects\\gim_ews_preassessment_project\\data\\data-analysis\\hews-news-pred.Rda')
library(dplyr)
library(lubridate)
library(pROC)
load('/mnt/research/LKS-CHART/Projects/gim_ews_preassessment_project/data/data-analysis/hews-news-pred.Rda.R')

split_path <- function(x) {
  if (dirname(x)==x) {
    x
  } else {
    c(basename(x),split_path(dirname(x)))
  }
}


#ap <- readr::read_csv('Z:\\LKS-CHART\\Projects\\gim_ews_preassessment_project\\data\\practitioner_predictions_outcomes.csv')
ap <- readr::read_csv('/mnt/research/LKS-CHART/Projects/gim_ews_preassessment_project/data/practitioner_predictions_outcomes.csv')


ap <- ap %>% 
  distinct(ENCOUNTER_NUM, timestamp, death, icu, 
           pal, icu_ts, death_ts, pal_ts, outcome,
           outcome48)


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

all_hews_news <- ap %>% left_join(all_hews_news)
all_hews_news <- all_hews_news %>% 
  filter(!is.na(NEWS))

roc_obj <- roc(all_hews_news$outcome, all_hews_news$NEWS)
auc(roc_obj)

tab <- table(all_hews_news$NEWS_group %in% c('very high risk',
                                             'high risk'), all_hews_news$outcome)
tab[2,2]/(tab[2,2] + tab[1,2]) # sens
tab[2,2]/(tab[2,2] + tab[2,1]) # ppv

roc_obj <- roc(all_hews_news$outcome, all_hews_news$HEWS)
auc(roc_obj)

tab <- table(all_hews_news$HEWS_group %in% c('very high risk',
                                            'high risk'), all_hews_news$outcome)
tab[2,2]/(tab[2,2] + tab[1,2]) # sens
tab[2,2]/(tab[2,2] + tab[2,1]) # ppv

all_hews_news %>% 
  filter(NEWS_group %in% c('very high risk',
                           'high risk')& outcome == 1) %>% 
  mutate(outcome_time = if_else(is.na(icu_ts), death_ts, icu_ts)) %>% 
  mutate(tte = as.numeric(difftime(outcome_time,
                                   timestamp, 
                                   units = 'hours'))) %>% 
  summarize(med_tte = median(tte, na.rm = T)/24,
            q25 = quantile(tte, .25, na.rm = T)/24,
            q75 = quantile(tte, .75, na.rm = T)/24)
