#load('Z:\\LKS-CHART\\Projects\\gim_ews_preassessment_project\\data\\data-analysis\\hews-news-pred.Rda')
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
  distinct(ENCOUNTER_NUM, timestamp, death_ever, death_48, icu_ever, icu_48,
           pal_ever, pal_48, icu_time, death_time, pal_time, outcome_ever,
           outcome_48)


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

roc_obj <- roc(all_hews_news$outcome_ever, all_hews_news$NEWS)
auc(roc_obj)

tab <- table(all_hews_news$NEWS_group %in% c('very high risk',
                                             'high risk'), all_hews_news$outcome_ever)
tab[2,2]/(tab[2,2] + tab[1,2]) # sens
tab[2,2]/(tab[2,2] + tab[2,1]) # ppv

roc_obj <- roc(all_hews_news$outcome_ever, all_hews_news$HEWS)
auc(roc_obj)

tab <- table(all_hews_news$HEWS_group %in% c('very high risk',
                                            'high risk'), all_hews_news$outcome_ever)
tab[2,2]/(tab[2,2] + tab[1,2]) # sens
tab[2,2]/(tab[2,2] + tab[2,1]) # ppv

all_hews_news %>% 
  filter(NEWS_group %in% c('very high risk',
                           'high risk')& outcome_ever == 1) %>% 
  mutate(outcome_time = if_else(is.na(icu_time), death_time, icu_time)) %>% 
  mutate(tte = as.numeric(difftime(outcome_time,
                                   timestamp, 
                                   units = 'hours'))) %>% 
  summarize(med_tte = median(tte)/24,
            q25 = quantile(tte, .25)/24,
            q75 = quantile(tte, .75)/24)
