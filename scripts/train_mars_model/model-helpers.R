#'  ***********************************************************************
#'  ***********************************************************************
#'  ***********************************************************************
#'  Script name:
#'  Initially created by: 
#'  Date created: 
#'  Maintainer information: 
#'
#'  Script contents: used to train the MARS model. run on the Lambda-Blade
#'  server on November 2020
#'
#'  ***********************************************************************
#'  ***********************************************************************
#'  ***********************************************************************

load_data <- function(type = "train",
                      window = 2,
                      topic_num = 50) {
  
  path <- glue("/data/gim_ews_model_data/model_data_{window}hours/{type}/")
  encounter_path <- "/data/gim_ews_data/raw-data-extraction/encounters/"
  
  encounters <- fread(glue(encounter_path, "{type}_encounters.csv"))
  
  numeric <- fread(glue(path, "{type}_numeric_timeseries_{window}hr.csv"))
  orders <-  fread(glue(path,
                        "{type}_clinical_orders_on_off_timeseries_{window}hr.csv"))
  medications <- fread(glue(path,
                            "{type}_medication_admin_timeseries_{window}hr.csv"))
  
  baseline <- fread(glue(path,
                         "{type}_baseline_values_{window}hr.csv"))
  topics <- fread(glue(path,
                       "{type}_topic_probabilities_{topic_num}_topics.csv"))
  
  demographics <- fread(glue(path,
                             "{type}_demographics_{window}hr.csv"))
  
  outcomes <- fread(glue(path,
                         "{type}_outcome_timeseries_{window}hr.csv")) 
  
  return_list <- list(encounters = encounters,
                      demographics = demographics,
                      baseline = baseline,
                      numeric = numeric,
                      orders = orders,
                      medications = medications,
                      topics = topics,
                      outcomes = outcomes)
  return(return_list)
}

prepare_data <- function(model_data, 
                         input_w = 8) {
  
  all_outcomes <- grep("outcome", names(model_data), value = T)
  
  
  actual_measures <- names(model_data)
  actual_measures <- actual_measures[grepl("lab|shift_assess|in_out|vital", actual_measures)]
  actual_measures <- actual_measures[!grepl("baseline", actual_measures)]
  actual_measures <- actual_measures[!grepl("measure", actual_measures)]
  actual_measures <- actual_measures[!grepl("outcome", actual_measures)]
  
  for(i in actual_measures) {
    diff1 <- paste0("diff1_", i)
    diff2 <- paste0("diff2_", i)
    i <- rlang::sym(i)
    
    if(input_w == 2) {
      model_data <- model_data %>% 
        group_by(ENCOUNTER_NUM) %>% 
        arrange(time_window) %>% 
        mutate(!!diff1 := !!i - lag(!!i, 3),
               !!diff1 := ifelse(is.na(!!rlang::sym(diff1)),
                                 0, !!rlang::sym(diff1)),
               !!diff2 := !!i - lag(!!i, 6),
               
               !!diff2 := ifelse(is.na(!!rlang::sym(diff2)),
                                 0, !!rlang::sym(diff2)))
    } else if(input_w == 4) {
      model_data <- model_data %>% 
        group_by(ENCOUNTER_NUM) %>% 
        arrange(time_window) %>% 
        mutate(!!diff1 := !!i - lag(!!i, 2),
               !!diff1 := ifelse(is.na(!!rlang::sym(diff1)),
                                 0, !!rlang::sym(diff1)),
               !!diff2 := !!i - lag(!!i, 4),
               
               !!diff2 := ifelse(is.na(!!rlang::sym(diff2)),
                                 0, !!rlang::sym(diff2)))
      
      
      print(i)
    } else if(input_w == 6) {
      model_data <- model_data %>% 
        group_by(ENCOUNTER_NUM) %>% 
        arrange(time_window) %>% 
        mutate(!!diff1 := !!i - lag(!!i, 1),
               !!diff1 := ifelse(is.na(!!rlang::sym(diff1)),
                                 0, !!rlang::sym(diff1)),
               !!diff2 := !!i - lag(!!i, 2),
               
               !!diff2 := ifelse(is.na(!!rlang::sym(diff2)),
                                 0, !!rlang::sym(diff2)))
      
      
      print(i)
    } else if(input_w == 8) {
      model_data <- model_data %>% 
        group_by(ENCOUNTER_NUM) %>% 
        arrange(time_window) %>% 
        mutate(!!diff1 := !!i - lag(!!i, 1),
               !!diff1 := ifelse(is.na(!!rlang::sym(diff1)),
                                 0, !!rlang::sym(diff1)),
               !!diff2 := !!i - lag(!!i, 2),
               
               !!diff2 := ifelse(is.na(!!rlang::sym(diff2)),
                                 0, !!rlang::sym(diff2)))
      
      
      print(i)
    }
    
  }
  
  return(model_data)
}

sample_data <- function(model_data, outcome_var, ratio_to_one = 15, seed = 1313) {
  
  set.seed(seed)
  
  yes <- which(model_data[[outcome_var]] == 1)
  no <- which(model_data[[outcome_var]] == 0)
  
  size <- ceiling(length(yes)*ratio_to_one)
  sample_no <- sample(no, size = size, replace = F)
  rows <- c(yes, sample_no)
  
  
  return_data <- model_data %>% 
    slice(rows)
  return(return_data)
  
}
