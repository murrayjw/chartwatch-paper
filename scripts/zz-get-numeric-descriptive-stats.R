#'  ***********************************************************************
#'  ***********************************************************************
#'  ***********************************************************************
#'  Script name:
#'  Initially created by: 
#'  Date created: Feb 24, 2021
#'  Maintainer information: 
#'
#'  Script contents: Generate numeric descriptive statistics. After looking
#'  at generated numeric descriptive stats, concluded that it's the same one
#'  as the one that's currently used in deployed data --> no need to update
#'  
#'
#'  ***********************************************************************
#'  ***********************************************************************
#'  ***********************************************************************

library(dplyr)
source("constants.R")

load(MARS_RECIPE_FILENAME)
load(MARS_MODEL_FILENAME)

train_data <- final_mars_recipe$template
train_encounter_nums <- unique(train_data$ENCOUNTER_NUM)
soarian_train_encounter_nums <- paste0("00", train_encounter_nums)

load(DESCRIPTIVE_STATS_FILE)

# Load numeric data
load(LAB_RESULTS_FILENAME)
lab_results <- lab_results %>%
  dplyr::filter(ENCOUNTER_NUM %in% soarian_train_encounter_nums)
load(NUMERIC_SOARIAN_FILENAME)
numeric_soarian <- numeric_soarian %>%
  dplyr::filter(ENCOUNTER_NUM %in% soarian_train_encounter_nums)

processed_labs <- chartwatch::process_labs_data(lab_results)
processed_vitals <- chartwatch::process_vitals_data(numeric_soarian, 
                                                    # BP data already split into 
                                                    # systolic and diastolic
                                                    process_bp = FALSE)

# Load transformations file
transforms_df <- read.csv(TRANSFORMATIONS_FILE, stringsAsFactors = FALSE)

updated_descriptive_stats <- chartwatch::get_numeric_descriptive_statistics(labs_df = processed_labs,
                                                                            vitals_df = processed_vitals,
                                                                            transformations_df = transforms_df)

# Compare old numeric descriptive stats file with updated one
for (var in names(updated_descriptive_stats)) {
  
  curr_var <- updated_descriptive_stats[[var]]
  old_var <- updated_descriptive_stats[[var]]
  
  print(abs(curr_var$mean - old_var$mean))
}

# turns out they're exactly the same!
