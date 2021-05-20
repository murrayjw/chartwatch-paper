#'  ***********************************************************************
#'  ***********************************************************************
#'  ***********************************************************************
#'  Script name:
#'  Initially created by: 
#'  Date created: Feb 25, 2021
#'  Maintainer information: 
#'
#'  Script contents: Update train/valid encounters. After re-training,
#'  realized that the MARS model was actually trained on a broader dataset
#'
#'  ***********************************************************************
#'  ***********************************************************************
#'  ***********************************************************************
#'  
#'  

source("constants.R")

load(MARS_RECIPE_FILENAME)


# Get updated encounter nums ----------------------------------------------

train_data <- final_mars_recipe$template

train_encounter_nums <- unique(train_data$ENCOUNTER_NUM)
train_encounters <- read.csv(file.path(MODEL_TRAINING_DATA_FOLDER, "encounters.csv"), 
                             stringsAsFactors = FALSE) %>%
  dplyr::filter(ENCOUNTER_NUM %in% train_encounter_nums)

valid_encounters <- read.csv(PAPER_VALID_ENCOUNTERS_FILENAME, 
                             stringsAsFactors = FALSE)

valid_encounter_nums <- valid_encounters %>%
  dplyr::filter(!ENCOUNTER_NUM %in% train_encounter_nums) %>%
  dplyr::pull(ENCOUNTER_NUM)


# Update encounters inpatient info ----------------------------------------

split_df <- function(df, train_encounter_nums, valid_encounter_nums) {
  updated_df <- list(
    train = df %>%
      dplyr::filter(ENCOUNTER_NUM %in% train_encounter_nums),
    valid = df %>%
      dplyr::filter(ENCOUNTER_NUM %in% valid_encounter_nums)
  )
  return(updated_df)
}

encounters <- rbind(train_encounters, valid_encounters %>% select(-X))

encounters_data <- rbind(
  read.csv(
    "/mnt/research/LKS-CHART/Projects/gim_ews_project/data/paper-data/train_encounters_data.csv",
    stringsAsFactors = FALSE),
  read.csv(
    "/mnt/research/LKS-CHART/Projects/gim_ews_project/data/paper-data/valid_encounters_data_method1.csv",
    stringsAsFactors = FALSE
  )
)

vitals_data <- rbind(
  read.csv(
    "/mnt/research/LKS-CHART/Projects/gim_ews_project/data/paper-data/train_vitals_data.csv",
    stringsAsFactors = FALSE
  ),
  read.csv(
    "/mnt/research/LKS-CHART/Projects/gim_ews_project/data/paper-data/valid_vitals_data_method1.csv",
    stringsAsFactors = FALSE
  )
)

split_encounters <- split_df(encounters, train_encounter_nums, valid_encounter_nums)
split_encounters_data <- split_df(encounters_data, train_encounter_nums, valid_encounter_nums)
split_vitals_data <- split_df(vitals_data, train_encounter_nums, valid_encounter_nums)


# Save files --------------------------------------------------------------

write.csv(split_encounters$train, file = FINAL_PAPER_TRAIN_ENCOUNTERS_FILENAME,
          row.names = FALSE)
write.csv(split_encounters$valid, file = FINAL_PAPER_VALID_ENCOUNTERS_FILENAME,
          row.names = FALSE)
write.csv(split_encounters_data$train, file = FINAL_PAPER_TRAIN_ENCOUNTERS_DATA_FILENAME,
          row.names = FALSE)
write.csv(split_encounters_data$valid, file = FINAL_PAPER_VALID_ENCOUNTERS_DATA_FILENAME,
          row.names = FALSE)
write.csv(split_vitals_data$train, file = FINAL_PAPER_TRAIN_VITALS_DATA_FILENAME,
          row.names = FALSE)
write.csv(split_vitals_data$valid, file = FINAL_PAPER_VALID_VITALS_DATA_FILENAME,
          row.names = FALSE)
