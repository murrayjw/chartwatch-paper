#'  ***********************************************************************
#'  ***********************************************************************
#'  ***********************************************************************
#'  Script name:
#'  Initially created by: 
#'  Date created: Mar 2, 2021
#'  Maintainer information: 
#'
#'  Script contents: Load all data in training folder and filter so that 
#'  we only keep encounter numbers that are actually used in the final
#'  paper training data cohort. Save all "new" train files in a way that 
#'  the files can later be used for re-training.
#'  
#'  These files will be used to re-train the GRU models.
#'
#'  ***********************************************************************
#'  ***********************************************************************
#'  ***********************************************************************

library(dplyr)
source("constants.R")


# Update train data -------------------------------------------------------

train_encounters <- read.csv(FINAL_PAPER_TRAIN_ENCOUNTERS_FILENAME, 
                             stringsAsFactors = FALSE)
train_encounter_nums <- unique(train_encounters$ENCOUNTER_NUM)
length(train_encounter_nums) # should be 22, 552


valid_encounters <- read.csv(FINAL_PAPER_VALID_ENCOUNTERS_FILENAME,
                             stringsAsFactors = FALSE)
valid_encounter_nums <- unique(valid_encounters$ENCOUNTER_NUM)


for (filename in list.files(MODEL_TRAINING_DATA_FOLDER)) {

  print(paste0("Loading ", filename))  
  data <- read.csv(file.path(MODEL_TRAINING_DATA_FOLDER,
                             filename), stringsAsFactors = FALSE)
  #train_data <- data %>%
  #  dplyr::filter(ENCOUNTER_NUM %in% train_encounter_nums)
  valid_data <- data %>%
    dplyr::filter(ENCOUNTER_NUM %in% valid_encounter_nums)
  
  #write.csv(train_data,
  #          file = file.path(FINAL_MODEL_TRAINING_DATA_FOLDER, filename),
  #          row.names = FALSE)
  write.csv(valid_data,
            file = file.path(FINAL_MODEL_VALIDATION_DATA_FOLDER, filename),
            row.names = FALSE)
}


# Update validation and test data -----------------------------------------

test_encounters <- read.csv(FINAL_PAPER_TEST_ENCOUNTERS_FILENAME,
                            stringsAsFactors = FALSE)

prediction_frames_folder <- "/mnt/research/LKS-CHART/Projects/gim_ews_preassessment_project/data/prediction_frames/"

load(file.path(prediction_frames_folder, "gim_census.Rda"))
