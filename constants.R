LOS_CUTOFF <- 30

DESCRIPTIVE_STATS_FILE <- "/mnt/research/DSAA_Deployment/chartwatch_project/model_objects/numeric_descriptive_statistics.R" 
TRANSFORMATIONS_FILE <- "/mnt/research/DSAA_Deployment/chartwatch_project/model_objects/numeric_variable_names.csv"

ALL_MARS_SCORES_FILENAME <- "/mnt/research/LKS-CHART/Projects/gim_ews_project/data/paper-data/all_mars_scores_2021_0222.csv"

PAPER_TRAIN_ENCOUNTERS_FILENAME <- "/mnt/research/LKS-CHART/Projects/gim_ews_project/data/paper-data/train_encounters.csv"
PAPER_VALID_ENCOUNTERS_FILENAME <- "/mnt/research/LKS-CHART/Projects/gim_ews_project/data/paper-data/valid_encounters_method2.csv"
PAPER_TEST_ENCOUNTERS_FILENAME <- "/mnt/research/LKS-CHART/Projects/gim_ews_project/data/paper-data/test_encounters_with_outcomes_method2.csv"



# Model data files --------------------------------------------------------

MARS_MODEL_FILENAME <- "/mnt/research/LKS-CHART/Projects/gim_ews_project/data/paper-data/final_mars_model_paper.R"
MARS_RECIPE_FILENAME <- "/mnt/research/LKS-CHART/Projects/gim_ews_project/data/paper-data/final_mars_recipe.R"
ENSEMBLE_MARS_MODEL_FILENAME <- "/mnt/research/LKS-CHART/Projects/gim_ews_project/data/paper-data/ensemble_mars_model_paper.Rds"
ENSEMBLE_MARS_RECIPE_FILENAME <- "/mnt/research/DSAA_Deployment/chartwatch_project/model_objects/ensemble_time_aware_mars/logistic-ensemble_recipe2020_0505_formula_mars_pcts_baseline_interaction.Rds"

LAB_RESULTS_FILENAME <- "/mnt/research/LKS-CHART/Projects/gim_ews_project/data/FULL-RAW-DATA/lab_results.Rda"
NUMERIC_SOARIAN_FILENAME <- "/mnt/research/LKS-CHART/Projects/gim_ews_project/data/FULL-RAW-DATA/numeric_soarian.Rda"



# Final data files, used for descriptive tables ---------------------------

FINAL_DATA_DIR <- "/mnt/research/LKS-CHART/Projects/gim_ews_project/data/paper-data/"

FINAL_PAPER_TRAIN_ENCOUNTERS_FILENAME <- file.path(FINAL_DATA_DIR, "paper_train_encounters.csv")
FINAL_PAPER_VALID_ENCOUNTERS_FILENAME <- file.path(FINAL_DATA_DIR, "paper_valid_encounters.csv")
FINAL_PAPER_TEST_ENCOUNTERS_FILENAME <- PAPER_TEST_ENCOUNTERS_FILENAME

FINAL_PAPER_TRAIN_ENCOUNTERS_DATA_FILENAME <- file.path(FINAL_DATA_DIR, "paper_train_encounters_data.csv")
FINAL_PAPER_VALID_ENCOUNTERS_DATA_FILENAME <- file.path(FINAL_DATA_DIR, "paper_valid_encounters_data.csv")

FINAL_PAPER_TRAIN_VITALS_DATA_FILENAME <- file.path(FINAL_DATA_DIR, "paper_train_vitals_data.csv")
FINAL_PAPER_VALID_VITALS_DATA_FILENAME <- file.path(FINAL_DATA_DIR, "paper_valid_vitals_data.csv")

FINAL_PAPER_TRAIN_TIME_AWARE_PREDICTIONS_FILENAME <- file.path(FINAL_DATA_DIR, "paper_train_time_aware_predictions.csv")
FINAL_PAPER_VALID_TIME_AWARE_PREDICTIONS_FILENAME <- file.path(FINAL_DATA_DIR, "paper_valid_time_aware_predictions.csv")
FINAL_PAPER_TEST_TIME_AWARE_PREDICTIONS_FILENAME <- file.path(FINAL_DATA_DIR, "paper_test_time_aware_predictions.csv")

FINAL_PAPER_TRAIN_ALL_METRICS_FILENAME <- file.path(FINAL_DATA_DIR, 
                                                    "paper_train_all_metrics.csv")
FINAL_PAPER_VALID_ALL_METRICS_FILENAME <- file.path(FINAL_DATA_DIR,
                                                    "paper_valid_all_metrics.csv")
FINAL_PAPER_TEST_ALL_METRICS_FILENAME <- file.path(FINAL_DATA_DIR,
                                                   "paper_test_all_metrics.csv")

FINAL_PAPER_TEST_CLINICIAN_PREDICTIONS_FILENAME <- "/mnt/research/LKS-CHART/Projects/gim_ews_preassessment_project/data/practitioner_predictions_outcomes.csv"
# Generated from get-thresholds.R file
FINAL_PAPER_ENSEMBLE_THRESHOLD <- 0.17
