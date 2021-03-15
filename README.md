
<!-- README.md is generated from README.Rmd. Please edit that file -->

# chartwatch-paper

<!-- badges: start -->
<!-- badges: end -->

## Files

| Filepath                                                                                            | Description                                                     |
|-----------------------------------------------------------------------------------------------------|-----------------------------------------------------------------|
| /mnt/research/LKS-CHART/Projects/gim\_ews\_project/data/paper-data/train\_vitals\_data.csv          | Raw vitals data (training set). Used to create Table 1.         |
| /mnt/research/LKS-CHART/Projects/gim\_ews\_project/data/paper-data/valid\_vitals\_data\_updated.csv | Raw vitals data (validation set). Used to create Table 1.       |
| /mnt/research/LKS-CHART/Projects/gim\_ews\_project/data/paper-data/test\_vitals\_data.csv           | Raw vitals data (prospective test set). Used to create Table 1. |

## Datasets

Clinician predictions:
`/mnt/research/LKS-CHART/Projects/gim_ews_preassessment_project/data/practitioner_predictions_outcomes.csv`

| Variable  | Description                                                                                                                   |
|-----------|-------------------------------------------------------------------------------------------------------------------------------|
| outcome   | 1 if visit experiences ICU, death, or palliative transfer at any point in the future - this is the column that should be used |
| OUTCOME   | 1 if visit experiences ICU or death at any point in the future                                                                |
| outcome48 | computed from the `outcome` column; 1 if visit experiences ICU, death or palliative transfer at any point in the future       |

HEWS/NEWS predictions: \`\`
