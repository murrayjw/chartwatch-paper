#'   ***********************************************************************
#'   ***********************************************************************
#'   ***********************************************************************
#'   Initially created by: Josh Murray 
#'   Date: April 23rd 2019
#'   contact information: murrayj@smh.ca
#'
#'   Script contents: Generate 800 random 4 digit numbers to be used as
#'   study ids 
#'
#'   ***********************************************************************
#'   ***********************************************************************
#'   ***********************************************************************
#'   
# path to the data
data_path <- "Z:\\LKS-CHART\\Projects\\gim_ews_preassessment_project\\data\\link_log\\"

# a tibble to hold study ids and mrns
study_id <- tibble(study_id = 2000:4000,
                   MRN = "") 

# the current link log
file_name <- paste0(data_path, "link_log.xlsx")

# open the link log
wb = createWorkbook()
sheet = createSheet(wb, "Link Log")
addDataFrame(as.data.frame(study_id), sheet=sheet,  row.names=FALSE)
# save the empty link log
saveWorkbook(wb, file_name)
