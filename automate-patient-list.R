# Path to script that should be sourced
rscript <- "H:/gim_ews_preassessment/01-generate-daily-patient-list.R"

# To create the task
taskscheduleR::taskscheduler_create(taskname = basename(rscript),
                                    rscript = rscript, 
                                    schedule = c("DAILY"),
                                    starttime = "15:00",
                                    startdate = format(as.Date("2019-05-17"), "%m/%d/%Y"))
