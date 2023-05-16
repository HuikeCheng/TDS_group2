rm(list=ls())

library(table1)

setwd("/rds/general/project/hda-22-23/live/TDS/Group2/R")
test <- readRDS("../data/Matched_data_v2.rds")

label(Matched_data_v2$time_to_diagnosis) <- "Time to Diagnosis"
test$diag_6_mon <- 
    factor(test$diag_6_mon , levels = c(0,1),
      labels = c("Control", "Case"))
label(test$time_to_diagnosis) <- "Time to Diagnosis"
label(test$Smoke_status.0.0) <- "Smoke Status"
label(test$Ethnicity.0.0) <- "Ethnicity"
label(test$BMI.0.0) <- "BMI"
label(test$Age_recr.0.0) <- "Age at Recruitment Categorised"
label(test$Age_recr_continuous.0.0) <- "Age at Recruitment"
Age_recr.0.0    
label(test$Alcohol_freq.0.0) <- "Alcohol Frequency"
label(test$highest_Education0) <- "Level of Education"
label(test$diag_6_mon) <- "Diagnosis at 6 Months"

sum(is.na(test$diag_6_mon))
sum(is.na(Matched_data_v2$diag_6_mon))

#full table 1 but needs splitting for slide size (too long). 
table1(~ Sex+ Smoke_status.0.0 + Ethnicity.0.0 + BMI.0.0 + Age_recr.0.0 + Age_recr_continuous.0.0 +
           + Alcohol_freq.0.0 + highest_Education0 | diag_6_mon, data = test, render.missing=NULL,  render.categorical="FREQ (PCTnoNA%)")
#needs to added cancer group to further stratify

#table 1 split 1
table1(~ Sex + Smoke_status.0.0 + Ethnicity.0.0 | diag_6_mon, data = test, render.missing=NULL,  render.categorical="FREQ (PCTnoNA%)")
#needs to added cancer group to further stratify

#table 1 split 2
table1(~ BMI.0.0+ Age_recr.0.0 + Age_recr_continuous.0.0 +
         + Alcohol_freq.0.0+ highest_Education0 | diag_6_mon, data = test, render.missing=NULL,  render.categorical="FREQ (PCTnoNA%)")
#needs to added cancer group to further stratify
str(test)
