################### Generate dataset for controls ################
library(dplyr)
######## import dataset
mydata <- readRDS("data/data_buf.rds")

healthy <- readRDS("data/healthy.rds") 
# this contains 56973 individuals that does not have any ICD codes listed in UK_Biobank
# healthy individuals are labeled as 0

######## turn into dataframe
healthy <- data.frame(healthy = healthy,
                      eid = names(healthy))
####### merge
mydata <- merge(x = mydata, y = healthy, by = "eid", all = TRUE)
mydata$healthy

####### quality check
which((mydata$case == 1 & mydata$healthy == 0) == T)
mydata[12558,]
mydata[158304,]
mydata[188639,]
mydata[300836,]
# 4 people who doesn't have any UKB ICDs are labeled as cases by HES, all of the
# 4 are diagnosed after 6 months of recruitment
# it could be that for these 4 individuals, UKB is out of date

######### generate control data ##########
mydata$healthy <- ifelse(is.na(mydata$healthy), 1, 0)
sum(mydata$healthy == 0)
mydata$healthy_control <- ifelse(mydata$diag_6_mon == 0 & mydata$healthy == 0 & is.na(mydata$date_death), 1, 0)
sum(mydata$healthy_control)
######### quality check
sum(mydata$healthy_control == 1 & mydata$diag_6_mon == 1)
sum(mydata$healthy_control == 1 & !is.na(mydata$date_death))

######## clean up temporary columns
mydata <- mydata %>% mutate(healthy = NULL)

######## clean up variables not needed
# no need for other case/control statuses
mydata <- mydata %>% mutate(case = NULL,
                            prevalent_case = NULL,
                            incident_case = NULL)
# no need for subsequent date of assess
mydata <- mydata %>% mutate(Date_assess.1 = NULL,
                            Date_assess.2 = NULL,
                            Date_assess.3 = NULL)
# no need for subsequent blood test
na_prct <- map_dbl(mydata[,c(6:112)],prct_na)
names(na_prct) <- colnames(mydata)[6:112]
a <- na_prct > 0.9
b <- names(na_prct)[a]

mydata <- mydata %>% select(!any_of(b))
mydata <- mydata %>% select(!c("Smoke_status.2.0", "BMI.2.0"))

########### order of columns ###########
mydata <- mydata[,c(1:5, 55, 56, 6:54)]

########### remove other controls #############
mydata <- mydata %>% filter(!(diag_6_mon == 0 & healthy_control == 0))

########### save dataset
saveRDS(mydata, "data/data_healthy_control.rds")
