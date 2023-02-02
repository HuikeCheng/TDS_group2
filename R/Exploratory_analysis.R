######### load packages
source("R/Functions.R")
library(tableone)

######### load data
mydata <- readRDS("data/data_buf.rds")

######## control vs cases
mydata <- mydata %>% mutate(case = NULL,
                            prevalent_case = NULL,
                            incident_case = NULL)

table(mydata$diag_6_mon)
# check
diag_data <- subset(mydata, mydata$diag_6_mon == 1)
check_data <- difftime(diag_data$date_diagnosis, diag_data$date_recr) > 180
sum(check_data)
# passed

######### missing values
na_prct <- map_dbl(mydata[,c(6:115, 128)],prct_na)
names(na_prct) <- colnames(mydata)[c(6:115,128)]
na_prct
plot(na_prct)

na_count <- map_dbl(mydata[c(6:115, 128)],count_not_na)
names(na_count) <- colnames(mydata)[c(6:115, 128)]
min(na_count) # 5264
which.min(na_count)

################ table one
a <- names(na_prct)[na_prct > 0.9]
baseline_data <- mydata %>% select(!any_of(a))
baseline_data <- baseline_data %>% select(!c("Date_assess.2", "Smoke_status.2.0", 
                                             "BMI.2.0"))
tab1 <- CreateTableOne(data = baseline_data[,c(6:42, 55)], strata = "diag_6_mon")
tab1
summary(tab1)

##### save table one
tab1Mat <- print(tab1, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
#write.csv(tab1Mat, file = "output/TableOne.csv")

##### prct missing for baseline data
na_prct <- map_dbl(baseline_data[,c(6:42, 55)],prct_na)
names(na_prct) <- colnames(baseline_data)[c(6:42, 55)]
as.data.frame(na_prct)
#write.csv(as.data.frame(na_prct), file = "output/Missingness.csv")

############## graphs #############
#library(DataExplorer)
#create_report(data = baseline_data[, c(6:42, 55)])

############# ggplots group by case-control status #############

############# consider using rmarkdown #############