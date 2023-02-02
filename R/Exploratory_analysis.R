######### load packages
source("R/Functions.R")
library(tableone)
######### load data
mydata <- readRDS("data/full_data.rds")

######### missing values
na_prct <- map_dbl(mydata[,9:118],prct_na)
names(na_prct) <- colnames(mydata)[9:118]
na_prct
plot(na_prct)

na_count <- map_dbl(mydata[9:118],count_not_na)
names(na_count) <- colnames(mydata)[9:118]
min(na_count) # 5274
which.min(na_count)

######## control vs cases
mydata$case_sum <- ifelse(mydata$case == 0, "control", "cases")
mydata$case_sum <- ifelse(mydata$incident_case == 1, "incident", mydata$case_sum)
mydata$case_sum <- ifelse(mydata$prevalent_case == 1, "prevalent", mydata$case_sum)
mydata$case_sum <- as.factor(mydata$case_sum)
summary(mydata$case_sum)

plot(mydata$case_sum)

################ exclusion criteria
# remove prevalent cases
mydata <- mydata %>% filter(prevalent_case != 1)

################ table one
a <- names(na_prct)[na_prct > 0.9]
baseline_data <- mydata %>% select(!any_of(a))
baseline_data <- baseline_data %>% select(!c("Date_assess.2", "Smoke_status.2.0", 
                                             "BMI.2.0", "prevalent_case"))
baseline_data$case_sum <- droplevels(baseline_data$case_sum)
tab1 <- CreateTableOne(data = baseline_data[,-c(1:7, 45:56)], strata = "case_sum")
tab1
summary(tab1)

##### save table one
tab1Mat <- print(tab1, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
#write.csv(tab1Mat, file = "output/TableOne.csv")

##### prct missing for baseline data
na_prct <- map_dbl(baseline_data[,-c(1:7, 45:56)],prct_na)
names(na_prct) <- colnames(baseline_data)[-c(1:7, 45:56)]
as.data.frame(na_prct)
#write.csv(as.data.frame(na_prct), file = "output/Missingness.csv")

############## graphs #############
#library(DataExplorer)
#create_report(data = baseline_data[, -c(1:7, 45:56)])

############# ggplots group by case-control status #############
############# consider using rmarkdown #############