######### load packages
source("R/Functions.R")
library(tableone)
library(ggplot2)

######### load data
# mydata <- readRDS("data/data_buf.rds")
mydata <- readRDS("data/data_healthy_control.rds")

######## control vs cases
# mydata <- mydata %>% mutate(case = NULL,
#                             prevalent_case = NULL,
#                             incident_case = NULL)

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
##### not really useful, too less cases compared to controls
# data_feature <- baseline_data[,c(6:42)]
# data_feature$diag_6_mon <- as.factor(mydata$diag_6_mon)
# data_feature <- data_feature[complete.cases(data_feature), ]
# 
# x <- colnames(data_feature)[4]
# ggplot(data = data_feature, aes(x = eval(parse(text = x)), color = diag_6_mon, fill = diag_6_mon)) +
#   geom_histogram() +
#   scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
#   scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))


################################
############################### combine data and ICD_data #############################
library(corrplot)
library(purrr)
library(dplyr)

###################
mydata <- readRDS("data/data_healthy_control.rds")
icd_data <- readRDS("data/ICD_data.rds")

###### remove percentages columns
a <- grepl(".*prct.*", colnames(mydata))
b <- which(a)
colnames(mydata)[b]

mydata <- mydata[, -b]

###### remove ICD columns
mydata <- mydata[, -c(36:47)]

##### correlation matrix
m <- cor(mydata[,c(14:35)], method = "spearman", use="complete.obs")
corrplot(m, method="circle", type = "upper")

find_g_90 <- function(x) {which(x > 0.9)}
for (i in 1:nrow(m)) {
  print(i)
  print(find_g_90(m[i,]))
}
# 18,22; 22,18
# reticulocyte count and high light scatter reticulocyte count is > 90% correlated
mydata <- mydata %>% select(!Hlsr_count.0.0)
colnames(mydata)

############### merge with icd_data ###############
mydata1 <- merge(x = mydata, y = icd_data, by = "eid", all = TRUE)

######## quality checks ###########
table(mydata1$diag_6_mon, mydata$healthy_control)

########
mydata1 <- mydata1 %>% select(-date_death, -healthy_control)
colnames(mydata1)

####### check NAs
nas <- map_dbl(mydata1, function(x) {sum(is.na(x))})
nas
############# deal with time_to_diagnose
difftime(as.Date("2023-01-31"), mydata1$date_recr[1])

mydata1$time_to_diagnosis <- ifelse(is.na(mydata1$time_to_diagnosis), 
                               difftime(as.Date("2023-01-31"), mydata1$date_recr), 
                               mydata1$time_to_diagnosis)
########### remove date_diagnosis
mydata1 <- mydata1 %>% select(-date_diagnosis)

######## check NAs
nas <- map_dbl(mydata1, function(x) {sum(is.na(x))})
nas
####### deal with demographic variables
# use complete cases for Age and Ethnicity
tmp <- subset(mydata1, is.na(mydata1$Ethnicity.0.0))
library(tidyverse)
mydata1 <- mydata1 %>% drop_na(Ethnicity.0.0, Age_recr.0.0, Age_recr_continuous.0.0)
######## check NAs
nas <- map_dbl(mydata1, function(x) {sum(is.na(x))})
nas
####### save data
# saveRDS(mydata1, "data/model_data_v1.rds")

####### recode extra variables
mydata1 <- readRDS("data/model_data_v1.rds")
other_var <- readRDS("data/other_variable_noncoded_clean.rds")
other_var <- other_var[mydata1$eid,]
### missing values
n_na <- map_dbl(other_var, function(x){sum(is.na(x))})

# recode alcohol freq:
other_var$Alcohol_freq.0.0 <- as.character(other_var$Alcohol_freq.0.0)
other_var$Alcohol_freq.0.0 <- recode(other_var$Alcohol_freq.0.0, 
                                     "1" = "daily",
                                     "2" = "weekly3_4",
                                     "3" = "weekly1_2",
                                     "4" = "monthly",
                                     "5" = "rarely",
                                     "6" = "never",
                                     "-3" = "-3")
other_var$Alcohol_freq.0.0 <- ifelse(other_var$Alcohol_freq.0.0 == "-3", NA, other_var$Alcohol_freq.0.0)

# recode highest education:
other_var$highest_Education0 <- as.character(other_var$highest_Education0)
other_var$highest_Education0 <- recode(other_var$highest_Education0, 
                                       "1" = "College",
                                       "2" = "A_level",
                                       "3" = "GCSE",
                                       "4" = "CSE",
                                       "5" = "NVQ",
                                       "6" = "Other_professional",
                                       "-7" = "Others",
                                       "-3" = "-3")
other_var$highest_Education0 <- ifelse(other_var$highest_Education0 == "-3", NA, other_var$highest_Education0)

## recode genetic sex: female-0, male-1
other_var$Genetic_sex.0.0 <- ifelse(other_var$Genetic_sex.0.0 == 1, "Male", other_var$Genetic_sex.0.0)
other_var$Genetic_sex.0.0 <- ifelse(other_var$Genetic_sex.0.0 == 0, "Female", other_var$Genetic_sex.0.0)
#### exclude
eid_list <- rownames(other_var)[(other_var$Genetic_sex.0.0 == "Female" & mydata1$Sex == "Male") | (other_var$Genetic_sex.0.0 == "Male" & mydata1$Sex == "Female")]
eid_list <- eid_list[is.na(eid_list) == FALSE]
length(eid_list) # 34
### remove
mydata2 <- mydata1 %>% filter(!(eid %in% eid_list))
###
other_var$eid <- rownames(other_var)

mydata2 <- merge(mydata2, other_var, by = "eid", all.x = TRUE)
mydata2 <- mydata2 %>% mutate(Genetic_sex.0.0 = NULL)

####### quality check for blood counts
ms <- rowSums(is.na(mydata1[,11:31]))
summary(as.factor(ms))
ms_data <- mydata1[which(ms == 21), ]

mydata2 <- mydata2 %>% filter(!(eid %in% ms_data$eid))
table(mydata2$diag_6_mon)

####### change order of columns
# mydata2 <- readRDS("data/model_data_v2.rds")
# mydata2 <- mydata2[,c(1:31, 55, 56, 32:54)]
# mydata2 <- mydata2[,c(1:10, 32, 33, 11:31, 34:56)]
# mydata2 <- mydata2[, c(1,2,4,3,5:56)]
# saveRDS(mydata2, "data/model_data_v2.rds")

####### data type
# map_chr(mydata2, class)
# mydata2$Alcohol_freq.0.0 <- as.factor(mydata2$Alcohol_freq.0.0)
# mydata2$highest_Education0 <- as.factor(mydata2$highest_Education0)
# saveRDS(mydata2, "data/model_data_v2.rds")

###### deal with smoke_status prefer not to answer?
# mydata2 <- readRDS("../data/archive/model_data_v2.rds")
# mydata2$Smoke_status.0.0 <- as.character(mydata2$Smoke_status.0.0)
# mydata2$Smoke_status.0.0 <- ifelse(mydata2$Smoke_status.0.0 == "Prefer not to answer", 
#                                    NA, 
#                                    mydata2$Smoke_status.0.0)
# mydata2$Smoke_status.0.0 <- as.factor(mydata2$Smoke_status.0.0)
# saveRDS(mydata2, "../data/archive/model_data_v2.rds")

########################### explore matched data ###############################
##### load packages
library(ggplot2)
library(tidyverse)
library(ggpubr)

###### load data
df <- readRDS("../data/Matched_data.rds")

####### case control
table(df$diag_6_mon)

####### plots
df %>% ggplot() + geom_bar(aes(x = Sex, fill = as.factor(diag_6_mon)))
df %>% ggplot() + geom_bar(aes(x = Ethnicity.0.0, fill = as.factor(diag_6_mon))) + 
  coord_flip()

df %>% mutate(diag_6_mon = as.factor(diag_6_mon)) %>% 
  ggplot(., aes(x = Age_recr_continuous.0.0, fill = diag_6_mon)) + 
  geom_histogram(position="dodge", alpha = 0.7) +
  scale_fill_grey() + theme_classic()

df %>% ggplot() + geom_bar(aes(x = Alcohol_freq.0.0, fill = as.factor(diag_6_mon)))
df %>% ggplot() + geom_bar(aes(x = highest_Education0, fill = as.factor(diag_6_mon)))

bc_plots <- vector("list", length = 21)
for (i in 13:33) {
  x <- colnames(df)[i]
  bc_plots[[i-12]] <- df %>% mutate(diag_6_mon = as.factor(diag_6_mon)) %>% 
    ggplot(., aes(.data[[x]], fill = diag_6_mon)) + 
    geom_density(alpha = 0.5)
}

ggarrange(plotlist = bc_plots[1:9], ncol = 3, nrow = 3)
