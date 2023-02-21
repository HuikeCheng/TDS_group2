################## load packages ##################
library(purrr)
library(dplyr)
source("R/Functions.R")
############### load data ##############
data_full <- readRDS("data/archive/full_data.rds")
############# check if blood cancer icds match
# hes icds are for blood cancers only
# UK Biobank included icds for all cancers
# so hes icds should be in UK biobank icds
data_case <- data_full[data_full$case == 1,c(1,119:130)]

match_check <- vector("double", length = nrow(data_case))
rownames(data_case) <- c(1:nrow(data_case))
for (i in 1:nrow(data_case)){
  hes_icd <- data_case[i, c(2:5)]
  hes_icd <- hes_icd[!is.na(hes_icd)]
  ukb_icd <- data_case[i, c(6:12)]
  ukb_icd <- ukb_icd[!is.na(ukb_icd)]
  ck <- sum(!(hes_icd %in% ukb_icd))
  match_check[i] <- ck
}
sum(match_check != 0)
a <- which(match_check != 0)
b <- data_case[a,]
data_full[data_full$eid == "6024691",]


###############################################################################
################################# PREVIOUS CODE ###############################
###############################################################################
# ################ load data ##############
# outcome_final <- readRDS("data/outcome_final.rds")
# ukb_recoded <- readRDS("data/ukb_recoded.rds")
# ukb_extracted <- readRDS("extraction_and_recoding/outputs/ukb_extracted.rds")
# ########## have a look ##########
# head(outcome_final)
# head(ukb_recoded)
# # 
############ recoded ICDs are not helpful
# which(colnames(ukb_recoded) == "UKB_ICD9.0.0") #270
# which(colnames(ukb_recoded) == "UKB_ICD9.14.0") #284
# which(colnames(ukb_recoded) == "UKB_ICD10.0.0") # 226
# which(colnames(ukb_recoded) == "UKB_ICD10.21.0") # 247
# which(colnames(ukb_recoded) == "Age_recr.0.0") # 17
# colnames(ukb_recoded)[285] #"Age_recr_continuous.0.0"
# 
# ukb_data <- cbind(ukb_recoded[,c(1:17, 285, 18:225, 248:269)], ukb_extracted[,c(226:247, 270:284)])
# summary(ukb_data)
#
# ############ simplifying ukb_icd codes
# which(colnames(ukb_data) == "UKB_ICD9.0.0") #271
# which(colnames(ukb_data) == "UKB_ICD9.14.0") #285
# for (i in c(271:285)) {
#   print(colnames(ukb_data)[i])
#   print(sum(ukb_data[,i] %in% c("2384, 2386, 2387")))
# }
# # no 2384, 2386, 2387 in ukb_icd9
# ####### apply simplification
# which(colnames(ukb_data) == "UKB_ICD10.0.0") #249
# which(colnames(ukb_data) == "UKB_ICD10.21.0") #270
# 
# for (i in c(249:285)) {
#   ukb_data[,i] <- get_first3(ukb_data[,i])
#   }
# 
# summary(ukb_data)
# ########## generate eid column
# ukb_data$eid <- rownames(ukb_data)
# sum(ukb_data$eid != rownames(ukb_data))
# 
# ############ match to generate full dataset
# data_full <- merge(x = ukb_data, y = outcome_final, by = "eid", all = TRUE)
#
# ########## compare dates of assessment ########## no need for now
# b <- colnames(data_full)[grepl("date....$", colnames(data_full))]
# b
# a <- which(grepl("date....$", colnames(data_full)))
# data_full <- data_full[,-c(a)]
#
# ##### rename some columns ######
# colnames(data_full)[2] <- "Sex"
# colnames(data_full)[3] <- "Date_assess.0"
# colnames(data_full)[4] <- "Date_assess.1"
# colnames(data_full)[5] <- "Date_assess.2"
# colnames(data_full)[6] <- "Date_assess.3"

# ##### Check date of first assessment vs date of recruitment
# sum(data_full$Date_assess.0 != data_full$date_recr)
# data_full <- data_full %>% mutate(Date_assess.0 = NULL)
# which(colnames(data_full) == "date_recr")
# data_full <- data_full[,c(1, 193, 2:192, 194:203)]

# ##### remove UKB_date_cancer_diag
# b <- colnames(data_full)[grepl("^UKB_date_cancer_diag", colnames(data_full))]
# b
# a <- which(grepl("^UKB_date_cancer_diag", colnames(data_full)))
# data_full <- data_full[,-c(a)]
# which(colnames(data_full) == "date_diagnosis")
# data_full <- data_full[,c(1,2, 172:177, 3:171, 178:181)]

# #### remove UKB_age_cancer_diag
# b <- colnames(data_full)[grepl("^UKB_age_cancer_diag", colnames(data_full))]
# b
# a <- which(grepl("^UKB_age_cancer_diag", colnames(data_full)))
# data_full <- data_full[,-c(a)]

# ## NAs in UKB_ICD10 columns are ""
# for (i in c(119:140)) {
#   data_full[,i] <- na_if(data_full[,i], "")
# }

# # UKB_ICD_9.9.0 & UKB_ICD9.13.0 are empty
# data_full <- data_full %>% mutate(UKB_ICD9.9.0 = NULL,
#                                   UKB_ICD9.13.0 = NULL)

# ###### check ICD codes
# UKB_codes <- data_full[, c(1, 119:153)]
# 
# map_dbl(UKB_codes, count_not_na)

# ######### get people with icd codes
# a <- rowSums(!is.na(UKB_codes[,-1]))
# b <- a>0
# UKB_codes <- UKB_codes[b,]

# ############# try to simplify
# ukb_list <- vector("list", length = nrow(UKB_codes))
# for (i in 1:nrow(UKB_codes)){
#   eid <- UKB_codes$eid[i]
#   icd <- UKB_codes[i,-1]
#   icd <- icd[!is.na(icd)]
#   icd <- unique(icd)
#   out <- c(eid, icd)
#   names(out)[[1]] <- "eid"
#   ukb_list[[i]] <- out
# }
# 
# max(map_dbl(ukb_list, length))
# which.max(map_dbl(ukb_list, length))
# ukb_list[[20448]]
# ukb_list <- map(ukb_list, extend_data)
# UKB_codes <- as.data.frame(list.rbind(ukb_list))
# colnames(UKB_codes)[2:9] <- c(paste0("UKB_ICD_", c(1:8)))
#
# sum(!(UKB_codes$eid %in% data_full$eid))
# ######## match simplified UKB codes to data_full
# data_full <- merge(x = data_full, y = UKB_codes, by = "eid", all = TRUE)

# #### remove unsimplified ICD_columns
# b <- colnames(data_full)[grepl("^UKB_ICD10", colnames(data_full))]
# b
# a <- which(grepl("^UKB_ICD10", colnames(data_full)))
# data_full <- data_full[,-c(a)]
#
# b <- colnames(data_full)[grepl("^UKB_ICD9", colnames(data_full))]
# b
# a <- which(grepl("^UKB_ICD9", colnames(data_full)))
# data_full <- data_full[,-c(a)]

#################### buffering window ####################
data_full <- data_full %>% mutate(diag_6_mon = ifelse(difftime(date_diagnosis, date_recr) > 180, 1, 0))
data_full$diag_6_mon <- ifelse(data_full$case == 0, 0, data_full$diag_6_mon)
summary(data_full$diag_6_mon)
table(data_full$case, data_full$diag_6_mon)
sum(data_full$diag_6_mon)

############ dataset with > 6 diag only
buf_data <- data_full %>% filter(!(prevalent_case == 1))
buf_data <- buf_data %>% filter(!(diag_6_mon == 0 & incident_case == 1))
table(buf_data$case, buf_data$diag_6_mon)


# ######## save data file ############
# saveRDS(data_full, file = "data/full_data.rds")
# saveRDS(buf_data, file = "data/data_buf.rds")
