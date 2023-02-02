################ load packages ############
library(tidyverse)
library(purrr)
source("R/Functions.R")
############### import dataset ############
outcome_df <- readRDS("data/outcome_data.rds")
hes_icd <- readRDS("data/hes_icds_v2.rds")
reg_icd <- readRDS("data/registry_icds_v2.rds")
############### have a look ###############
head(outcome_df)
head(hes_icd)
head(reg_icd)
################ match icd to participant ##################
colnames(hes_icd)[1] <- "eid"
colnames(reg_icd)[1] <- "eid"
df1 <- merge(x = outcome_df, y = hes_icd, by = "eid", all = TRUE)
colnames(df1)[9] <- "HES_ICD"
df1 <- merge(x = df1, y = reg_icd, by = "eid", all = TRUE)
colnames(df1)[10] <- "REG_ICD"

################# quality check #######################
####### controls should not have icd codes
contrl_df <- subset(df1, case == 0)
sum(is.na(contrl_df$HES_ICD))
sum(is.na(contrl_df$REG_ICD))
# passed
####### cases should have icd codes
case_df <- subset(df1, case == 1)
sum(is.na(case_df$HES_ICD) & is.na(case_df$REG_ICD)) # 3781
# get problematic records
prob <- is.na(case_df$HES_ICD) & is.na(case_df$REG_ICD)
prob_df <- case_df[prob,] 
no_h_r_icd <- prob_df$eid # cases without icd codes from hes or registry

############################ get icds for each eid #############################
### take out records with 2384, 2386, 2387
tmp_df1 <- subset(case_df, case_df$HES_ICD %in% c("2384", "2386", "2387"))
tmp_df2 <- subset(case_df, !(case_df$HES_ICD %in% c("2384", "2386", "2387")))
# note: no records in REG_ICD contain 2384
### remove the last digit from icd codes other than 2384, 2386, 2387
tmp_df2$HES_ICD <- map_chr(tmp_df2$HES_ICD, get_first3)
tmp_df2$REG_ICD <- map_chr(tmp_df2$REG_ICD, get_first3)
tmp_df1$REG_ICD <- map_chr(tmp_df1$REG_ICD, get_first3)
head(tmp_df2)
tmp_df1
##### get simplified codes
tmp_df <- rbind(tmp_df1, tmp_df2)
head(tmp_df)
##### list of icds for each eid
list_eid <- split(tmp_df[,c(1,9,10)], tmp_df$eid)
######## expand the list into a dataframe #################
b <- map(list_eid, rm_dup_row)
b <- map(b, expand_data)
### check length of each element
req_length <- max(map_dbl(b, length)) #5
which.max(map_dbl(b, length))
b[[239]]
b <- map(b, extend_data) # set default to 5 first, so can use map here without external argument
### convert to dataframe
df_eid_icd <- as.data.frame(list.rbind(b))
colnames(df_eid_icd)[2:5] <- c("ICD_1", "ICD_2", "ICD_3", "ICD_4")

#################### remove temporary data.frames ##########################
# obj_to_rm <- c("contrl_df", "case_df", "tmp_df", "tmp_df1", "tmp_df2", "b", 
#                "df1", "list_eid")
# rm(list = obj_to_rm)

############## combine eid_icd data to outcome_df
# outcome_final <- merge(x = outcome_df, y = df_eid_icd, by = "eid", all = TRUE)

############### save data ####################
# saveRDS(outcome_final, file = "data/outcome_final.rds")
