#### load package
library(ggplot2)
library(dplyr)
library(gridExtra)

#### load data
df_lym <- readRDS("xgb.lyph.varimp.rds")
df_all <- readRDS("xgb.all.varimp.rds")
df_mye <- readRDS("xgb.myeloid.varimp.rds")
df_oth <- readRDS("xgb.other.varimp.rds")

####
plotVarImp <- function(data, varname, varimp, criteria) {
  # prepare dataset
  colnames(data)[varname]  <- "varname"
  colnames(data)[varimp]  <- "varimp"
  colnames(data)[criteria] <- "criteria"
  # plot
  ggplot(data, aes(x=reorder(varname, varimp), y=varimp, color = criteria)) + 
    geom_point() +
    geom_segment(aes(x=varname,xend=varname,y=0,yend=varimp)) + 
    scale_color_manual(values=c("darkgrey", "red")) +
    ylab("Variable Importance") +
    xlab("") +
    coord_flip() +
    theme_bw() + 
    theme(legend.position = "none")
}

########################## Myeloid ###################################
df_mye$varname <- rownames(df_mye, 3)

df_mye$varname <- recode(df_mye$varname,
                     "Wbc_count.0.0" = "WBC count",
                     "Rbc_count.0.0" = "RBC count",
                     "Haemoglobin_conc.0.0" = "Hb conc.",
                     "Mean_corpuscular_vol.0.0" = "MCV",
                     "Mean_hgb.0.0" = "MCH",
                     "Mean_hgb_conc.0.0" = "MCHC",
                     "Rbc_width.0.0" = "RDW",
                     "Plt_count.0.0" = "PLT count",
                     "Plt_crit.0.0" = "PCT",
                     "Mean_plt_vol.0.0" = "MPV",
                     "Plt_width.0.0" = "PDW",
                     "Lymphocyte_count.0.0" = "Lymphocyte count",
                     "Monocyte_count.0.0" = "Monocyte count",
                     "Neutrophill_count.0.0" = "Neutrophil count",
                     "Eosinophill_count.0.0" = "Eosinophil count",
                     "Basophill_count.0.0" = "Basophil count",
                     "Nucleated_rbc_count.0.0" = "NRBC count",
                     "Reticulocyte_count.0.0" = "Reticulocyte count",
                     "Mean_reticulocyte_vol.0.0" = "MRV",
                     "Mean_spcell_vol.0.0" = "MSCV",
                     "Ir_fraction.0.0" = "IRF",
                     "SexMale" = "Sex_Male",
                     "Age_recr_continuous.0.0" = "Age at recruitment",
                     "BMI.0.0" = "BMI",
                     "Ethnicity.0.0" = "Ethnicity",
                     "Ethnicity.0.0African" = "Ethnicity_African",
                     "Ethnicity.0.0Asian" = "Ethnicity_Asian",
                     "Ethnicity.0.0Prefer not to answer" = "Ethnicity_Prefer not to answer",
                     "Ethnicity.0.0Mixed" = "Ethnicity_Mixed",
                     "Ethnicity.0.0Other" = "Ethnicity_Other",
                     "Ethnicity.0.0Caribbean" = "Ethnicity_Caribbean",
                     "Smoke_status.0.0" = "Smoking status",
                     "Smoke_status.0.0Current" = "Smoking status_Current",
                     "Smoke_status.0.0Previous" = "Smoking status_Previous",
                     "Alcohol_freq.0.0" = "Alcohol intake",
                     "Alcohol_freq.0.0rarely" = "Alcohol intake_Rarely",
                     "Alcohol_freq.0.0monthly" = "Alcohol intake_Monthly",
                     "Alcohol_freq.0.0weekly1_2" = "Alcohol intake_1-2 weekly",
                     "Alcohol_freq.0.0weekly3_4" = "Alcohol intake_3-4 weekly",
                     "Alcohol_freq.0.0daily" = "Alcohol intake_Daily",
                     "highest_Education0" = "Highest education",
                     "highest_Education0A_level" = "Highest education_A level",
                     "highest_Education0College" = "Highest education_College",
                     "highest_Education0CSE" = "Highest education_CSE",
                     "highest_Education0GCSE" = "Highest education_GCSE",
                     "highest_Education0NVQ" = "Highest education_NVQ",
                     "highest_Education0Others" = "Highest education_Others")


df_mye$model <- rep("Myeloid", 16)
top5 <- order(df_mye$Overall, decreasing = TRUE)[1:5]
df_mye$criteria <- rep(0, 16)
df_mye$criteria[top5] <- 1
df_mye$criteria <- as.factor(df_mye$criteria)
plotVarImp(data = df_mye, varname = 2, varimp = 1, criteria = 4)

####################### ALL #############################
df_all$varname <- rownames(df_all, 3)

df_all$varname <- recode(df_all$varname,
                     "Wbc_count.0.0" = "WBC count",
                     "Rbc_count.0.0" = "RBC count",
                     "Haemoglobin_conc.0.0" = "Hb conc.",
                     "Mean_corpuscular_vol.0.0" = "MCV",
                     "Mean_hgb.0.0" = "MCH",
                     "Mean_hgb_conc.0.0" = "MCHC",
                     "Rbc_width.0.0" = "RDW",
                     "Plt_count.0.0" = "PLT count",
                     "Plt_crit.0.0" = "PCT",
                     "Mean_plt_vol.0.0" = "MPV",
                     "Plt_width.0.0" = "PDW",
                     "Lymphocyte_count.0.0" = "Lymphocyte count",
                     "Monocyte_count.0.0" = "Monocyte count",
                     "Neutrophill_count.0.0" = "Neutrophil count",
                     "Eosinophill_count.0.0" = "Eosinophil count",
                     "Basophill_count.0.0" = "Basophil count",
                     "Nucleated_rbc_count.0.0" = "NRBC count",
                     "Reticulocyte_count.0.0" = "Reticulocyte count",
                     "Mean_reticulocyte_vol.0.0" = "MRV",
                     "Mean_spcell_vol.0.0" = "MSCV",
                     "Ir_fraction.0.0" = "IRF",
                     "SexMale" = "Sex_Male",
                     "Age_recr_continuous.0.0" = "Age at recruitment",
                     "BMI.0.0" = "BMI",
                     "Ethnicity.0.0" = "Ethnicity",
                     "Ethnicity.0.0African" = "Ethnicity_African",
                     "Ethnicity.0.0Asian" = "Ethnicity_Asian",
                     "Ethnicity.0.0Prefer not to answer" = "Ethnicity_Prefer not to answer",
                     "Ethnicity.0.0Mixed" = "Ethnicity_Mixed",
                     "Ethnicity.0.0Other" = "Ethnicity_Other",
                     "Ethnicity.0.0Caribbean" = "Ethnicity_Caribbean",
                     "Smoke_status.0.0" = "Smoking status",
                     "Smoke_status.0.0Current" = "Smoking status_Current",
                     "Smoke_status.0.0Previous" = "Smoking status_Previous",
                     "Alcohol_freq.0.0" = "Alcohol intake",
                     "Alcohol_freq.0.0rarely" = "Alcohol intake_Rarely",
                     "Alcohol_freq.0.0monthly" = "Alcohol intake_Monthly",
                     "Alcohol_freq.0.0weekly1_2" = "Alcohol intake_1-2 weekly",
                     "Alcohol_freq.0.0weekly3_4" = "Alcohol intake_3-4 weekly",
                     "Alcohol_freq.0.0daily" = "Alcohol intake_Daily",
                     "highest_Education0" = "Highest education",
                     "highest_Education0A_level" = "Highest education_A level",
                     "highest_Education0College" = "Highest education_College",
                     "highest_Education0CSE" = "Highest education_CSE",
                     "highest_Education0GCSE" = "Highest education_GCSE",
                     "highest_Education0NVQ" = "Highest education_NVQ",
                     "highest_Education0Others" = "Highest education_Others")

df_all$model <- rep("All", 28)
top5 <- order(df_all$Overall, decreasing = TRUE)[1:5]
df_all$criteria <- rep(0, 28)
df_all$criteria[top5] <- 1
df_all$criteria <- as.factor(df_all$criteria)
plotVarImp(data = df_all, varname = 2, varimp = 1, criteria = 4)

######################## Lymphoid
df_lym$varname <- rownames(df_lym, 3)

df_lym$varname <- recode(df_lym$varname,
                     "Wbc_count.0.0" = "WBC count",
                     "Rbc_count.0.0" = "RBC count",
                     "Haemoglobin_conc.0.0" = "Hb conc.",
                     "Mean_corpuscular_vol.0.0" = "MCV",
                     "Mean_hgb.0.0" = "MCH",
                     "Mean_hgb_conc.0.0" = "MCHC",
                     "Rbc_width.0.0" = "RDW",
                     "Plt_count.0.0" = "PLT count",
                     "Plt_crit.0.0" = "PCT",
                     "Mean_plt_vol.0.0" = "MPV",
                     "Plt_width.0.0" = "PDW",
                     "Lymphocyte_count.0.0" = "Lymphocyte count",
                     "Monocyte_count.0.0" = "Monocyte count",
                     "Neutrophill_count.0.0" = "Neutrophil count",
                     "Eosinophill_count.0.0" = "Eosinophil count",
                     "Basophill_count.0.0" = "Basophil count",
                     "Nucleated_rbc_count.0.0" = "NRBC count",
                     "Reticulocyte_count.0.0" = "Reticulocyte count",
                     "Mean_reticulocyte_vol.0.0" = "MRV",
                     "Mean_spcell_vol.0.0" = "MSCV",
                     "Ir_fraction.0.0" = "IRF",
                     "SexMale" = "Sex_Male",
                     "Age_recr_continuous.0.0" = "Age at recruitment",
                     "BMI.0.0" = "BMI",
                     "Ethnicity.0.0" = "Ethnicity",
                     "Ethnicity.0.0African" = "Ethnicity_African",
                     "Ethnicity.0.0Asian" = "Ethnicity_Asian",
                     "Ethnicity.0.0Prefer not to answer" = "Ethnicity_Prefer not to answer",
                     "Ethnicity.0.0Mixed" = "Ethnicity_Mixed",
                     "Ethnicity.0.0Other" = "Ethnicity_Other",
                     "Ethnicity.0.0Caribbean" = "Ethnicity_Caribbean",
                     "Smoke_status.0.0" = "Smoking status",
                     "Smoke_status.0.0Current" = "Smoking status_Current",
                     "Smoke_status.0.0Previous" = "Smoking status_Previous",
                     "Alcohol_freq.0.0" = "Alcohol intake",
                     "Alcohol_freq.0.0rarely" = "Alcohol intake_Rarely",
                     "Alcohol_freq.0.0monthly" = "Alcohol intake_Monthly",
                     "Alcohol_freq.0.0weekly1_2" = "Alcohol intake_1-2 weekly",
                     "Alcohol_freq.0.0weekly3_4" = "Alcohol intake_3-4 weekly",
                     "Alcohol_freq.0.0daily" = "Alcohol intake_Daily",
                     "highest_Education0" = "Highest education",
                     "highest_Education0A_level" = "Highest education_A level",
                     "highest_Education0College" = "Highest education_College",
                     "highest_Education0CSE" = "Highest education_CSE",
                     "highest_Education0GCSE" = "Highest education_GCSE",
                     "highest_Education0NVQ" = "Highest education_NVQ",
                     "highest_Education0Others" = "Highest education_Others")

df_lym$model <- rep("Lymphoid", 28)
top5 <- order(df_lym$Overall, decreasing = TRUE)[1:5]
df_lym$criteria <- rep(0, 28)
df_lym$criteria[top5] <- 1
df_lym$criteria <- as.factor(df_lym$criteria)
plotVarImp(data = df_lym, varname = 2, varimp = 1, criteria = 4)


################################ Other ##########################################
df_oth$varname <- rownames(df_oth, 3)

df_oth$varname <- recode(df_oth$varname,
                     "Wbc_count.0.0" = "WBC count",
                     "Rbc_count.0.0" = "RBC count",
                     "Haemoglobin_conc.0.0" = "Hb conc.",
                     "Mean_corpuscular_vol.0.0" = "MCV",
                     "Mean_hgb.0.0" = "MCH",
                     "Mean_hgb_conc.0.0" = "MCHC",
                     "Rbc_width.0.0" = "RDW",
                     "Plt_count.0.0" = "PLT count",
                     "Plt_crit.0.0" = "PCT",
                     "Mean_plt_vol.0.0" = "MPV",
                     "Plt_width.0.0" = "PDW",
                     "Lymphocyte_count.0.0" = "Lymphocyte count",
                     "Monocyte_count.0.0" = "Monocyte count",
                     "Neutrophill_count.0.0" = "Neutrophil count",
                     "Eosinophill_count.0.0" = "Eosinophil count",
                     "Basophill_count.0.0" = "Basophil count",
                     "Nucleated_rbc_count.0.0" = "NRBC count",
                     "Reticulocyte_count.0.0" = "Reticulocyte count",
                     "Mean_reticulocyte_vol.0.0" = "MRV",
                     "Mean_spcell_vol.0.0" = "MSCV",
                     "Ir_fraction.0.0" = "IRF",
                     "SexMale" = "Sex_Male",
                     "Age_recr_continuous.0.0" = "Age at recruitment",
                     "BMI.0.0" = "BMI",
                     "Ethnicity.0.0" = "Ethnicity",
                     "Ethnicity.0.0African" = "Ethnicity_African",
                     "Ethnicity.0.0Asian" = "Ethnicity_Asian",
                     "Ethnicity.0.0Prefer not to answer" = "Ethnicity_Prefer not to answer",
                     "Ethnicity.0.0Mixed" = "Ethnicity_Mixed",
                     "Ethnicity.0.0Other" = "Ethnicity_Other",
                     "Ethnicity.0.0Caribbean" = "Ethnicity_Caribbean",
                     "Smoke_status.0.0" = "Smoking status",
                     "Smoke_status.0.0Current" = "Smoking status_Current",
                     "Smoke_status.0.0Previous" = "Smoking status_Previous",
                     "Alcohol_freq.0.0" = "Alcohol intake",
                     "Alcohol_freq.0.0rarely" = "Alcohol intake_Rarely",
                     "Alcohol_freq.0.0monthly" = "Alcohol intake_Monthly",
                     "Alcohol_freq.0.0weekly1_2" = "Alcohol intake_1-2 weekly",
                     "Alcohol_freq.0.0weekly3_4" = "Alcohol intake_3-4 weekly",
                     "Alcohol_freq.0.0daily" = "Alcohol intake_Daily",
                     "highest_Education0" = "Highest education",
                     "highest_Education0A_level" = "Highest education_A level",
                     "highest_Education0College" = "Highest education_College",
                     "highest_Education0CSE" = "Highest education_CSE",
                     "highest_Education0GCSE" = "Highest education_GCSE",
                     "highest_Education0NVQ" = "Highest education_NVQ",
                     "highest_Education0Others" = "Highest education_Others")

df_oth$model <- rep("Other", 12)
top5 <- order(df_oth$Overall, decreasing = TRUE)[1:5]
df_oth$criteria <- rep(0, 12)
df_oth$criteria[top5] <- 1
df_oth$criteria <- as.factor(df_oth$criteria)
plotVarImp(data = df_oth, varname = 2, varimp = 1, criteria = 4)

################################################################################
#### Combine
df <- rbind(df_all, df_lym, df_mye, df_oth)
plotVarImp4(data = df, varname = 2, varimp = 1, model = 3, criteria = 4)
#saveRDS(df, "../../../output/XGB_varimp.rds")
