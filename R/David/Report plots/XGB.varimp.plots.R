rm(list=ls())

library(ggplot2)
library(dplyr)
library(gridExtra)

setwd("~/Desktop/TDS/TDS_project/Data/XGB")

xgb.lyph.varimp <- readRDS("~/Desktop/TDS/TDS_project/Data/XGB/xgb.lyph.varimp.rds")


plotVarImp <- function(data, varname, varimp, cutoff) {
  # prepare dataset
  colnames(data)[varname]  <- "varname"
  colnames(data)[varimp]  <- "varimp"
  data$criteria <- as.factor(ifelse(data$varimp >= cutoff, 1, 0))
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

df <- xgb.lyph.varimp
df$varname <- rownames(df, 3)

df$varname <- recode(df$varname,
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
                     "highest_Education0" = "Highest Education",
                     "highest_Education0A_level" = "Highest education_A level",
                     "highest_Education0College" = "Highest education_College",
                     "highest_Education0CSE" = "Highest education_CSE",
                     "highest_Education0GCSE" = "Highest education_GCSE",
                     "highest_Education0NVQ" = "Highest education_NVQ",
                     "highest_Education0Others" = "Highest education_Others")

df
p1 <- plotVarImp(data = df, varname = 2, varimp = 1, cutoff = 80)
ggsave("xgb.lyph.varimp.png")
p1
       
xgb.all.varimp <- readRDS("~/Desktop/TDS/TDS_project/Data/XGB/xgb.all.varimp.rds")


df <- xgb.all.varimp
df$varname <- rownames(df, 3)

df$varname <- recode(df$varname,
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
                     "highest_Education0" = "Highest Education",
                     "highest_Education0A_level" = "Highest education_A level",
                     "highest_Education0College" = "Highest education_College",
                     "highest_Education0CSE" = "Highest education_CSE",
                     "highest_Education0GCSE" = "Highest education_GCSE",
                     "highest_Education0NVQ" = "Highest education_NVQ",
                     "highest_Education0Others" = "Highest education_Others")

p2 <- plotVarImp(data = df, varname = 2, varimp = 1, cutoff = 80)
ggsave("xgb.all.varimp.png")

xgb.myeloid.varimp <- readRDS("~/Desktop/TDS/TDS_project/Data/XGB/xgb.myeloid.varimp.rds")


df <- xgb.myeloid.varimp
df$varname <- rownames(df, 3)

df$varname <- recode(df$varname,
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
                     "highest_Education0" = "Highest Education",
                     "highest_Education0A_level" = "Highest education_A level",
                     "highest_Education0College" = "Highest education_College",
                     "highest_Education0CSE" = "Highest education_CSE",
                     "highest_Education0GCSE" = "Highest education_GCSE",
                     "highest_Education0NVQ" = "Highest education_NVQ",
                     "highest_Education0Others" = "Highest education_Others")

p3 <- plotVarImp(data = df, varname = 2, varimp = 1, cutoff = 80)
ggsave("xgb.myeloid.varimp.png")



xgb.other.varimp <- readRDS("~/Desktop/TDS/TDS_project/Data/XGB/xgb.other.varimp.rds")

df <- xgb.other.varimp
df$varname <- rownames(df, 3)

df$varname <- recode(df$varname,
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
                     "highest_Education0" = "Highest Education",
                     "highest_Education0A_level" = "Highest education_A level",
                     "highest_Education0College" = "Highest education_College",
                     "highest_Education0CSE" = "Highest education_CSE",
                     "highest_Education0GCSE" = "Highest education_GCSE",
                     "highest_Education0NVQ" = "Highest education_NVQ",
                     "highest_Education0Others" = "Highest education_Others")

p4 <- plotVarImp(data = df, varname = 2, varimp = 1, cutoff = 80)
ggsave("xgb.other.varimp.png")

p1 <- p1 + ggtitle("XGB Model - Lymphoid")
p2 <- p2 + ggtitle("XGB Model - All")
p3 <- p3 + ggtitle("XGB Model - Myeloid")
p4 <- p4 + ggtitle("XGB Model - Other")

# create the grid of plots
grid.arrange(p2, p1, p3, p4, ncol = 2, top = "Variable Importance Plots For XGBoost")

# save the plot as a png file
png("xgb.combined.varimp.png", width = 6, height = 8)
grid.arrange(p2, p1, p3, p4, ncol = 2, top = "Variable Importance Plots For XGBoost")
dev.off()

