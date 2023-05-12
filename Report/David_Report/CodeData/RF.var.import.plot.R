rm(list=ls())


library(caret)
library(randomForest)
library(ggplot2)
library(dplyr)

setwd("~/Desktop/TDS/TDS_project/Data/RF")
RF.myeloid.model <- readRDS("~/Desktop/TDS/TDS_project/Data/RF/RF.myeloid.model.rds")
RF.all.model <- readRDS("~/Desktop/TDS/TDS_project/Data/RF/RF.all.model.rds")
RF.lymph.model <- readRDS("~/Desktop/TDS/TDS_project/Data/RF/RF.lymph.model.rds")
RF.other.model <- readRDS("~/Desktop/TDS/TDS_project/Data/RF/RF.other.model.rds")
varImp(RF.myeloid.model, scale = TRUE)

plotVarImp <- function(data, varname, varimp, cutoff) {
  # prepare dataset
  colnames(data)[varname]  <- "varname"
  colnames(data)[varimp]  <- "varimp"
  data$ScaledImportance <- 100 * scale(data$varimp, center = min(data$varimp), scale = max(data$varimp) - min(data$varimp))
  data$criteria <- as.factor(ifelse(data$ScaledImportance >= cutoff, 1, 0))
  # plot
  ggplot(data, aes(x=reorder(varname, ScaledImportance), y=ScaledImportance, color = criteria)) + 
    geom_point() +
    geom_segment(aes(x=varname,xend=varname,y=0,yend=ScaledImportance)) + 
    scale_color_manual(values=c("darkgrey", "red")) +
    ylab("Variable Importance") +
    xlab("") +
    coord_flip() +
    theme_bw() + 
    theme(legend.position = "none")
}

importance(RF.myeloid.model)
df <- importance(RF.myeloid.model) %>% 
  data.frame() %>% 
  mutate(feature = row.names(.))
df
colnames(df)
row.names(df)

#The 'gain' metric is similar to the MeanDecreaseGini metric and reflects the relative contribution of each feature to the model's accuracy.

df$varname <- rownames(df)

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
colnames(df)
length(df$feature)
length(df$MeanDecreaseGini)
p1 <- plotVarImp(data = df, varname = 6, varimp = 4, cutoff = 80)
ggsave("RF.myeloid.varimp.png")



RF.all.model <- readRDS("~/Desktop/TDS/TDS_project/Data/RF/RF.all.model.rds")

df <- importance(RF.all.model) %>% 
  data.frame() %>% 
  mutate(feature = row.names(.))

colnames(df)
row.names(df)

df$varname <- rownames(df, 5)

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
colnames(df)

p2 <- plotVarImp(data = df, varname = 6, varimp = 4, cutoff = 80)
p2
ggsave("RF.all.varimp.png")


df <- importance(RF.lymph.model) %>% 
  data.frame() %>% 
  mutate(feature = row.names(.))

colnames(df)
row.names(df)

df$varname <- rownames(df, 5)

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
colnames(df)

p3 <- plotVarImp(data = df, varname = 6, varimp = 4, cutoff = 80)
p3
ggsave("RF.lyph.varimp.png")



df <- importance(RF.other.model) %>% 
  data.frame() %>% 
  mutate(feature = row.names(.))

colnames(df)
row.names(df)

df$varname <- rownames(df, 5)

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
colnames(df)

p4 <- plotVarImp(data = df, varname = 6, varimp = 4, cutoff = 80)
p4
ggsave("RF.other.varimp.png")

p1 <- p1 + ggtitle("RF Model - Myeloid")
p2 <- p2 + ggtitle("RF Model - All")
p3 <- p3 + ggtitle("RF Model - Lymphoid")
p4 <- p4 + ggtitle("RF Model - Other")

# create the grid of plots
grid.arrange(p2, p3, p1, p4, ncol = 2, top = "Variable Importance Plots For Random Forest")

# save the plot as a PDF file
png("RF.combined.varimp.png", width = 6, height = 8)
grid.arrange(p2, p3, p1, p4, ncol = 2, top = "Variable Importance Plots For Random Forest")
dev.off()
