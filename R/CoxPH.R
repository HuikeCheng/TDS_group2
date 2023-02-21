#################### load packages
library(dplyr)
library(survival)
library(survminer)
################# load data
df <- readRDS("../data/Matched_data_v2.rds")
############### selected vars
sel_vars <- c(13, 14, 19, 20, 22, 23, 30, 31, 33, 16, 24, 25, 28)
model_df <- df[,c(3:12,sel_vars)]
summary(model_df)
# define reference level
model_df <- model_df %>% mutate(Smoke_status.0.0 = relevel(Smoke_status.0.0, ref = "Never"),
                                Ethnicity.0.0 = relevel(Ethnicity.0.0, ref = "British"),
                                Alcohol_freq.0.0 = relevel(Alcohol_freq.0.0, ref = "never"),
                                highest_Education0 = relevel(highest_Education0, ref = "Other_professional"))

####### assumptions of linearity
fit <- coxph(Surv(time_to_diagnosis, diag_6_mon) ~ Age_recr_continuous.0.0, 
             data = model_df)
ggcoxfunctional(fit, data = model_df)

######## fit model
model1 <- coxph(Surv(time_to_diagnosis, diag_6_mon) ~ . - Age_recr.0.0 - Lymphocyte_count.0.0
                - Monocyte_count.0.0 - Basophill_count.0.0 - Mean_corpuscular_vol.0.0, 
                data = model_df)
summary(model1)

######## Assumptions
# Test for the proportional-hazards (PH) assumption
test1 <- cox.zph(model1)
test1
# Schoenfeld residuals
ggcoxzph(test1)
# smoke status, BMI, wbc_count, plt_count, plt_width

####### time-split
tcuts <- c(1461, 2922, 4383)
vet_split <- survSplit(Surv(time_to_diagnosis, diag_6_mon) ~ . - Age_recr.0.0 - Lymphocyte_count.0.0
                       - Monocyte_count.0.0 - Basophill_count.0.0 - Mean_corpuscular_vol.0.0, 
                       data = model_df,
                       cut=tcuts, episode="ftime_group")

vet_split[, "ftime_group"] <- factor(vet_split[, "ftime_group"])

model1_t <- coxph(Surv(time_to_diagnosis, diag_6_mon) ~ . - Age_recr.0.0 - Lymphocyte_count.0.0
              - Monocyte_count.0.0 - Basophill_count.0.0 - Mean_corpuscular_vol.0.0 +
                Smoke_status.0.0*ftime_group + BMI.0.0*ftime_group + highest_Education0*ftime_group +
                Wbc_count.0.0*ftime_group + Rbc_count.0.0*ftime_group + Rbc_width.0.0*ftime_group + 
                Plt_count.0.0*ftime_group + Plt_width.0.0*ftime_group,
              data = vet_split)
summary(model1_t)


######### model 2
model2 <- coxph(Surv(time_to_diagnosis, diag_6_mon) ~ . - Age_recr.0.0, 
                data = model_df)
summary(model2)

######### model 3
model3 <- coxph(Surv(time_to_diagnosis, diag_6_mon) ~ . - Age_recr.0.0- Lymphocyte_count.0.0
                - Monocyte_count.0.0 - Mean_corpuscular_vol.0.0, 
                data = model_df)
summary(model3)

############ anova
anova(model1, model2)
anova(model1, model3)
anova(model3, model2)


###########plots
source('ggforest_new.R')
ggforest_new(model1, data = model_df)


############ outputs
# out_coxph1 <- cbind(round(summary(model1)$coefficients, 2), round(exp(confint(model1)), 2))
# write.csv(out_coxph1, "../output/out_coxph1.csv")

# out_coxph3 <- cbind(round(summary(model3)$coefficients, 2), round(exp(confint(model3)), 2))
# write.csv(out_coxph3, "../output/out_coxph3.csv")

############## km curve
alc_fit <- survfit(Surv(time_to_diagnosis, diag_6_mon) ~ Alcohol_freq.0.0, data = df)
ggsurvplot(
  alc_fit,                     # survfit object with calculated statistics.
  data = df,  # data used to fit survival curves. 
  risk.table = FALSE,       # show risk table.
  pval = TRUE,             # show p-value of log-rank test.
  conf.int = TRUE,         # show confidence intervals for 
  ggtheme = theme_minimal(), # customize plot and risk table with a theme.
  risk.table.y.text.col = T, # colour risk table text annotations.
  risk.table.y.text = FALSE # show bars instead of names in text annotations
)
