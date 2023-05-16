setwd("/rds/general/project/hda-22-23/live/TDS/Group2/R")
df <- readRDS("../data/Matched_data_v3.rds")
df_test <- readRDS("../data/Imputed_test_V_v3.rds")
df_cat <- readRDS("../data/Matched_data_v2.rds")
df_cat_test <- readRDS("../data/Imputed_test_V_v2.rds")
# change categorical columns back
df_cat <- df_cat[,c(1,5,6,11,12)]
df <- df[,-c(5,6,10,11)]
df <- merge(df, df_cat, by = "eid", all.x = TRUE)

# same for test set
df_cat_test <- df_cat_test[,c(1,5,6,11,12)]
df_test <- df_test[,-c(5,6,10,11)]
df_test <- merge(df_test, df_cat_test, by = "eid", all.x = TRUE)
#glimpse(df_test)
#colnames(df_test)
# test x
test_X <- df_test[, c(5:29, 33:36)]

# remove not needed
rm(df_cat)
rm(df_cat_test)


#################################### Myeloid ##################################
####### prepare model data
mye_df <- df %>% filter(!(diag_6_mon == 1 & myeloid == 0))
# check
table(mye_df$diag_6_mon, mye_df$myeloid)
# select the vars needed
sel_vars <- c(11, 12, 14, 15, 18, 20, 23, 25, 28)
mye_df <- mye_df[,c(30, 5:7, 33:36, sel_vars)]
summary(mye_df)
mye_df$myeloid <- as.factor(mye_df$myeloid)
# rematch
bd <- ovun.sample(myeloid ~., data = mye_df, method = "both",
                  p = 0.5,
                  seed = 3,
                  N = 6520)$data
# check ethnicity
table(bd$Ethnicity.0.0)
###### unregularised
bd <- bd %>% mutate(Smoke_status.0.0 = relevel(Smoke_status.0.0, ref = "Never"),
                    Ethnicity.0.0 = relevel(Ethnicity.0.0, ref = "White"),
                    Alcohol_freq.0.0 = relevel(Alcohol_freq.0.0, ref = "never"),
                    highest_Education0 = relevel(highest_Education0, ref = "Other_professional"))
summary(bd)

################################################################################
##################################### other ####################################
################################################################################
####### prepare model_data
# get other cases vs controls
oth_df <- df %>% filter(!(diag_6_mon == 1 & other == 0))
# check
table(oth_df$diag_6_mon, oth_df$other)
# select the vars needed
sel_vars <- c(8, 9, 14, 15, 18)
oth_df <- oth_df[,c(32, 5:7, 33:36, sel_vars)]
summary(oth_df)
# rematch
bd <- ovun.sample(other ~., data = oth_df, method = "both",
                  p = 0.5,
                  seed = 3,
                  N = 6867)$data
# check ethnicity
table(bd$Ethnicity.0.0)
###### unregularised
bd <- bd %>% mutate(Smoke_status.0.0 = relevel(Smoke_status.0.0, ref = "Never"),
                    Ethnicity.0.0 = relevel(Ethnicity.0.0, ref = "White"),
                    Alcohol_freq.0.0 = relevel(Alcohol_freq.0.0, ref = "never"),
                    highest_Education0 = relevel(highest_Education0, ref = "Other_professional"))
summary(bd)