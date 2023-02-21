#########
library(glmnet)
library(survival)
library(cvwrapr)
library(dplyr)

######## load data
df <- readRDS("../data/Matched_data_v2.rds")
df_test <- readRDS("../data/Imputed_test_V_v2.rds")

####### Code alcohol_freq, education and smoking as ordinal
df$Alcohol_freq.0.0 <- as.character(df$Alcohol_freq.0.0)
df$Alcohol_freq.0.0 <- recode(df$Alcohol_freq.0.0, 
                              "daily" = "6",
                              "weekly3_4" = "5",
                              "weekly1_2" = "4",
                              "monthly" = "3",
                              "rarely" = "2",
                              "never" = "1")
df$Alcohol_freq.0.0 <- as.numeric(df$Alcohol_freq.0.0)

#### highest education
df$highest_Education0 <- as.character(df$highest_Education0)
df$highest_Education0 <- recode(df$highest_Education0, 
                                "College" = "1",
                                "A_level" = "2",
                                "GCSE" = "3",
                                "CSE" = "4",
                                "NVQ" = "5",
                                "Other_professional" = "6",
                                "Others" = "7")
df$highest_Education0 <- as.numeric(df$highest_Education0)

####### smoking
df$Smoke_status.0.0 <- as.character(df$Smoke_status.0.0)
df$Smoke_status.0.0 <- recode(df$Smoke_status.0.0, 
                              "Never" = "1",
                              "Previous" = "2",
                              "Current" = "3")
df$Smoke_status.0.0 <- as.numeric(df$Smoke_status.0.0)

####### sex
df$Sex <- ifelse(df$Sex == "Female", 0, 1)

############################# same for test_df #################################
df_test$Alcohol_freq.0.0 <- as.character(df_test$Alcohol_freq.0.0)
df_test$Alcohol_freq.0.0 <- recode(df_test$Alcohol_freq.0.0, 
                              "daily" = "6",
                              "weekly3_4" = "5",
                              "weekly1_2" = "4",
                              "monthly" = "3",
                              "rarely" = "2",
                              "never" = "1")
df_test$Alcohol_freq.0.0 <- as.numeric(df_test$Alcohol_freq.0.0)

#### highest education
df_test$highest_Education0 <- as.character(df_test$highest_Education0)
df_test$highest_Education0 <- recode(df_test$highest_Education0, 
                                "College" = "1",
                                "A_level" = "2",
                                "GCSE" = "3",
                                "CSE" = "4",
                                "NVQ" = "5",
                                "Other_professional" = "6",
                                "Others" = "7")
df_test$highest_Education0 <- as.numeric(df_test$highest_Education0)

####### smoking
df_test$Smoke_status.0.0 <- as.character(df_test$Smoke_status.0.0)
df_test$Smoke_status.0.0 <- recode(df_test$Smoke_status.0.0, 
                              "Never" = "1",
                              "Previous" = "2",
                              "Current" = "3")
df_test$Smoke_status.0.0 <- as.numeric(df_test$Smoke_status.0.0)

####### sex
df_test$Sex <- ifelse(df_test$Sex == "Female", 0, 1)


####### prepare model_data
X <- df[, c(5:8, 10:33)]
newX <- model.matrix(~., data = X) # turn ethnicity to dummy variables
Y <- Surv(time = df$time_to_diagnosis, event = df$diag_6_mon, type = "right")

######### CV
cvfit <- cv.glmnet(newX, Y, family = "cox", type.measure = "C")
plot(cvfit)

best_lam <- cvfit$lambda.1se

######### fit
fit <- glmnet(newX, Y, family = "cox")
# regularisation path
plot(fit)
# get coefficients
coef(fit, s = best_lam)
# get the coefs for selected vars
tmp <- coef(fit, s = best_lam)
coefs <- exp(tmp@x)
names(coefs) <- colnames(newX)[tmp@i+1]
coefs

##### predict
test_X <- df_test[, c(5:8, 10:33)]
test_X <- model.matrix(~., data = test_X)
fit_test <- predict(fit, newx=test_X, s=best_lam, type = "response")

test_Y <- Surv(time = df_test$time_to_diagnosis, event = df_test$diag_6_mon, type = "right")

getCindex(pred = fit_test, y = test_Y) # 0.6740



###############################################################################
######### CV
# specify variables to be kept in model
weights <- c(1, 0, 0, rep(1, 21), rep(0, 4), rep(1, 21))
cvfit1 <- cv.glmnet(newX, Y, family = "cox", type.measure = "C", penalty.factor = weights)
plot(cvfit1)

best_lam1 <- cvfit1$lambda.1se # 0.016

######### fit
fit1 <- glmnet(newX, Y, family = "cox", penalty.factor = weights)
# regularisation path
plot(fit1)
# get coefficients
coef(fit1, s = best_lam1)
# get the coefs for selected vars
tmp1 <- coef(fit1, s = best_lam1)
coefs1 <- exp(tmp1@x)
names(coefs1) <- colnames(newX)[tmp1@i+1]
coefs1

##### predict
fit_test1 <- predict(fit1, newx=test_X, s=best_lam1, type = "response")

getCindex(pred = fit_test1, y = test_Y) # 0.6651
