#########
library(glmnet)
library(survival)
library(cvwrapr)
library(tidyverse)

######## load data
df <- readRDS("../data/Matched_data_v3.rds")
df_test <- readRDS("../data/Imputed_test_V_v3.rds")

###### Ethnicity
summary(df$Ethnicity.0.0)
summary(df_test$Ethnicity.0.0)

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



##########
# df$Ethnicity.0.0 <- recode(df$Ethnicity.0.0,
#                            "Black" = "African",
#                            "Do not know" = "Prefer not to answer")
# df_test$Ethnicity.0.0 <- recode(df_test$Ethnicity.0.0,
#                                 "Black" = "African",
#                                 "Do not know" = "Prefer not to answer")
# df$Ethnicity.0.0 <- droplevels(df$Ethnicity.0.0)           
# df_test$Ethnicity.0.0 <- droplevels(df_test$Ethnicity.0.0)
# saveRDS(df, "../data/Matched_data_v3.rds")
# saveRDS(df_test, "../data/Imputed_test_V_v3.rds")