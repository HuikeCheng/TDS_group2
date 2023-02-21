######### load packages
library(sharp)
library(survival)
library(dplyr)
library(cvwrapr)

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

####### prepare dataset
X <- df[, c(5:8, 10:33)]
newX <- model.matrix(~., data = X) # turn ethnicity to dummy variables
Y <- Surv(time = df$time_to_diagnosis, event = df$diag_6_mon, type = "right")

######## Var sel
fit <- VariableSelection(
  xdata = newX,
  ydata = Y,
  pi_list = seq(0.6, 0.9, by = 0.01),
  K = 100,
  tau = 0.5,
  seed = 1,
  n_cat = 3,
  family = "cox",
  implementation = PenalisedRegression,
  resampling = "subsampling",
  cpss = FALSE,
  PFER_method = "MB",
  PFER_thr = Inf,
  FDP_thr = Inf,
  Lambda_cardinal = 100,
  group_x = NULL,
  group_penalisation = FALSE,
  n_cores = 1,
  output_data = FALSE,
  verbose = TRUE
)

########## calibration plot
par(oma=c(1,1,1,2))
CalibrationPlot(fit)

########## var_sel plot
par(oma=c(6,1,0,0))
plot(fit,
      col = c("red", "grey"),
      col.axis = NULL,
      col.thr = "darkred",
      lty.thr = 2)

########### get results
summary(fit$S)
index <- which.max(fit$S)
fit$Lambda[index]
sel_var <- which(fit$selprop[index,] >= 0.90)
sel_var
beta_bl <- fit$Beta[index,sel_var,]

### 11 selected vars using selprop > 0.9
betas <- apply(beta_bl, 1, mean)
exp(betas)

########## prediction
# prepare test data
test_X <- df_test[, c(5:8, 10:33)]
test_X <- model.matrix(~., data = test_X)
test_Y <- Surv(time = df_test$time_to_diagnosis, event = df_test$diag_6_mon, type = "right")

pred <- predict(fit,
                xdata = newX,
                ydata = Y,
                newdata = test_X,
                method = "refit")
getCindex(pred = pred, y = test_Y) # 0.6726

################################################################################
# specify variables to be kept in model
weights <- c(1, 0, 0, rep(1, 21), rep(0, 4), rep(1, 21))

######## Var sel
fit1 <- VariableSelection(
  xdata = newX,
  ydata = Y,
  pi_list = seq(0.6, 0.9, by = 0.01),
  K = 100,
  tau = 0.5,
  seed = 1,
  n_cat = 3,
  family = "cox",
  implementation = PenalisedRegression,
  resampling = "subsampling",
  cpss = FALSE,
  PFER_method = "MB",
  PFER_thr = Inf,
  FDP_thr = Inf,
  Lambda_cardinal = 100,
  group_x = NULL,
  group_penalisation = FALSE,
  n_cores = 1,
  output_data = FALSE,
  verbose = TRUE,
  penalty.factor = weights)

########## calibration plot
par(oma=c(1,1,1,2))
CalibrationPlot(fit1)

########## var_sel plot
par(oma=c(6,1,0,0))
plot(fit1,
     col = c("red", "grey"),
     col.axis = NULL,
     col.thr = "darkred",
     lty.thr = 2)

########### get results
summary(fit1$S)
index <- which.max(fit1$S)
fit1$Lambda[index]
sel_var <- which(fit1$selprop[index,] >= 0.90)
sel_var
beta_bl <- fit1$Beta[index,sel_var+6,]

### 9 selected vars using selprop > 0.9
betas <- apply(beta_bl, 1, mean)
exp(betas)

########## prediction
pred <- predict(fit1,
                xdata = newX,
                ydata = Y,
                newdata = test_X,
                method = "refit")

getCindex(pred = pred, y = test_Y) # 0.664
