# CM plots
rm(list=ls())


library(ggplot2)
library(gridExtra)
library(caret)
library(xgboost)
library(ConfusionTableR)
library(pROC)

set.seed(2)



#XGB all 

model <- readRDS("~/Desktop/TDS/TDS_project/Data/XGB/xgb.all.xgb_model.rds")
train_data <- model$trainingData
predictions <- predict(model, train_data)
cm <- confusionMatrix(predictions, train_data$.outcome)
cm

xgb.all.train_pred <- readRDS("~/Desktop/TDS/TDS_project/Data/XGB/xgb.all.train_pred.rds")
xgb.all.train_prob <- readRDS("~/Desktop/TDS/TDS_project/Data/XGB/xgb.all.train_prob.rds")
xgb.all.train_y <- readRDS("~/Desktop/TDS/TDS_project/Data/XGB/xgb.all.train_y.rds")

pred <- as.factor(xgb.all.train_pred$train_pred)
actual <- as.factor(xgb.all.train_y$train_y)

cm <- confusionMatrix(pred, actual)
cm

pred <- as.numeric(xgb.all.train_prob$train_pred_probs)
actual <- as.numeric(xgb.all.train_y$train_y)

XGB.all.model.roc <- roc(actual, pred)
XGB.all.model.auc_value <- auc(XGB.all.model.roc)
XGB.all.model.auc_value

TP <-3511
FP <-880
TN <-4605
FN <- 1974

#precision 
precision<-TP/(TP+FP)
precision

#recall
recall<-TP/(TP+FN)
recall



#XGB lymph

model <- readRDS("~/Desktop/TDS/TDS_project/Data/XGB/xgb.lymph.xgb_model.rds")
train_data <- model$trainingData
colnames(train_data)

outcome <- train_data[,29]
train <- train_data[, -29]
predictions <- predict(model, train)
cm <- confusionMatrix(predictions, outcome)
cm




xgb.lymph.train_pred <- readRDS("~/Desktop/TDS/TDS_project/Data/XGB/xgb.lymph.train_pred.rds")
xgb.lymph.train_prob <- readRDS("~/Desktop/TDS/TDS_project/Data/XGB/xgb.lymph.train_prob.rds")
xgb.lymph.train_y <- readRDS("~/Desktop/TDS/TDS_project/Data/XGB/xgb.lymph.train_y.rds")

pred <- as.factor(xgb.lymph.train_pred$train_pred)
actual <- as.factor(xgb.lymph.train_y$train_y)

cm <- confusionMatrix(pred, actual)
cm

table(xgb.lymph.train_pred)
table(xgb.lymph.train_y)
table(xgb.lymph.train_prob)

pred <- as.numeric(xgb.lymph.train_prob$train_pred_probs)
actual <- as.numeric(xgb.lymph.train_y$train_y)

XGB.all.model.roc <- roc(actual, pred)
XGB.all.model.auc_value <- auc(XGB.all.model.roc)
XGB.all.model.auc_value

TP <-3169
FP <-1571
TN <-3914
FN <- 2316

#precision 
precision<-TP/(TP+FP)
precision

#recall
recall<-TP/(TP+FN)
recall


#xgb myeloid






xgb.myeloid.train_pred <- readRDS("~/Desktop/TDS/TDS_project/Data/XGB/xgb.myeloid.train_pred.rds")
xgb.myeloid.train_prob <- readRDS("~/Desktop/TDS/TDS_project/Data/XGB/xgb.myeloid.train_prob.rds")
xgb.myeloid.train_y <- readRDS("~/Desktop/TDS/TDS_project/Data/XGB/xgb.myeloid.train_y.rds")

pred <- as.factor(xgb.myeloid.train_pred$train_pred)
actual <- as.factor(xgb.myeloid.train_y$train_y)

cm <- confusionMatrix(pred, actual)
cm

pred <- as.numeric(xgb.myeloid.train_prob$train_pred_probs)
actual <- as.numeric(xgb.myeloid.train_y$train_y)

XGB.all.model.roc <- roc(actual, pred)
XGB.all.model.auc_value <- auc(XGB.all.model.roc)
XGB.all.model.auc_value

TP <-3169
FP <-1571
TN <-3914
FN <- 2316

#precision 
precision<-TP/(TP+FP)
precision

#recall
recall<-TP/(TP+FN)
recall

#XGB other
xgb.other.train_pred <- readRDS("~/Desktop/TDS/TDS_project/Data/XGB/xgb.other.train_pred.rds")
xgb.other.train_prob <- readRDS("~/Desktop/TDS/TDS_project/Data/XGB/xgb.other.train_prob.rds")
xgb.other.train_y <- readRDS("~/Desktop/TDS/TDS_project/Data/XGB/xgb.other.train_y.rds")

pred <- as.factor(xgb.other.train_pred$train_pred)
actual <- as.factor(xgb.other.train_y$train_y)

cm <- confusionMatrix(pred, actual)
cm

pred <- as.numeric(xgb.other.train_prob$train_pred_probs)
actual <- as.numeric(xgb.other.train_y$train_y)

XGB.all.model.roc <- roc(actual, pred)
XGB.all.model.auc_value <- auc(XGB.all.model.roc)
XGB.all.model.auc_value

TP <-3169
FP <-1571
TN <-3914
FN <- 2316

#precision 
precision<-TP/(TP+FP)
precision

#recall
recall<-TP/(TP+FN)
recall
