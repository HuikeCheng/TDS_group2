# CM plots
rm(list=ls())


library(ggplot2)
library(gridExtra)
library(caret)
library(ConfusionTableR)
library(pROC)

set.seed(2)

RF.all.train_pred <- readRDS("~/Desktop/TDS/TDS_project/Data/RF/RF.all.train_pred.rds")
RF.all.train_y <- readRDS("~/Desktop/TDS/TDS_project/Data/RF/RF.all.train_y.rds")


pred <- as.numeric(RF.all.train_pred[,2] > 0.5)
pred <- as.factor(pred)
actual <- as.factor(RF.all.train_y$train_y)
cm <- confusionMatrix(pred, actual)
ConfusionTableR::binary_class_cm(pred, actual)

print(cm)

RF.all.model <- readRDS("~/Desktop/TDS/TDS_project/Data/RF/RF.all.model.rds")
cm <- confusionMatrix(RF.all.model$pred,RF.all.model$y)
cm

cm <- ConfusionTableR::binary_class_cm(RF.all.model$pred,RF.all.model$y)
cm

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


RF.all.pred <- readRDS("~/Desktop/TDS/TDS_project/Data/RF/RF.all.pred.rds")
RF.all.test_y <- readRDS("~/Desktop/TDS/TDS_project/Data/RF/RF.all.train_y.rds")
RF.all.pred_probs <- readRDS("~/Desktop/TDS/TDS_project/Data/RF/RF.all.train_pred_probs.rds")

colnames(RF.all.pred_probs)
pred <- as.factor(RF.all.pred$pred)
test_y <- as.factor(RF.all.test_y$train_y)
pred_probs <- as.numeric(RF.all.pred_probs[, 2])
length(test_y)
length(pred_probs)


RF.all.pred <- readRDS("~/Desktop/TDS/TDS_project/Data/RF/RF.all.pred.rds")
RF.all.test_y <- readRDS("~/Desktop/TDS/TDS_project/Data/RF/RF.all.train_y.rds")
RF.all.pred_probs <- readRDS("~/Desktop/TDS/TDS_project/Data/RF/RF.all.train_pred_probs.rds")

colnames(RF.all.pred_probs)
pred <- as.factor(RF.all.pred$pred)
test_y <- as.factor(RF.all.test_y$train_y)
pred_probs <- as.numeric(RF.all.pred_probs[, 2])
length(test_y)
length(pred_probs)


RF.all.roc_obj <- roc(as.numeric(RF.all.model$y), as.numeric(RF.all.model$predicted))
RF.all.auc_value <- auc(RF.all.roc_obj)
RF.all.auc_value


#lymphoid

RF.lymph.model <- readRDS("~/Desktop/TDS/TDS_project/Data/RF/RF.lymph.model.rds")

cm <- confusionMatrix(RF.lymph.model$pred,RF.lymph.model$y)
cm

RF.lymph.model <- roc(as.numeric(RF.lymph.model$y), as.numeric(RF.lymph.model$predicted))
RF.lymph.model.auc_value <- auc(RF.lymph.model)
RF.lymph.model.auc_value

TP <-3688
FP <-434
TN <-3889
FN <- 631

#precision 
precision<-TP/(TP+FP)
precision

#recall
recall<-TP/(TP+FN)
recall


#myleoid

RF.myeloid.model <- readRDS("~/Desktop/TDS/TDS_project/Data/RF/RF.myeloid.model.rds")

cm <- confusionMatrix(RF.myeloid.model$pred,RF.myeloid.model$y)
cm

RF.myeloid.model <- roc(as.numeric(RF.myeloid.model$y), as.numeric(RF.myeloid.model$predicted))
RF.myeloid.model.auc_value <- auc(RF.myeloid.model)
RF.myeloid.model.auc_value

TP <-3688
FP <-145
TN <-3112
FN <- 71

#precision 
precision<-TP/(TP+FP)
precision

#recall
recall<-TP/(TP+FN)
recall

#other
RF.other.model <- readRDS("~/Desktop/TDS/TDS_project/Data/RF/RF.other.model.rds")

cm <- confusionMatrix(RF.other.model$pred,RF.other.model$y)
cm

RF.other.model <- roc(as.numeric(RF.other.model$y), as.numeric(RF.other.model$predicted))
RF.other.model.auc_value <- auc(RF.other.model)
RF.other.model.auc_value

TP <-3280
FP <-192
TN <-3230
FN <- 165

#precision 
precision<-TP/(TP+FP)
precision

#recall
recall<-TP/(TP+FN)
recall
