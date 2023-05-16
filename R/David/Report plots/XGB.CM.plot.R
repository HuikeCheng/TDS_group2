# CM plots
rm(list=ls())


library(ggplot2)
library(gridExtra)
library(caret)
library(ConfusionTableR)
################################################################################
############################# plot confusion matrix ############################
################################################################################
#pred: numeric vector of binary predictions
#actual: numeric vector of actual binary outcome
plotCM <- function(pred, actual){
  hm <- data.frame(table(pred, actual))
  tmp <- table(actual)
  hm$Proportion <- c(hm[1,3]/tmp[1], hm[2,3]/tmp[1], hm[3,3]/tmp[2], hm[4,3]/tmp[2])
  
  colnames(hm) <- c("Predicted", "Actual", "Count", "Proportion")
  
  # Replace numeric values with labels
  hm$Predicted <- factor(hm$Predicted, levels = c(1, 2), labels = c("Negative", "Positive"))
  hm$Actual <- factor(hm$Actual, levels = c(1, 2), labels = c("Negative", "Positive"))
  
  ggplot(hm, aes(Actual, Predicted, fill = Proportion)) + 
    geom_tile() +
    geom_text(aes(label = Count), color = "white", size = 5) +
    scale_fill_gradient(low = "#56B4E9", high = "#0072B2") +
    theme_bw()
}


xgb.all.pred <- readRDS("~/Desktop/TDS/TDS_project/Data/XGB/xgb.all.pred.rds")
xgb.all.test_y <- readRDS("~/Desktop/TDS/TDS_project/Data/XGB/xgb.all.test_y.rds")
xgb.all.pred

pred <- as.factor(xgb.all.pred$xgb.pred)
actual <- as.factor(xgb.all.test_y$test_y)
cm <- confusionMatrix(pred, actual)
ConfusionTableR::binary_class_cm(pred, actual)

print(cm)


pred <- as.numeric(xgb.all.pred$xgb.pred)
actual <- as.numeric(xgb.all.test_y$test_y)


cm1 <- plotCM(pred, actual)
plot(cm1)


# CM plot XGB lymphoid
xgb.lymph.pred <- readRDS("~/Desktop/TDS/TDS_project/Data/XGB/xgb.lymph.pred.rds")
xgb.lymph.test_y <- readRDS("~/Desktop/TDS/TDS_project/Data/XGB/xgb.lymph.test_y.rds")

pred <- as.factor(xgb.lymph.pred$xgb.pred)
actual <- as.factor(xgb.lymph.test_y$test_y)
cm <- confusionMatrix(pred, actual)
ConfusionTableR::binary_class_cm(pred, actual)

print(cm)



pred <- as.numeric(xgb.lymph.pred$xgb.pred)
actual <- as.numeric(xgb.lymph.test_y$test_y)

cm2 <- plotCM(pred, actual)
plot(cm2)

# CM plot RF myeloid
xgb.myeloid.pred <- readRDS("~/Desktop/TDS/TDS_project/Data/XGB/xgb.myeloid.pred.rds")
xgb.myeloid.test_y <- readRDS("~/Desktop/TDS/TDS_project/Data/XGB/xgb.myeloid.test_y.rds")

pred <- as.factor(xgb.myeloid.pred$xgb.pred)
actual <- as.factor(xgb.myeloid.test_y$test_y)
cm <- confusionMatrix(pred, actual)
ConfusionTableR::binary_class_cm(pred, actual)

print(cm)


pred <- as.numeric(xgb.myeloid.pred$xgb.pred)
actual <- as.numeric(xgb.myeloid.test_y$test_y)

cm3 <- plotCM(pred, actual)
plot(cm3)

# CM plot RF other
xgb.other.pred <- readRDS("~/Desktop/TDS/TDS_project/Data/XGB/xgb.other.pred.rds")
xgb.other.test_y <- readRDS("~/Desktop/TDS/TDS_project/Data/XGB/xgb.other.test_y.rds")


pred <- as.factor(xgb.other.pred$xgb.pred)
actual <- as.factor(xgb.other.test_y$test_y)
cm <- confusionMatrix(pred, actual)
ConfusionTableR::binary_class_cm(pred, actual)

print(cm)

pred <- as.numeric(xgb.other.pred$xgb.pred)
actual <- as.numeric(xgb.other.test_y$test_y)

cm4 <- plotCM(pred, actual)
plot(cm4)


p1 <- cm1 + ggtitle("XGB Model - All")
p2 <- cm2 + ggtitle("XGB Model - Lymphoid")
p3 <- cm3 + ggtitle("XGB Model - Myeloid")
p4 <- cm4 + ggtitle("XGB Model - Other")

# create the grid of plots
grid.arrange(p1, p2, p3, p4, ncol = 2, top = "Confusion Matrix Plots For XGBoost Models")
