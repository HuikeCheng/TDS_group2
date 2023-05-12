# CM plots
rm(list=ls())


library(ggplot2)
library(gridExtra)
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


RF.all.pred <- readRDS("~/Desktop/TDS/TDS_project/Data/RF/RF.all.pred.rds")
RF.all.test_y <- readRDS("~/Desktop/TDS/TDS_project/Data/RF/RF.all.test_y.rds")


pred <- as.numeric(RF.all.pred$pred)
actual <- as.numeric(RF.all.test_y$test_y)


cm1 <- plotCM(pred, actual)
plot(cm1)


# CM plot RF lymphoid
RF.lymph.pred <- readRDS("~/Desktop/TDS/TDS_project/Data/RF/RF.lymph.pred.rds")
RF.lymph.test_y <- readRDS("~/Desktop/TDS/TDS_project/Data/RF/RF.lymph.test_y.rds")

pred <- as.numeric(RF.lymph.pred$pred)
actual <- as.numeric(RF.lymph.test_y$test_y)

cm2 <- plotCM(pred, actual)
plot(cm2)

# CM plot RF myeloid
RF.myeloid.pred <- readRDS("~/Desktop/TDS/TDS_project/Data/RF/RF.myeloid.pred.rds")
RF.myeloid.test_y <- readRDS("~/Desktop/TDS/TDS_project/Data/RF/RF.myeloid.test_y.rds")

pred <- as.numeric(RF.myeloid.pred$pred)
actual <- as.numeric(RF.myeloid.test_y$test_y)

cm3 <- plotCM(pred, actual)
plot(cm3)

# CM plot RF other
RF.other.pred <- readRDS("~/Desktop/TDS/TDS_project/Data/RF/RF.other.pred.rds")
RF.other.test_y <- readRDS("~/Desktop/TDS/TDS_project/Data/RF/RF.other.test_y.rds")

pred <- as.numeric(RF.other.pred$pred)
actual <- as.numeric(RF.other.test_y$test_y)

cm4 <- plotCM(pred, actual)
plot(cm4)




p1 <- cm1 + ggtitle("RF Model - All")
p2 <- cm2 + ggtitle("RF Model - Lymphoid")
p3 <- cm3 + ggtitle("RF Model - Myeloid")
p4 <- cm4 + ggtitle("RF Model - Other")

# create the grid of plots
grid.arrange(p1, p2, p3, p4, ncol = 2, top = "Confusion Matrix Plots For Random Forest Models")
