library(gridExtra)
library(ggplot2)
library(caret)

plotCM <- function(pred, actual){
  hm <- data.frame(table(pred, actual))
  tmp <- table(actual)
  hm$Proportion <- c(hm[1,3]/tmp[1], hm[2,3]/tmp[1], hm[3,3]/tmp[2], hm[4,3]/tmp[2])
  
  colnames(hm) <- c("Predicted", "Actual", "Count", "Proportion")
  ggplot(hm, aes(Actual, Predicted, fill = Proportion)) + 
    geom_tile() +
    geom_text(aes(label = Count), color = "white", size = 5) +
    scale_fill_gradient(low = "#56B4E9", high = "#0072B2") +
    theme_bw()
}

setwd("~/Desktop/TDS/TDS_project/Data/RF")

## RF ALL
pred <- readRDS("~/Desktop/TDS/TDS_project/Data/RF/RF.all.pred.rds")
pred_probs <- readRDS("~/Desktop/TDS/TDS_project/Data/RF/RF.all.pred_probs.rds")
test_y <- readRDS("~/Desktop/TDS/TDS_project/Data/RF/RF.all.test_y.rds")
RF.all.test.diag_6_mon <- readRDS("~/Desktop/TDS/TDS_project/Data/RF/RF.all.test.diag_6_mon.rds")

summary(RF.all.test.diag_6_mon)

names(RF.all.test.diag_6_mon)[names(RF.all.test.diag_6_mon) == "test$diag_6_mon"] <- "test_diag_6_mon"
head(RF.all.test.diag_6_mon)

RF.all.test.diag_6_mon <- as.factor(RF.all.test.diag_6_mon$test$diag_6_mon)
pred <- as.factor(pred$pred)


`RF.all.test$diag_6_mon`

RF.all.test.diag_6_mon
str(pred)
str(RF.all.test.diag_6_mon)

plotCM(pred, RF.all.test.diag_6_mon)


class(pred)

# Find the union of levels present in both factors
all_levels <- union(levels(pred), levels(RF.all.test.diag_6_mon))

# Set the same levels for both factors
levels(pred) <- all_levels
levels(RF.all.test.diag_6_mon) <- all_levels

# Compute the confusion matrix
cm <- confusionMatrix(plotCM, mode = "everything")




cm <- confusionMatrix(pred, RF.all.test.diag_6_mon, mode = "everything")
print(cm)

#confusion_matrix <- as.data.frame(table(xgb.pred, test_y))

plt <- as.data.frame(cm$table)
plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))

a <- ggplot(plt, aes(Reference,Prediction, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction", title = "Confusion Matrix for Random Forest for Group All Cancer") +
  scale_x_discrete(labels=c("Negative","Positive")) +
  scale_y_discrete(labels=c("Positive","Negative"))
a
ggsave(filename="../output/RF_All_cm.plot.pdf", plot=a)