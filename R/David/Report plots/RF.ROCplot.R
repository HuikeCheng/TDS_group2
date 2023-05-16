rm(list=ls())


library(randomForest)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(pROC)


setwd("~/Desktop/TDS/TDS_project/Data/RF")

#ROC for RF ALL


RF.all.pred <- readRDS("~/Desktop/TDS/TDS_project/Data/RF/RF.all.pred.rds")
RF.all.test_y <- readRDS("~/Desktop/TDS/TDS_project/Data/RF/RF.all.test_y.rds")
RF.all.pred_probs <- readRDS("~/Desktop/TDS/TDS_project/Data/RF/RF.all.pred_probs.rds")

colnames(RF.all.pred_probs)
pred <- as.factor(RF.all.pred$pred)
test_y <- as.factor(RF.all.test_y$test_y)
pred_probs <- as.numeric(RF.all.pred_probs[, 2])


#ROC curve 
RF.all.roc_obj <- roc(test_y, pred_probs)

RF.all.auc_value <- auc(RF.all.roc_obj)
RF.all.auc_value

ggroc_plot.RF.all <- ggroc(RF.all.roc_obj, legacy.axes = TRUE,  color = "blue") + 
  ggtitle(paste0("ROC curve for RF Group All\nAUC: ", round(RF.all.auc_value, 3))) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype = "dashed", color = "gray") +
  labs(x = "False Positive Rate", y = "True Positive Rate")

print(ggroc_plot.RF.all)
ggsave(filename="~/Desktop/TDS/TDS_project/Figures/RF_All_roc.plot.png", plot=ggroc_plot.RF.all, 
       width=8, height=6)


#ROC for RF lymphoid

RF.lymph.pred <- readRDS("~/Desktop/TDS/TDS_project/Data/RF/RF.lymph.pred.rds")
RF.lymph.test_y <- readRDS("~/Desktop/TDS/TDS_project/Data/RF/RF.lymph.test_y.rds")
RF.lymph.pred_probs <- readRDS("~/Desktop/TDS/TDS_project/Data/RF/RF.lymph.pred_probs.rds")

colnames(RF.lymph.pred_probs)
pred <- as.factor(RF.lymph.pred$pred)
test_y <- as.factor(RF.lymph.test_y$test_y)
pred_probs <- as.numeric(RF.lymph.pred_probs[, 2])


#ROC curve
RF.lymph.roc_obj <- roc(test_y, pred_probs)

RF.lymph.auc_value <- auc(RF.lymph.roc_obj)
RF.lymph.auc_value

ggroc_plot.RF.lymph <- ggroc(RF.lymph.roc_obj, legacy.axes = TRUE,  color = "blue") + 
  ggtitle(paste0("ROC curve for RF Group Lymphoid\nAUC: ", round(RF.lymph.auc_value, 3))) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype = "dashed", color = "gray") +
  labs(x = "False Positive Rate", y = "True Positive Rate")

print(ggroc_plot.RF.lymph)
ggsave(filename="~/Desktop/TDS/TDS_project/Figures/RF_lymph_roc.plot.png", plot=ggroc_plot.RF.lymph, 
       width=8, height=6)

#ROC for RF Myeloid

RF.myeloid.pred <- readRDS("~/Desktop/TDS/TDS_project/Data/RF/RF.myeloid.pred.rds")
RF.myeloid.pred_probs <- readRDS("~/Desktop/TDS/TDS_project/Data/RF/RF.myeloid.pred_probs.rds")
RF.myeloid.test_y <- readRDS("~/Desktop/TDS/TDS_project/Data/RF/RF.myeloid.test_y.rds")

colnames(RF.all.pred_probs)
pred <- as.factor(RF.myeloid.pred$pred)
test_y <- as.factor(RF.myeloid.test_y$test_y)
pred_probs <- as.numeric(RF.myeloid.pred_probs[, 2])


RF.myeloid.roc_obj <- roc(test_y, pred_probs)

RF.myeloid.auc_value <- auc(RF.myeloid.roc_obj)
RF.myeloid.auc_value

ggroc_plot.RF.myeloid <- ggroc(RF.myeloid.roc_obj, legacy.axes = TRUE,  color = "blue") + 
  ggtitle(paste0("ROC curve for RF Group Myeloid\nAUC: ", round(RF.myeloid.auc_value, 3))) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype = "dashed", color = "gray") +
  labs(x = "False Positive Rate", y = "True Positive Rate")

print(ggroc_plot.RF.myeloid)
ggsave(filename="~/Desktop/TDS/TDS_project/Figures/RF_myeloid_roc.plot.png", plot=ggroc_plot.RF.myeloid, 
       width=8, height=6)

#ROC curve for other

RF.other.pred <- readRDS("~/Desktop/TDS/TDS_project/Data/RF/RF.other.pred.rds")
RF.other.pred_probs <- readRDS("~/Desktop/TDS/TDS_project/Data/RF/RF.other.pred_probs.rds")
RF.other.test_y <- readRDS("~/Desktop/TDS/TDS_project/Data/RF/RF.other.test_y.rds")

colnames(RF.all.pred_probs)
pred <- as.factor(RF.other.pred$pred)
test_y <- as.factor(RF.other.test_y$test_y)
pred_probs <- as.numeric(RF.other.pred_probs[, 2])


#ROC curve
RF.other.roc_obj <- roc(test_y, pred_probs)

RF.other.auc_value <- auc(RF.other.roc_obj)

ggroc_plot.RF.other <- ggroc(RF.other.roc_obj, legacy.axes = TRUE,  color = "blue") + 
  ggtitle(paste0("ROC curve for RF Group Other\nAUC: ", round(RF.other.auc_value, 3))) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype = "dashed", color = "gray") +
  labs(x = "False Positive Rate", y = "True Positive Rate")

print(ggroc_plot.RF.other)
ggsave(filename="~/Desktop/TDS/TDS_project/Figures/RF_other_roc.plot.png", plot=ggroc_plot.RF.other, 
       width=8, height=6)





#combine all into single graph

library(ggplot2)

# Combine data from all ROC curve objects into a single data frame
combined_roc_data <- data.frame(
  FPR = c(1 - RF.all.roc_obj$specificities, 1 - RF.lymph.roc_obj$specificities, 1 - RF.myeloid.roc_obj$specificities, 1 - RF.other.roc_obj$specificities),
  TPR = c(RF.all.roc_obj$sensitivities, RF.lymph.roc_obj$sensitivities, RF.myeloid.roc_obj$sensitivities, RF.other.roc_obj$sensitivities),
  AUC = c(rep(RF.all.auc_value, length(RF.all.roc_obj$sensitivities)),
          rep(RF.lymph.auc_value, length(RF.lymph.roc_obj$sensitivities)),
          rep(RF.myeloid.auc_value, length(RF.myeloid.roc_obj$sensitivities)),
          rep(RF.other.auc_value, length(RF.other.roc_obj$sensitivities))),
  Group = c(rep("All", length(RF.all.roc_obj$sensitivities)),
            rep("Lymphoid", length(RF.lymph.roc_obj$sensitivities)),
            rep("Myeloid", length(RF.myeloid.roc_obj$sensitivities)),
            rep("Other", length(RF.other.roc_obj$sensitivities)))
)

combined_roc_plot <- ggplot(combined_roc_data, aes(x = FPR, y = TPR, color = Group, group = Group)) +
  geom_line() +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype = "dashed", color = "gray") +
  labs(title = "ROC Curves for RF Groups", x = "False Positive Rate", y = "True Positive Rate") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("blue", "red", "green", "purple")) +
  geom_text(data = data.frame(x = c(0.6, 0.6, 0.6, 0.6), 
                              y = c(0.45, 0.40, 0.35, 0.3),
                              labels = paste("AUC: ", round(c(RF.all.auc_value, RF.lymph.auc_value, RF.myeloid.auc_value, RF.other.auc_value), 3)),
                              Group = c("All", "Lymphoid", "Myeloid", "Other")),
            aes(x = x, y = y, label = labels, color = Group),
            size = 4,
            hjust = 0,
            show.legend = FALSE)

print(combined_roc_plot)
ggsave(filename = "~/Desktop/TDS/TDS_project/Figures/combined_RF_ROC_plot.png", plot = combined_roc_plot, 
       width=8, height=6)
