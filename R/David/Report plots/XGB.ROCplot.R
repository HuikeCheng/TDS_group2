rm(list=ls())

library(xgboost)
library(caret)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(pROC)

#ROC curve for XGB model - group ALL

xgb.all.test_y <- readRDS("~/Desktop/TDS/TDS_project/Data/XGB/xgb.all.test_y.rds")
xgb.all.pred_probs <- readRDS("~/Desktop/TDS/TDS_project/Data/XGB/xgb.all.pred_probs.rds")
xgb.all.pred <- readRDS("~/Desktop/TDS/TDS_project/Data/XGB/xgb.all.pred.rds")

pred <- as.factor(xgb.all.pred$pred)
test_y <- as.factor(xgb.all.test_y$test_y)
pred_probs <- as.numeric(xgb.all.pred_probs[, 2])

#ROC curve 
XGB.all.roc_obj <- roc(test_y, pred_probs)

XGB.all.auc_value <- auc(XGB.all.roc_obj)
XGB.all.auc_value

ggroc_plot.XGB.all <- ggroc(XGB.all.roc_obj, legacy.axes = TRUE,  color = "blue") + 
  ggtitle(paste0("ROC curve for XGB Group All\nAUC: ", round(XGB.all.auc_value, 3))) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype = "dashed", color = "gray") +
  labs(x = "False Positive Rate", y = "True Positive Rate")

print(ggroc_plot.XGB.all)
ggsave(filename="~/Desktop/TDS/TDS_project/Figures/XGB_All_roc.plot.png", plot=ggroc_plot.XGB.all, 
       width=8, height=6)




#ROC curve for XGB model - group Lymphoid

xgb.lymph.pred <- readRDS("~/Desktop/TDS/TDS_project/Data/XGB/xgb.lymph.pred.rds")
xgb.lymph.test_y <- readRDS("~/Desktop/TDS/TDS_project/Data/XGB/xgb.lymph.test_y.rds")
xgb.lymph.pred_probs <- readRDS("~/Desktop/TDS/TDS_project/Data/XGB/xgb.lymph.pred_probs.rds")

pred <- as.factor(xgb.lymph.pred$pred)
test_y <- as.factor(xgb.lymph.test_y$test_y)
pred_probs <- as.numeric(xgb.lymph.pred_probs[, 2])


#ROC curve 
XGB.lymph.roc_obj <- roc(test_y, pred_probs)

XGB.lymph.auc_value <- auc(XGB.lymph.roc_obj)
XGB.lymph.auc_value

ggroc_plot.XGB.lymph <- ggroc(XGB.lymph.roc_obj, legacy.axes = TRUE,  color = "blue") + 
  ggtitle(paste0("ROC curve for XGB Group Lymphoid\nAUC: ", round(XGB.lymph.auc_value, 3))) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype = "dashed", color = "gray") +
  labs(x = "False Positive Rate", y = "True Positive Rate")

print(ggroc_plot.XGB.lymph)
ggsave(filename="~/Desktop/TDS/TDS_project/Figures/XGB_lymphoid_roc.plot.png", plot=ggroc_plot.XGB.lymph, 
       width=8, height=6)


#ROC curve for XGB model - group myeloid

xgb.myeloid.pred <- readRDS("~/Desktop/TDS/TDS_project/Data/XGB/xgb.myeloid.pred.rds")
xgb.myeloid.pred_probs <- readRDS("~/Desktop/TDS/TDS_project/Data/XGB/xgb.myeloid.pred_probs.rds")
xgb.myeloid.test_y <- readRDS("~/Desktop/TDS/TDS_project/Data/XGB/xgb.myeloid.test_y.rds")

pred <- as.factor(xgb.myeloid.pred$pred)
test_y <- as.factor(xgb.myeloid.test_y$test_y)
pred_probs <- as.numeric(xgb.myeloid.pred_probs[, 2])


#ROC curve 
XGB.myeloid.roc_obj <- roc(test_y, pred_probs)

XGB.myeloid.auc_value <- auc(XGB.myeloid.roc_obj)
XGB.myeloid.auc_value

ggroc_plot.XGB.Myeloid <- ggroc(XGB.myeloid.roc_obj, legacy.axes = TRUE,  color = "blue") + 
  ggtitle(paste0("ROC curve for XGB Group Myeloid\nAUC: ", round(XGB.myeloid.auc_value, 3))) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype = "dashed", color = "gray") +
  labs(x = "False Positive Rate", y = "True Positive Rate")

print(ggroc_plot.XGB.Myeloid)
ggsave(filename="~/Desktop/TDS/TDS_project/Figures/XGB_Myeloid_roc.plot.png", plot=ggroc_plot.XGB.Myeloid, 
       width=8, height=6)



#ROC curve for XGB model - group Other

xgb.other.pred <- readRDS("~/Desktop/TDS/TDS_project/Data/XGB/xgb.other.pred.rds")
xgb.other.pred_probs <- readRDS("~/Desktop/TDS/TDS_project/Data/XGB/xgb.other.pred_probs.rds")
xgb.other.test_y <- readRDS("~/Desktop/TDS/TDS_project/Data/XGB/xgb.other.test_y.rds")

pred <- as.factor(xgb.other.pred$pred)
test_y <- as.factor(xgb.other.test_y$test_y)
pred_probs <- as.numeric(xgb.other.pred_probs[, 2])


#ROC curve 
XGB.other.roc_obj <- roc(test_y, pred_probs)

XGB.other.auc_value <- auc(XGB.other.roc_obj)
XGB.other.auc_value

ggroc_plot.XGB.other <- ggroc(XGB.other.roc_obj, legacy.axes = TRUE,  color = "blue") + 
  ggtitle(paste0("ROC curve for XGB Group Other\nAUC: ", round(XGB.other.auc_value, 3))) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype = "dashed", color = "gray") +
  labs(x = "False Positive Rate", y = "True Positive Rate")

print(ggroc_plot.XGB.other)
ggsave(filename="~/Desktop/TDS/TDS_project/Figures/XGB_other_roc.plot.png", plot=ggroc_plot.XGB.other, 
       width=8, height=6)



# Combine the ROC curve data into a single data frame
roc_data <- data.frame(
  FPR = c(1 - XGB.all.roc_obj$specificities,
          1 - XGB.lymph.roc_obj$specificities,
          1 - XGB.myeloid.roc_obj$specificities,
          1-  XGB.other.roc_obj$specificities),
  TPR = c(XGB.all.roc_obj$sensitivities,
          XGB.lymph.roc_obj$sensitivities,
          XGB.myeloid.roc_obj$sensitivities,
          XGB.other.roc_obj$sensitivities),
  Model = factor(rep(c("All", "Lymphoid", "Myeloid", "Other"),
                     times = c(length(XGB.all.roc_obj$sensitivities),
                               length(XGB.lymph.roc_obj$sensitivities),
                               length(XGB.myeloid.roc_obj$sensitivities),
                               length(XGB.other.roc_obj$sensitivities))))
)

# Plot the combined ROC curve
combined_ROC_plot <- ggplot(roc_data, aes(x = FPR, y = TPR, color = Model)) +
  geom_line() +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype = "dashed", color = "gray") +
  scale_color_manual(values = c("All" = "blue", "Lymphoid" = "red", "Myeloid" = "green", "Other" = "purple")) +
  labs(title = "Combined ROC Curve for XGB Models",
       x = "False Positive Rate",
       y = "True Positive Rate",
       color = "Model") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Add AUC values to the legend
combined_ROC_plot <- combined_ROC_plot + 
  guides(color = guide_legend(title = "Model",
                              override.aes = list(shape = rep(NA, 4)))) +
  annotate("text",
           x = c(0.75, 0.75, 0.75, 0.75),
           y = c(0.15, 0.10, 0.05, 0),
           label = paste("AUC =", 
                         round(c(XGB.all.auc_value,
                                 XGB.lymph.auc_value,
                                 XGB.myeloid.auc_value,
                                 XGB.other.auc_value), 3)),
           color = c("blue", "red", "green", "purple"))

# Print and save the combined ROC curve
print(combined_ROC_plot)
ggsave(filename = "~/Desktop/TDS/TDS_project/Figures/combined_XGB_ROC_plot.pdf", plot = combined_ROC_plot, 
       width=8, height=6)


####

# Combine data from all ROC curve objects into a single data frame
combined_roc_data_xgb <- data.frame(
  FPR = c(1 - XGB.all.roc_obj$specificities, 1 - XGB.lymph.roc_obj$specificities, 1 - XGB.myeloid.roc_obj$specificities, 1 - XGB.other.roc_obj$specificities),
  TPR = c(XGB.all.roc_obj$sensitivities, XGB.lymph.roc_obj$sensitivities, XGB.myeloid.roc_obj$sensitivities, XGB.other.roc_obj$sensitivities),
  AUC = c(rep(XGB.all.auc_value, length(XGB.all.roc_obj$sensitivities)),
          rep(XGB.lymph.auc_value, length(XGB.lymph.roc_obj$sensitivities)),
          rep(XGB.myeloid.auc_value, length(XGB.myeloid.roc_obj$sensitivities)),
          rep(XGB.other.auc_value, length(XGB.other.roc_obj$sensitivities))),
  Group = c(rep("All", length(XGB.all.roc_obj$sensitivities)),
            rep("Lymphoid", length(XGB.lymph.roc_obj$sensitivities)),
            rep("Myeloid", length(XGB.myeloid.roc_obj$sensitivities)),
            rep("Other", length(XGB.other.roc_obj$sensitivities)))
)

combined_roc_plot_xgb <- ggplot(combined_roc_data_xgb, aes(x = FPR, y = TPR, color = Group, group = Group)) +
  geom_line() +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype = "dashed", color = "gray") +
  labs(title = "ROC Curves for XGB Groups", x = "False Positive Rate", y = "True Positive Rate") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("blue", "red", "green", "purple")) +
  geom_text(data = data.frame(x = c(0.6, 0.6, 0.6, 0.6), 
                              y = c(0.45, 0.40, 0.35, 0.3),
                              labels = paste("AUC: ", round(c(XGB.all.auc_value, XGB.lymph.auc_value, XGB.myeloid.auc_value, XGB.other.auc_value), 3)),
                              Group = c("All", "Lymphoid", "Myeloid", "Other")),
            aes(x = x, y = y, label = labels, color = Group),
            size = 4,
            hjust = 0,
            show.legend = FALSE)

print(combined_roc_plot_xgb)
ggsave(filename = "~/Desktop/TDS/TDS_project/Figures/combined_XGB_ROC_plot.png", plot = combined_roc_plot_xgb, 
       width=8, height=6)


