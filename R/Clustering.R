###### load packages
library(tidyverse)
library(survival)
library(cvwrapr)
library(nnet)
library(pROC)

library(ggplot2);library(dplyr);library(tidyr);library(pROC)

#load data
train_df <- readRDS("../data/Imputed_train_V_v3.rds")
test_df <- readRDS("../data/Imputed_test_V_v3.rds")
matched_df <- readRDS("../data/Matched_data_v3.rds")

#select blood counts
bc_train_df <- train_df[,12:32] %>% scale()
bc_test_df <- test_df[,12:32] %>% scale()

#kmeans
set.seed(55)
cluster.blood <- kmeans(bc_train_df,4,nstart=14)

table(cluster.blood$cluster, train_df$lymphoid)

cluster.blood$cluster <- as.factor(cluster.blood$cluster)

#add cluster membership column to train_df
train_df$kmeans_clus <- cluster.blood$cluster

###Determine variable importance
#boxplots
loop<-colnames(bc_train_df)
plots<-list()
for (col in loop) {
  plot<-ggplot(train_df, aes(x = kmeans_clus, y = .data[[col]])) +
    geom_boxplot() +
    labs(x = "Cluster", y = col)  # set the y-axis label to the column name
  plots[[col]] <- plot
  ggsave(paste0("boxplots/kmeans/", col, ".png"), plot = plot, width = 8, height = 4)
}

# # Compute WCSS for each cluster
# wcss <- sapply(unique(clusters), function(c) sum((train_df[clusters == c,] - colMeans(train_df[clusters == c,]))^2))
# 
# # Create a dataframe with the WCSS values
# wcss_df <- data.frame(wcss = wcss, kmeans_clus = unique(train_df$kmeans_clus))
# 
# # Fit linear regression model between WCSS and each variable
# model <- lm(cluster.blood$withinss ~ ., data = as.data.frame(bc_train_df))
# 
# # Extract coefficients as variable importance measures
# var_importance <- coef(model)[-1]

#PCA
pca_model <- prcomp(bc_train_df, center = TRUE, scale. = TRUE)

# View the summary of the PCA model
summary(pca_model)

# Create a data frame with the first two principal components and kmeans cluster
library(ggplot2)

# Create a data frame with the first two principal components and kmeans cluster
pca_df <- data.frame(pca_model$x[,1:2], kmeans_clus = train_df$kmeans_clus)

# Set color palette for clusters
colors <- c("#0072B2", "#D55E00", "#009E73", "#E69F00")

# Plot the first two principal components, colored by kmeans cluster
p <- ggplot(pca_df, aes(x = PC1, y = PC2, color = kmeans_clus)) + 
  geom_point(size = 3, alpha = 0.8) +
  scale_color_manual(values = colors) +
  labs(x = "PC1", y = "PC2", color = "Cluster") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(size = 0.8, color = "black"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top",
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10)) +
  guides(colour = guide_legend(override.aes = list(size=3)))


###Test df
#determine the cluster with the closest centroid (euclidean)
distances <- apply(bc_test_df, 1, function(x) apply(cluster.blood$centers, 1, function(y) sum((x - y)^2)))

#assign to test_df
test_df$kmeans_clus <- as.factor(apply(distances, 2, which.min))

#add a column for any cancer type
test_df$any <- ifelse(test_df$lymphoid == 1 | test_df$myeloid == 1 | test_df$other == 1, 1, 0)
matched_df$any <- ifelse(matched_df$lymphoid == 1 | matched_df$myeloid == 1 | matched_df$other == 1, 1, 0)


#create matched dataset with cluster membership
matched_df <- left_join(matched_df, train_df %>% select(eid, kmeans_clus), by="eid")

#save clusters
saveRDS(train_df, file="../data/clus_membership_kmeans_train.rds")
saveRDS(test_df, file="../data/clus_membership_kmeans_test.rds")

#This loop iterates over the two outcomes 'lymphoid' and 'myeloid'. For each outcome, it fits a logistic regression model using the corresponding variable and the other predictor variables in the dataset. It then predicts probabilities for the outcome variable using the fitted model and calculates the ROC curve and AUC. Finally, it plots the ROC curve, adds a diagonal line for reference, and prints the AUC value.
for (outcome in c("lymphoid", "myeloid","other","any")) {
  # Fit logistic regression model
  model <- glm(paste("any", "~ kmeans_clus + Sex + Smoke_status.0.0 + BMI.0.0 + Age_recr_continuous.0.0 + Alcohol_freq.0.0 + highest_Education0"), data = matched_df, family = "binomial")
  
  # Predict probabilities
  predict <- predict(model, test_df, type = "response")
  
  # Calculate ROC curve and AUC
  roc_curve <- roc(test_df[[outcome]], predict)
  auc_value <- auc(roc_curve)
  
  # Plot ROC curve
  plot(roc_curve, main = paste("ROC Curve for", outcome), print.auc = TRUE)
  
  # Print AUC value
  print(paste("AUC for", outcome, ":", round(auc_value, 3)))
  
  # Print table of p-value of kmeans_clus variable
  p_values <- summary(model)$coefficients[c("kmeans_clus2", "kmeans_clus3", "kmeans_clus4"), "Pr(>|z|)"]
  table_p_values <- data.frame(P_Value = p_values)
  print(table_p_values)
}


#silhouette plots of different methods
dist_methods <- c("euclidean")

# Calculate silhouette scores for each distance measure and number of clusters
library(cluster); library(factoextra)
sil_scores <- lapply(dist_methods, function(d) {
  
  sil_scores<-lapply(2:10, function(k) {
    pam <- pam(bc_train_df, k)
  })
  
  sil <- silhouette(km$cluster, dist(bc_train_df, method = "euclidean"))
  mean(sil[,3])
  
})

# Plot the results
sil_matrix <- do.call(rbind, sil_scores)
fviz_cluster(list(data = train_df, sil_matrix), geom = "line", labelsize = 12, labelcol = "black", stand = FALSE,
             ggtheme = theme_classic(), main = "Silhouette Scores by Distance Method and Number of Clusters",
             xlab = "Number of Clusters", ylab = "Silhouette Score", legend.title = "Distance Method",
             legend.position = "bottom")


k <- 2:10
pam_obj <- lapply(2:10, function(k) pam(iris, k))
sil_scores <- lapply(pam_obj, function(pam_obj) silhouette(pam_obj))
fviz_silhouette(sil_scores)

###Clustering
###### load data
pam <- readRDS("../output/PAM.rds")
pamCons <- readRDS("../output/PAMCons2.rds")
kmCons <- readRDS("../output/KMCons.rds")
km <- readRDS("../data/clus_membership_kmeans_train.rds")
df <- readRDS("../data/Imputed_train_V_v3.rds")
df_test <- readRDS("../data/Imputed_test_clus.rds")

###### cleaning
pam <- as.data.frame(pam)
colnames(pam) <- paste0("pam", 2:6)
colnames(pamCons) <- paste0("pamCons", 2:6)
colnames(kmCons) <- paste0("kmCons", 2:6)
colnames(km)[37] <- "km4"

###### pick clustering to look at
## KMCons: 4
## PAMCons: 4
## PAM: 4

###### look at clustering across methods
table(pam$pam4, pamCons$pamCons4)
table(kmCons$kmCons4, pam$pam4)

###### prepare dataset
df <- cbind(df[,c(1,3:32)], km$km4, pam$pam4, kmCons$kmCons4, pamCons$pamCons4)
colnames(df) <- recodeVarName(colnames(df))
colnames(df)[32:35] <- c("km4", "pam4", "kmCons4", "pamCons4")

################################################################################
################## Visualise blood counts and clustering #######################
################################################################################
bcdata <- df[,11:35]
bcdata$km4 <- as.numeric(bcdata$km4)
#saveRDS(bcdata, file = "../output/ClusBC.rds")

# ## box plot
# pdf("../output/kmCons4.pdf")
# par(mfrow=c(3,3))
# for (i in 1:21){
#   boxplot(bcdata[,i] ~ as.factor(bcdata$kmCons4), ylab = colnames(bcdata)[i], xlab = "")
# }
# dev.off()

#### find good point of view for presenting clustering
# ggplot(data = bcdata, aes(x=`Hb conc.`, y=PDW, color = as.factor(kmCons4))) +
#   geom_point(alpha = 0.5)
# ggplot(data = bcdata, aes(x=`Hb conc.`, y=PDW, color = as.factor(pamCons4))) +
#   geom_point(alpha = 0.5)
# ggplot(data = bcdata, aes(x=`Hb conc.`, y=PDW, color = as.factor(pam4))) +
#   geom_point(alpha = 0.5)
# 
# ggplot(data = bcdata, aes(x=MCV, y=MSCV, color = as.factor(kmCons4))) +
#   geom_point(alpha = 0.5)
# ggplot(data = bcdata, aes(x=MCV, y=MSCV, color = as.factor(pamCons4))) +
#   geom_point(alpha = 0.5)
# ggplot(data = bcdata, aes(x=MCV, y=MSCV, color = as.factor(pam4))) +
#   geom_point(alpha = 0.5)

################################################################################
################ Number of each subtype in clusters ############################
################################################################################
bcdata <- readRDS("../output/ClusBC.rds")
ClusOut <- cbind(df[,c(3, 33:36)], bcdata[,22:25])
#saveRDS(ClusOut, "../output/ClusOut.rds")
Outdf <- readRDS("../output/ClusOut.rds")

km4 <- Outdf %>% group_by(km4) %>% summarise(All = sum(diag_6_mon),
                                               Lymphoid = sum(lymphoid),
                                               Myeloid = sum(myeloid),
                                               Other = sum(other),
                                               H_D_cell = sum(H_D_cell),
                                               N = n())

pam4 <- Outdf %>% group_by(pam4) %>% summarise(All = sum(diag_6_mon),
                                               Lymphoid = sum(lymphoid),
                                               Myeloid = sum(myeloid),
                                               Other = sum(other),
                                               H_D_cell = sum(H_D_cell),
                                               N = n())
kmCons4 <- Outdf %>% group_by(kmCons4) %>% summarise(All = sum(diag_6_mon),
                                                     Lymphoid = sum(lymphoid),
                                                     Myeloid = sum(myeloid),
                                                     Other = sum(other),
                                                     H_D_cell = sum(H_D_cell),
                                                     N = n())
pamCons4 <- Outdf %>% group_by(pamCons4) %>% summarise(All = sum(diag_6_mon),
                                                     Lymphoid = sum(lymphoid),
                                                     Myeloid = sum(myeloid),
                                                     Other = sum(other),
                                                     H_D_cell = sum(H_D_cell),
                                                     N = n())

for (i in 2:6){
  km4[,i] <- km4[,i]/km4$N
  kmCons4[,i] <- kmCons4[,i]/kmCons4$N
  pam4[,i] <- pam4[,i]/pam4$N
  pamCons4[,i] <- pamCons4[,i]/pamCons4$N
}

### collect results
km4$km4 <- paste0(rep("KM_", 4), 1:4)
pam4$pam4 <- paste0(rep("PAM_", 4), 1:4)
kmCons4$kmCons4 <- paste0(rep("C-KM_", 4), 1:4)
pamCons4$pamCons4 <- paste0(rep("C-PAM_", 4), 1:4)
colnames(km4)[1] <- "Method"
colnames(pam4)[1] <- "Method"
colnames(kmCons4)[1] <- "Method"
colnames(pamCons4)[1] <- "Method"
df_sumout <- rbind(km4, pam4, kmCons4, pamCons4)
#saveRDS(df_sumout, "../output/Clus_sumout.rds")

################################################################################
############################## Multinomial model ###############################
################################################################################
bcdata <- readRDS("../output/ClusBC.rds")
model_df <- readRDS("../output/ClusBC.rds")
model_df <- model_df[,-26]
# pcdf <- model_df[,-c(23:24)]
# modelpc <- multinom(pamCons4 ~ ., data = pcdf)
kcdf <- model_df[,-c(22)]
modelkc <- multinom(kmCons4 ~ ., data = kcdf)
pamdf <- model_df[,-c(23)]
modelpam <- multinom(pam4 ~ ., data = pamdf)

###
summary(modelkc)

### predict
# Predicting the values for train dataset
#bcdata$predpc <- predict(modelpc, newdata = bcdata, "class")
bcdata$predkc <- predict(modelkc, newdata = bcdata, "class")
bcdata$predpam <- predict(modelpam, newdata = bcdata, "class")

# Building classification table
tab <- table(bcdata$pam4, bcdata$predpam) # 99.45
tab <- table(bcdata$kmCons4, bcdata$predkc) # 99.48
tab <- table(bcdata$pamCons4, bcdata$predpc) # 97

# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(tab))/sum(tab))*100,2)

### get coef
#out_pc <- exp(coef(modelpc))
out_kc <- exp(coef(modelkc))
out_pam <- exp(coef(modelpam))

################################################################################
##################### get cluster_mem for test set #############################
################################################################################
colnames(df_test)[8:28] <- recodeVarName(colnames(df_test)[8:28])
#df_test$pamCons4 <- predict(modelpc, newdata = df_test, "class")
df_test$kmCons4 <- predict(modelkc, newdata = df_test, "class")
df_test$pam4 <- predict(modelpam, newdata = df_test, "class")

#saveRDS(df_test, "../data/df_test_clus.rds")

################################################################################
################################### CoxPH ######################################
################################################################################
bcdata <- readRDS("../output/ClusBC.rds")
df <- readRDS("../data/Matched_data_v2.rds")
df_eth <- readRDS("../data/Matched_data_v3.rds")
df_test <- readRDS("../data/df_test_clus.rds")
###### change ethnicity column
df$Ethnicity.0.0 <- df_eth$Ethnicity.0.0
###### remove not needed
rm(df_eth)
###### add the clus_mem
clus_df <- bcdata[,c(22:26)]
df <- merge(df, clus_df, by = "eid", all.x = TRUE)
df$km4 <- as.factor(df$km4)
df$pam4 <- as.factor(df$pam4)
df$kmCons4 <- as.factor(df$kmCons4)
df$pamCons4 <- as.factor(df$pamCons4)
############### selected vars
model_df <- df[,c(3:8, 10:12,60)]
summary(model_df)
# define reference level
model_df <- model_df %>% mutate(Smoke_status.0.0 = relevel(Smoke_status.0.0, ref = "Never"),
                                Ethnicity.0.0 = relevel(Ethnicity.0.0, ref = "White"),
                                Alcohol_freq.0.0 = relevel(Alcohol_freq.0.0, ref = "never"),
                                highest_Education0 = relevel(highest_Education0, ref = "Other_professional"))
colnames(model_df) <- recodeVarName(colnames(model_df))
######### fit full model
model <- coxph(Surv(time_to_diagnosis, diag_6_mon) ~ ., 
               data = model_df)
summary(model)

######## prediction
test_X <- df_test[, c(5:11, 37:44)]
test_Y <- Surv(time = df_test$time_to_diagnosis, event = df_test$diag_6_mon, type = "right")
train_Y <- Surv(time = model_df$time_to_diagnosis, event = model_df$diag_6_mon, type = "right")
### change colnames temporarily
colnames(test_X)[which(colnames(test_X) == "pamCons_mn")] <- "pamCons4"
colnames(test_X)[which(colnames(test_X) == "pamCons4")] <- "pamCons_mn"
colnames(test_X)[which(colnames(test_X) == "pc_nn")] <- "pamCons4"
colnames(test_X)[which(colnames(test_X) == "pamCons4")] <- "pc_nn"

yhat <- predict(model, newdata = model_df, type = "risk")
summary(yhat)
getCindex(pred = yhat, y = train_Y) # 0.61

##### output
out_coxph <- cbind(round(summary(model)$coefficients, 2), round(exp(confint(model)), 2))
# colnames(out_coxph) <- c("coef", "HR", "se", "z", "p", "CI_l", "CI_u")
# out_coxph <- as.data.frame(out_coxph)
# out_coxph$varname <- rownames(out_coxph)
# out_coxph <- out_coxph[c(1,11,10,4:9,3,2,14,13,15,16,12,17:22,23:28),]
# out_coxph$varname <- recodeVarName(out_coxph$varname)
# #saveRDS(out_coxph, "../output/out_coxph.rds")

################################################################################
##################################### Logistic #################################
################################################################################
######### Load data
bcdata <- readRDS("../output/ClusBC.rds")
clus_df <- bcdata[,c(22:26)]
df <- readRDS("../data/Matched_data_v3.rds")
df_cat <- readRDS("../data/Matched_data_v2.rds")
# change categorical columns back
df_cat <- df_cat[,c(1,5,6,11,12)]
df <- df[,-c(5,6,10,11)]
df <- merge(df, df_cat, by = "eid", all.x = TRUE)
# remove not needed
rm(df_cat)
###### add the clus_mem
df <- df[,-c(8:28)]
df <- merge(df, clus_df, by = "eid", all.x = TRUE)
df$km4 <- as.factor(df$km4)
df$pam4 <- as.factor(df$pam4)
df$kmCons4 <- as.factor(df$kmCons4)
df$pamCons4 <- as.factor(df$pamCons4)
#saveRDS(df, "../data/df_clus.rds")
################################################################################
###################################### All #####################################
################################################################################
library(tidyverse)
df <- readRDS("../data/df_clus.rds")
df_test <- readRDS("../data/df_test_clus.rds")
###### testX
test_X <- df_test[,c(5:11, 37:44)]
######
cov_df <- df[,c(3, 5:7, 12:15, 16:19)]
cov_df$diag_6_mon <- as.factor(cov_df$diag_6_mon)

cov_df <- cov_df %>% mutate(Smoke_status.0.0 = relevel(Smoke_status.0.0, ref = "Never"),
                            Ethnicity.0.0 = relevel(Ethnicity.0.0, ref = "White"),
                            Alcohol_freq.0.0 = relevel(Alcohol_freq.0.0, ref = "never"),
                            highest_Education0 = relevel(highest_Education0, ref = "Other_professional"))
summary(cov_df)
colnames(cov_df) <- recodeVarName(colnames(cov_df))
##### model
md <- glm(diag_6_mon ~ .-km4-pam4-kmCons4, data = cov_df, family = binomial)
summary(md)

##### results
result_full <- cbind(exp(coef(md)), exp(confint(md)), summary(md)$coefficients[,4])
out_full <- cbind(round(result_full[,1:3], 2), signif(result_full[,4], 3))
out_full <- data.frame(out_full)
colnames(out_full) <- c("Estimate", "2.5 %", "97.5 %", "p-value")
out_all <- out_full[24:26,]
out_all$varname <- rownames(out_all)
out_all$outcome <- rep("All", 3)
out_all_pc <- out_all

out_all <- rbind(out_all_km, out_all_pam, out_all_kc, out_all_pc)
#saveRDS(out_all, "../output/out_clus_all.rds")

### change colnames temporarily
colnames(test_X)[which(colnames(test_X) == "kmCons_mn")] <- "kmCons4"
colnames(test_X)[which(colnames(test_X) == "kmCons4")] <- "kmCons_mn"
colnames(test_X)[which(colnames(test_X) == "pc_nn")] <- "pamCons4"
colnames(test_X)[which(colnames(test_X) == "pamCons4")] <- "pc_nn"

##### prediction
yhat <- predict(md, newdata = test_X, type = "response")

plot.roc(df_test$diag_6_mon, as.numeric(yhat), 
         print.auc = TRUE)
# other metrics
pred <- as.factor(ifelse(as.numeric(yhat) > 0.5, 1, 0))
confusionMatrix(pred, as.factor(df_test$diag_6_mon), mode = "everything", positive="1")
# ROC object
roc_obj_all_kc <- roc(df_test$diag_6_mon, as.numeric(yhat))

####### ROC curves
combined_roc_data <- data.frame(
  FPR = c(1 - roc_obj_all_km$specificities, 1 - roc_obj_all_pam$specificities, 1 - roc_obj_all_kc$specificities, 1 - roc_obj_all_pc$specificities),
  TPR = c(roc_obj_all_km$sensitivities, roc_obj_all_pam$sensitivities, roc_obj_all_kc$sensitivities, roc_obj_all_pc$sensitivities),
  AUC = c(rep(as.numeric(roc_obj_all_km$auc), length(roc_obj_all_km$sensitivities)),
          rep(as.numeric(roc_obj_all_pam$auc), length(roc_obj_all_pam$sensitivities)),
          rep(as.numeric(roc_obj_all_kc$auc), length(roc_obj_all_kc$sensitivities)),
          rep(as.numeric(roc_obj_all_pc$auc), length(roc_obj_all_pc$sensitivities))),
  Method = c(rep("KM", length(roc_obj_all_km$sensitivities)),
            rep("PAM", length(roc_obj_all_pam$sensitivities)),
            rep("C-KM", length(roc_obj_all_kc$sensitivities)),
            rep("C-PAM", length(roc_obj_all_pc$sensitivities))),
  Outcome = rep("All", 61113)
)
#saveRDS(combined_roc_data, "../output/roc_clus_all.rds")


################################################################################
################################### Lymphoid ###################################
################################################################################
####### prepare model_data
# get lymphoid cases vs controls
lym_df <- df %>% filter(!(diag_6_mon == 1 & lymphoid == 0))
# check
table(lym_df$diag_6_mon,lym_df$lymphoid)
# select the vars needed
lym_df <- lym_df[,c(8, 5:7, 12:15, 16:19)]
lym_df$lymphoid <- as.factor(lym_df$lymphoid)
###### unregularised
lym_df <- lym_df %>% mutate(Smoke_status.0.0 = relevel(Smoke_status.0.0, ref = "Never"),
                            Ethnicity.0.0 = relevel(Ethnicity.0.0, ref = "White"),
                            Alcohol_freq.0.0 = relevel(Alcohol_freq.0.0, ref = "never"),
                            highest_Education0 = relevel(highest_Education0, ref = "Other_professional"))
summary(lym_df)
colnames(lym_df) <- recodeVarName(colnames(lym_df))
##### model
md1 <- glm(lymphoid ~ .-km4-pam4-kmCons4, data = lym_df, family = binomial)
summary(md1)

##### results
result_full <- cbind(exp(coef(md1)), exp(confint(md1)), summary(md1)$coefficients[,4])
out_full <- cbind(round(result_full[,1:3], 2), signif(result_full[,4], 3))
out_full <- data.frame(out_full)
colnames(out_full) <- c("Estimate", "2.5 %", "97.5 %", "p-value")
out_lym <- out_full[24:26,]
out_lym$varname <- rownames(out_lym)
out_lym$outcome <- rep("Lymphoid", 3)
out_lym_pc <- out_lym

out_lym <- rbind(out_lym_km, out_lym_pam, out_lym_kc, out_lym_pc)
#saveRDS(out_lym, "../output/out_clus_lym.rds")

##### prediction
yhat <- predict(md1, newdata = test_X, type = "response")

plot.roc(df_test$lymphoid, as.numeric(yhat), 
         print.auc = TRUE)

# confusion matrix
3157/(3157+5485)
pred <- as.factor(ifelse(as.numeric(yhat) > 0.365, 1, 0))
# other metrics
confusionMatrix(pred, as.factor(df_test$lymphoid), mode = "everything", positive="1")
# ROC object
roc_obj_lym_pc <- roc(df_test$lymphoid, as.numeric(yhat))

####### ROC curves
combined_roc_data <- data.frame(
  FPR = c(1 - roc_obj_lym_km$specificities, 1 - roc_obj_lym_pam$specificities, 1 - roc_obj_lym_kc$specificities, 1 - roc_obj_lym_pc$specificities),
  TPR = c(roc_obj_lym_km$sensitivities, roc_obj_lym_pam$sensitivities, roc_obj_lym_kc$sensitivities, roc_obj_lym_pc$sensitivities),
  AUC = c(rep(as.numeric(roc_obj_lym_km$auc), length(roc_obj_lym_km$sensitivities)),
          rep(as.numeric(roc_obj_lym_pam$auc), length(roc_obj_lym_pam$sensitivities)),
          rep(as.numeric(roc_obj_lym_kc$auc), length(roc_obj_lym_kc$sensitivities)),
          rep(as.numeric(roc_obj_lym_pc$auc), length(roc_obj_lym_pc$sensitivities))),
  Method = c(rep("KM", length(roc_obj_lym_km$sensitivities)),
             rep("PAM", length(roc_obj_lym_pam$sensitivities)),
             rep("C-KM", length(roc_obj_lym_kc$sensitivities)),
             rep("C-PAM", length(roc_obj_lym_pc$sensitivities))),
  Outcome = rep("Lymphoid", 61113)
)
#saveRDS(combined_roc_data, "../output/roc_clus_lym.rds")
# remove df not needed
rm(lym_df)
################################################################################
#################################### Myeloid ###################################
################################################################################
####### prepare model_data
# get mye cases vs controls
mye_df <- df %>% filter(!(diag_6_mon == 1 & myeloid == 0))
# check
table(mye_df$diag_6_mon, mye_df$myeloid)
# select the vars needed
mye_df <- mye_df[,c(9, 5:7, 12:15, 16:19)]
summary(mye_df)
mye_df$myeloid <- as.factor(mye_df$myeloid)
###### unregularised
mye_df <- mye_df %>% mutate(Smoke_status.0.0 = relevel(Smoke_status.0.0, ref = "Never"),
                            Ethnicity.0.0 = relevel(Ethnicity.0.0, ref = "White"),
                            Alcohol_freq.0.0 = relevel(Alcohol_freq.0.0, ref = "never"),
                            highest_Education0 = relevel(highest_Education0, ref = "Other_professional"))
summary(mye_df)
colnames(mye_df) <- recodeVarName(colnames(mye_df))
##### model
md2 <- glm(myeloid ~ .-km4-pam4-kmCons4, data = mye_df, family = binomial)
summary(md2)

##### result
result_full <- cbind(exp(coef(md2)), exp(confint(md2)), summary(md2)$coefficients[,4])
out_full <- cbind(round(result_full[,1:3], 2), signif(result_full[,4], 3))
out_full <- data.frame(out_full)
colnames(out_full) <- c("Estimate", "2.5 %", "97.5 %", "p-value")
out_mye <- out_full[24:26,]
out_mye$varname <- rownames(out_mye)
out_mye$outcome <- rep("myephoid", 3)
out_mye_pc <- out_mye

out_mye <- rbind(out_mye_km, out_mye_pam, out_mye_kc, out_mye_pc)
#saveRDS(out_mye, "../output/out_clus_mye.rds")

##### prediction
yhat <- predict(md2, newdata = test_X, type = "response")

plot.roc(df_test$myeloid, as.numeric(yhat), 
         print.auc = TRUE)

# confusion matrix
1035/(1035+5458) # 0.1594
pred <- as.factor(ifelse(as.numeric(yhat) > 0.159, 1, 0))
# other metrics
confusionMatrix(pred, as.factor(df_test$myeloid), mode = "everything", positive="1")
# ROC object
roc_obj_mye_pc <- roc(df_test$myeloid, as.numeric(yhat))
####### ROC curves
combined_roc_data <- data.frame(
  FPR = c(1 - roc_obj_mye_km$specificities, 1 - roc_obj_mye_pam$specificities, 1 - roc_obj_mye_kc$specificities, 1 - roc_obj_mye_pc$specificities),
  TPR = c(roc_obj_mye_km$sensitivities, roc_obj_mye_pam$sensitivities, roc_obj_mye_kc$sensitivities, roc_obj_mye_pc$sensitivities),
  AUC = c(rep(as.numeric(roc_obj_mye_km$auc), length(roc_obj_mye_km$sensitivities)),
          rep(as.numeric(roc_obj_mye_pam$auc), length(roc_obj_mye_pam$sensitivities)),
          rep(as.numeric(roc_obj_mye_kc$auc), length(roc_obj_mye_kc$sensitivities)),
          rep(as.numeric(roc_obj_mye_pc$auc), length(roc_obj_mye_pc$sensitivities))),
  Method = c(rep("KM", length(roc_obj_mye_km$sensitivities)),
             rep("PAM", length(roc_obj_mye_pam$sensitivities)),
             rep("C-KM", length(roc_obj_mye_kc$sensitivities)),
             rep("C-PAM", length(roc_obj_mye_pc$sensitivities))),
  Outcome = rep("Myeloid", 61096)
)
# saveRDS(combined_roc_data, "../output/roc_clus_mye.rds")
# remove uneeded df
rm(mye_df)
################################################################################
##################################### other ####################################
################################################################################
####### prepare model_data
# get other cases vs controls
oth_df <- df %>% filter(!(diag_6_mon == 1 & other == 0))
# check
table(oth_df$diag_6_mon, oth_df$other)
# select the vars needed
oth_df <- oth_df[,c(11, 5:7, 12:15, 16:19)]
oth_df$other <- as.factor(oth_df$other)
###### unregularised
oth_df <- oth_df %>% mutate(Smoke_status.0.0 = relevel(Smoke_status.0.0, ref = "Never"),
                            Ethnicity.0.0 = relevel(Ethnicity.0.0, ref = "White"),
                            Alcohol_freq.0.0 = relevel(Alcohol_freq.0.0, ref = "never"),
                            highest_Education0 = relevel(highest_Education0, ref = "Other_professional"))
summary(oth_df)
colnames(oth_df) <- recodeVarName(colnames(oth_df))
##### model
md3 <- glm(other ~ .-km4-pam4-kmCons4, data = oth_df, family = binomial)
summary(md3)

##### results
result_full <- cbind(exp(coef(md3)), exp(confint(md3)), summary(md3)$coefficients[,4])
out_full <- cbind(round(result_full[,1:3], 2), signif(result_full[,4], 3))
out_full <- data.frame(out_full)
colnames(out_full) <- c("Estimate", "2.5 %", "97.5 %", "p-value")
out_oth <- out_full[24:26,]
out_oth$varname <- rownames(out_oth)
out_oth$outcome <- rep("Other", 3)
out_oth_pc <- out_oth

out_oth <- rbind(out_oth_km, out_oth_pam, out_oth_kc, out_oth_pc)
#saveRDS(out_oth, "../output/out_clus_oth.rds")

##### prediction
yhat <- predict(md3, newdata = test_X, type = "response")

plot.roc(df_test$other, as.numeric(yhat), 
         print.auc = TRUE)

# confusion matrix
1382/(1382+5485) # 0.20
pred <- as.factor(ifelse(as.numeric(yhat) > 0.2, 1, 0))
# other metrics
confusionMatrix(pred, as.factor(df_test$other), mode = "everything", positive="1")
# ROC object
roc_obj_oth_pc <- roc(df_test$other, as.numeric(yhat))

####### ROC curves
combined_roc_data <- data.frame(
  FPR = c(1 - roc_obj_oth_km$specificities, 1 - roc_obj_oth_pam$specificities, 1 - roc_obj_oth_kc$specificities, 1 - roc_obj_oth_pc$specificities),
  TPR = c(roc_obj_oth_km$sensitivities, roc_obj_oth_pam$sensitivities, roc_obj_oth_kc$sensitivities, roc_obj_oth_pc$sensitivities),
  AUC = c(rep(as.numeric(roc_obj_oth_km$auc), length(roc_obj_oth_km$sensitivities)),
          rep(as.numeric(roc_obj_oth_pam$auc), length(roc_obj_oth_pam$sensitivities)),
          rep(as.numeric(roc_obj_oth_kc$auc), length(roc_obj_oth_kc$sensitivities)),
          rep(as.numeric(roc_obj_oth_pc$auc), length(roc_obj_oth_pc$sensitivities))),
  Method = c(rep("KM", length(roc_obj_oth_km$sensitivities)),
             rep("PAM", length(roc_obj_oth_pam$sensitivities)),
             rep("C-KM", length(roc_obj_oth_kc$sensitivities)),
             rep("C-PAM", length(roc_obj_oth_pc$sensitivities))),
  Outcome = rep("Other", 61113)
)
# saveRDS(combined_roc_data, "../output/roc_clus_oth.rds")
# remove unneeded
rm(oth_df)

######### ROC plot
df1 <- readRDS("../output/roc_clus_all.rds")
df2 <- readRDS("../output/roc_clus_lym.rds")
df3 <- readRDS("../output/roc_clus_mye.rds")
df4 <- readRDS("../output/roc_clus_oth.rds")
df <- rbind(df1,df2,df3,df4)
df1 <- subset(df, Method == "C-PAM")

AUCs <- df1$AUC[c(1,15280,30559,45838)]
ggplot(df1, aes(x = FPR, y = TPR, color = Outcome, group = Outcome)) +
  geom_line() +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype = "dashed", color = "gray") +
  labs(title = "C-PAM", x = "False Positive Rate", y = "True Positive Rate") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank()) +
  #scale_color_manual(values = c("blue", "red", "green", "purple")) +
  geom_text(data = data.frame(x = c(0.6, 0.6, 0.6, 0.6), 
                              y = c(0.45, 0.40, 0.35, 0.3),
                              labels = paste("AUC: ", round(AUCs, 3)),
                              Outcome = c("All", "Lymphoid", "Myeloid", "Other")),
            aes(x = x, y = y, label = labels, color = Outcome),
            size = 4,
            hjust = 0,
            show.legend = FALSE)

#### forest plot
df1 <- readRDS("../output/out_clus_all.rds")
df2 <- readRDS("../output/out_clus_lym.rds")
df3 <- readRDS("../output/out_clus_mye.rds")
df4 <- readRDS("../output/out_clus_oth.rds")

df <-rbind(df1, df2, df3, df4)
colnames(df) <- c("estimate", "CI_l", "CI_u", "p", "varname", "model")
#saveRDS(df, "../output/Clus_BC.rds")
ggplot(df, aes(y = fct_rev(varname))) + 
  theme_classic() +
  geom_vline(xintercept = 1, linetype="dashed") +
  geom_point(aes(x=estimate, color = model), shape=15, size=3) +
  geom_linerange(aes(xmin=CI_l, xmax=CI_u, color = model)) +
  labs(x="Odds Ratio", y="") +
  #scale_color_manual(values = c("blue", "red", "green", "purple")) +
  theme(legend.position = "right", legend.title = element_blank()) +
  ggtitle("")

# recodeVarName2 <- function(x) {
#   recode(df1$varname,
#          "km42" = "KM_2",
#          "km44" = "KM_4",
#          "pam42" = "PAM_2",
#          "pam43" = "PAM_3",
#          "pam44" = "PAM_4",
#          "kmCons42" = "C-KM_2",
#          "kmCons43" = "C-KM_3",
#          "kmCons44" = "C-KM_4",
#          "pamCons42" = "C-PAM_2",
#          "pamCons43" = "C-PAM_3",
#          "pamCons44" = "C-PAM_4")
# }
# df1$varname <- recodeVarName2(df1$varname)
# df2$varname <- recodeVarName2(df2$varname)
# df3$varname <- recodeVarName2(df3$varname)
# df4$varname <- recodeVarName2(df4$varname)

# df1 <- df1[-2,]
# df2 <- df2[-2,]
# df3 <- df3[-3,]
# df3$outcome <- rep("Myeloid", 11)
# df4 <- df4[-2,]

#saveRDS(df1, "../output/out_clus_all.rds")

########## Imputed train clus
# df <- readRDS("../data/Imputed_train_clus.rds")
# df1 <- readRDS("../data/Imputed_train_V_v2.rds")
# colnames(df) <- recodeVarName(colnames(df))
# df$Sex <- df1$Sex
# df$`Smoking status` <- df1$Smoke_status.0.0
# df$`Alcohol intake` <- df1$Alcohol_freq.0.0
# df$`Highest Education` <- df1$highest_Education0

# saveRDS(df, "../data/Imputed_train_clus.rds")

######## number of clusters
library(NbClust)
library(cluster)
library(factoextra)
df <- readRDS("../output/ClusBC.rds")
df <- df[,1:21]
#fviz_nbclust(df, FUNcluster = cluster::pam, method = "silhouette") + theme_classic()
