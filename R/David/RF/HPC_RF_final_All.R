rm(list=ls())

library(ROSE)
library(tidyverse)
library(caret)
library(randomForest)
library(pROC)
library(ROCR)
library(ggplot2)


########## load data
######### Load data
setwd("/rds/general/project/hda-22-23/live/TDS/Group2/R")
df <- readRDS("../data/Matched_data_v3.rds")
df_test <- readRDS("../data/Imputed_test_V_v3.rds")
df_cat <- readRDS("../data/Matched_data_v2.rds")
df_cat_test <- readRDS("../data/Imputed_test_V_v2.rds")
# change categorical columns back
df_cat <- df_cat[,c(1,5,6,11,12)]
df <- df[,-c(5,6,10,11)]
df <- merge(df, df_cat, by = "eid", all.x = TRUE)

# same for test set
df_cat_test <- df_cat_test[,c(1,5,6,11,12)]
df_test <- df_test[,-c(5,6,10,11)]
df_test <- merge(df_test, df_cat_test, by = "eid", all.x = TRUE)
#glimpse(df_test)

colnames(df_test)
colnames(df)

#select variables
df <- df[, c(3, 5:28, 33:36)]
test_df <- df_test[, c(3, 5:28, 33:36)]

colnames(test_df)
colnames(df)
# remove not needed
rm(df_cat)
rm(df_cat_test)

#Change size of data before running on HPC. 
df$diag_6_mon <- as.factor(df$diag_6_mon)
test_df$diag_6_mon <- as.factor(test_df$diag_6_mon)


#summary(bd)
#summary(bd$Ethnicity.0.0)
#str(bd$Ethnicity.0.0)
#levels(bd$Ethnicity.0.0)
eth <- as.character(df$Ethnicity.0.0)
eth[eth == "African"] <- "Black"   # Replace   african to black
eth[eth == "Caribbean"] <- "Black" # Replace african to black  
#table(eth)
eth[eth == "Prefer not to answer"] <-"Do not know" # Replace prefer not to answer with do not know
#table(eth) 

df$Ethnicity.0.0 <- as.factor(eth)

df <- df %>% mutate(Smoke_status.0.0 = relevel(Smoke_status.0.0, ref = "Never"),
                    Ethnicity.0.0 = relevel(Ethnicity.0.0, ref = "White"),
                    Alcohol_freq.0.0 = relevel(Alcohol_freq.0.0, ref = "never"),
                    highest_Education0 = relevel(highest_Education0, ref = "Other_professional"))

eth <- as.character(test_df$Ethnicity.0.0)
eth[eth == "African"] <- "Black"   # Replace   african to black
eth[eth == "Caribbean"] <- "Black" # Replace african to black  
#table(eth)
eth[eth == "Prefer not to answer"] <-"Do not know" # Replace prefer not to answer with do not know
#table(eth)
test_df$Ethnicity.0.0 <- as.factor(eth)

test_df <- test_df %>% mutate(Smoke_status.0.0 = relevel(Smoke_status.0.0, ref = "Never"),
                    Ethnicity.0.0 = relevel(Ethnicity.0.0, ref = "White"),
                    Alcohol_freq.0.0 = relevel(Alcohol_freq.0.0, ref = "never"),
                    highest_Education0 = relevel(highest_Education0, ref = "Other_professional"))




#######################toy model - turn on BELOW for toy set #################

#need to randomise to get cases into small sample.
#set.seed(2347723)  # Set seed                                               #turn on  toy set
#df <- df[sample(1:nrow(df)), ]     # Randomly reorder rows                 #turn on  toy set    
#df <- df[1:200, ]                                                         #turn on  toy set
#test_df <- test_df[1:200, ]                                                #turn on toy set


#######################toy model - turn on ABOVE for toy set #################




#data split
set.seed(1)
#parts = createDataPartition(df$diag_6_mon, p = .8, list = F)
train = df
test = test_df

#define predictor and response variables in training set
train_x = data.matrix(train[, -1])
train_y = train[,1]

#define predictor and response variables in testing set
test_x = data.matrix(test[, -1])
test_y = test[,1]




#######################HPC model - SKIP BELOW for HPC set #################


#finalmodel

custom <- randomForest(diag_6_mon ~ ., data = train, 
                       importance = TRUE,
                       proximity = TRUE, 
                       ntree=500, 
                       mtry = 7)


#assess final model 

train_pred <-predict(custom, train,  type='prob')
train_pred <- as.data.frame(train_pred)
saveRDS(train_pred, "David/model_output/RF.all.train_pred.rds")

train_pred_probs <- predict(custom, train,  type='prob')
train_pred_probs <- as.data.frame(train_pred_probs)
saveRDS(train_pred_probs, "David/model_output/RF.all.train_pred_probs.rds")

train_y <- as.data.frame(train_y)
saveRDS(train_y, "David/model_output/RF.all.train_y.rds")

train$diag_6_mon <- as.data.frame(train$diag_6_mon)
saveRDS(train$diag_6_mon, "David/model_output/RF.all.train.diag_6_mon.rds")

#predict with new data

pred <-predict(custom, test,  type='prob')
pred <- as.numeric(pred[,2] > 0.5)
pred <- factor(pred)

cm <- confusionMatrix(pred, test$diag_6_mon, mode = "everything")
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


#variable importance
varImpPlot(custom)

varimp<- varImp(custom, scale = FALSE) #The function automatically scales the importance scores to be between 0 and 100. Set to False to avoid)
varimp_plot <- plot(varimp, main="Variable Importance with Random Forest for Group All Cancer")
varimp_plot

pdf("../output/RF_All_varimp_plot.pdf")
print(varimp_plot)
dev.off()




#ROC curve
pred_probs <- predict(custom, test,  type='prob')
pred_probs
roc_obj <- roc(test_y, pred_probs[, 2])

roc_obj <- roc(response = test_y, predictor = as.numeric(pred_probs[, 2]))
auc <- round(auc(response = test_y, predictor = pred_probs[, 2]),4)

b <- ggroc(roc_obj, colour = 'steelblue', size = 2) +
  ggtitle(paste0('ROC Curve with Random Forest for Group All Cancer', '(AUC = ', auc, ')'))
b
ggsave(filename="../output/RF_All_roc.plot.pdf", plot=b)


#Save output for visualisation

RF.all.varimp <- varImp
saveRDS(RF.all.varimp, "David/model_output/RF.all.varimp.rds")

saveRDS(custom, "David/model_output/RF.all.model.rds")

test$diag_6_mon <- as.data.frame(test$diag_6_mon)
saveRDS(test$diag_6_mon, "David/model_output/RF.all.test$diag_6_mon.rds")

test_y <- as.data.frame(test_y)
saveRDS(test_y, "David/model_output/RF.all.test_y.rds")

xgb.pred <- as.data.frame(pred)
saveRDS(xgb.pred, "David/model_output/RF.all.pred.rds")

pred_probs <- as.data.frame(pred_probs)
saveRDS(pred_probs, "David/model_output/RF.all.pred_probs.rds")






#OLD CODE##
#data split
#set.seed(1)
#parts = createDataPartition(df$diag_6_mon, p = .8, list = F)
#train = df[parts, ]
#test = df[-parts, ]

#Caret Tuning
#NZV<- nearZeroVar(train, saveMetrics = TRUE)
#NZV[NZV[,"zeroVar"] > 0, ] 
#NZV[NZV[,"zeroVar"] + NZV[,"nzv"] > 0, ]