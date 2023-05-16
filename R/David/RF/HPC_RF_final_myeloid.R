rm(list=ls())

library(ROSE)
library(tidyverse)
library(caret)
library(randomForest)
library(pROC)
library(ROCR)
library(ggplot2)


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
# test x
test_X <- df_test[, c(5:28, 30, 33:36)]

colnames(test_X)

# remove not needed
rm(df_cat)
rm(df_cat_test)


#################################### Myeloid ##################################
####### prepare model data
mye_df <- df %>% filter(!(diag_6_mon == 1 & myeloid == 0))
# check
table(mye_df$diag_6_mon, mye_df$myeloid)
# select the vars needed
sel_vars <- c(11, 12, 14, 15, 18, 20, 23, 25, 28)
mye_df <- mye_df[,c(30, 5:7, 33:36, sel_vars)]
summary(mye_df)
mye_df$myeloid <- as.factor(mye_df$myeloid)
# rematch
bd <- ovun.sample(myeloid ~., data = mye_df, method = "both",
                  p = 0.5,
                  seed = 3,
                  N = 6520)$data

# check ethnicity
#table(bd$Ethnicity.0.0)

###### unregularised

#summary(bd)
#summary(bd$Ethnicity.0.0)
#str(bd$Ethnicity.0.0)
#levels(bd$Ethnicity.0.0)
eth <- as.character(bd$Ethnicity.0.0)
eth[eth == "African"] <- "Black"   # Replace   african to black
eth[eth == "Caribbean"] <- "Black" # Replace african to black  
#table(eth)
eth[eth == "Prefer not to answer"] <-"Do not know" # Replace prefer not to answer with do not know
#table(eth) 

bd$Ethnicity.0.0 <- as.factor(eth)

table(bd$Ethnicity.0.0)
###### unregularised
bd <- bd %>% mutate(Smoke_status.0.0 = relevel(Smoke_status.0.0, ref = "Never"),
                    Ethnicity.0.0 = relevel(Ethnicity.0.0, ref = "White"),
                    Alcohol_freq.0.0 = relevel(Alcohol_freq.0.0, ref = "never"),
                    highest_Education0 = relevel(highest_Education0, ref = "Other_professional"))
summary(bd)

df <- bd # training set. 

#class(bd$Ethnicity.0.0)
#bd <- bd %>% mutate(Ethnicity.0.0 = as.character(Ethnicity.0.0),
#Ethnicity.0.0 = str_replace(Ethnicity.0.0, "African", "Black"),
#Ethnicity.0.0 = str_replace(Ethnicity.0.0, "Caribbean", "Black"),
#Ethnicity.0.0 = str_replace(Ethnicity.0.0, "Prefer not to answer", "Do not know"),
#Ethnicity.0.0 = as.factor(Ethnicity.0.0),
#Ethnicity.0.0 = droplevels(Ethnicity.0.0),
#Ethnicity.0.0 = relevel(Ethnicity.0.0, ref = "White"))

#class(bd$Ethnicity.0.0)

#bd$Ethnicity.0.0 <- as.factor(bd$Ethnicity.0.0)    
#summary(bd$Ethnicity.0.0)              

#reorder columns to match
reorder_idx <- match(colnames(df), colnames(test_X))
test_X[reorder_idx]
test_X <- test_X[reorder_idx]
df$myeloid <- as.factor(df$myeloid)
test_X$myeloid <- as.factor(test_X$myeloid)
sum(df$myeloid ==1)
sum(test_X$myeloid ==1)


eth <- as.character(test_X$Ethnicity.0.0)
eth[eth == "African"] <- "Black"   # Replace   african to black
eth[eth == "Caribbean"] <- "Black" # Replace african to black  
#table(eth)
eth[eth == "Prefer not to answer"] <-"Do not know" # Replace prefer not to answer with do not know
#table(eth)
test_X$Ethnicity.0.0 <- as.factor(eth)

test_X <- test_X %>% mutate(Smoke_status.0.0 = relevel(Smoke_status.0.0, ref = "Never"),
                            Ethnicity.0.0 = relevel(Ethnicity.0.0, ref = "White"),
                            Alcohol_freq.0.0 = relevel(Alcohol_freq.0.0, ref = "never"),
                            highest_Education0 = relevel(highest_Education0, ref = "Other_professional"))

#df$myeloid <- match(df$myeloid, levels(test_X$myeloid))
#df$Ethnicity.0.0 <- match(df$Ethnicity.0.0, levels(test_X$Ethnicity.0.0))
#df$Sex <- match(df$Sex, levels(test_X$Sex))
#df$Smoke_status.0.0 <- match(df$Smoke_status.0.0, levels(test_X$Smoke_status.0.0))
#df$Alcohol_freq.0.0 <- match(df$Alcohol_freq.0.0, levels(test_X$Alcohol_freq.0.0))
#df$highest_Education0 <- match(df$highest_Education0, levels(test_X$highest_Education0))

df$myeloid <- factor(df$myeloid, levels = levels(test_X$myeloid))
df$Ethnicity.0.0 <- factor(df$Ethnicity.0.0, levels = levels(test_X$Ethnicity.0.0))
df$Sex <- factor(df$Sex, levels = levels(test_X$Sex))
df$Smoke_status.0.0 <- factor(df$Smoke_status.0.0, levels = levels(test_X$Smoke_status.0.0))
df$Alcohol_freq.0.0 <- factor(df$Alcohol_freq.0.0, levels = levels(test_X$Alcohol_freq.0.0))
df$highest_Education0 <- factor(df$highest_Education0, levels = levels(test_X$highest_Education0))


#######For Practise runs NEEDS SWAP for toy set#######

#need to randomise to get cases into small sample.
#set.seed(2347723)  # Set seed                                               #turn on  toy set
#df <- df[sample(1:nrow(df)), ]     # Randomly reorder rows                 #turn on  toy set    
#df <- df[1:200, ]                                                         #turn on  toy set
#test_df <- test_X[1:200, ]                                                #turn on toy set


# Print updated data
#table(df$myeloid ==1)

rm(bd)
rm(mye_df)
#rm(test_X)                                                              #turn ON for toy set
rm(df_test)

#data split
set.seed(3)
#parts = createDataPartition(df$diag_6_mon, p = .8, list = F)
train = df
test = test_X                                                        #turn on for HPC run
#test = test_df                                                       #turn on toy data

#define predictor and response variables in training set
train_x = data.matrix(train[, -1])
train_y = train[,1]

#define predictor and response variables in testing set
test_x = data.matrix(test[, -1])
test_y = test[,1]


#finalmodel

custom <- randomForest(myeloid ~ ., data = train, 
                       importance = TRUE,
                       proximity = TRUE, 
                       ntree=500, 
                       mtry = 5)


#assess final model 

train_pred <-predict(custom, train, type='prob')
train_pred <- as.data.frame(train_pred)
saveRDS(train_pred, "David/model_output/RF.myeloid.train_pred.rds")

train_pred_probs <- predict(custom, train,  type='prob')
train_pred_probs <- as.data.frame(train_pred_probs)
saveRDS(train_pred_probs, "David/model_output/RF.myeloid.train_pred_probs.rds")

train_y <- as.data.frame(train_y)
saveRDS(train_y, "David/model_output/RF.myeloid.train_y.rds")

train$myeloid <- as.data.frame(train$myeloid)
saveRDS(train$myeloid, "David/model_output/RF.myeloid.train.myeloid.rds")



#predict with new data


pred <-predict(custom, test,  type='prob')
pred <- as.numeric(pred[,2] > 0.5)

pred <- as.factor(pred)

cm <- confusionMatrix(pred, test$myeloid, mode = "everything")
print(cm)

plt <- as.data.frame(cm$table)
plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))

a <- ggplot(plt, aes(Reference,Prediction, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction", title = "Confusion Matrix for Random Forest for Group Myeloid") +
  scale_x_discrete(labels=c("Negative","Positive")) +
  scale_y_discrete(labels=c("Positive","Negative"))
a
ggsave(filename="../output/RF_myeloid_cm.plot.pdf", plot=a)

#variable importance

varImpPlot(custom)

varimp<- varImp(custom, scale = FALSE) #The function automatically scales the importance scores to be between 0 and 100. Set to False to avoid)
varimp_plot <- plot(varimp, main="Variable Importance with Random Forest for Group Myeloid")
varimp_plot

pdf("../output/RF_myeloid_varimp_plot.pdf")
print(varimp_plot)
dev.off()

#ROC curve
pred_probs <- predict(custom, test,  type='prob')
pred_probs
roc_obj <- roc(test_y, pred_probs[, 2])

roc_obj <- roc(response = test_y, predictor = as.numeric(pred_probs[, 2]))
auc <- round(auc(response = test_y, predictor = pred_probs[, 2]),4)

b <- ggroc(roc_obj, colour = 'steelblue', size = 2) +
  ggtitle(paste0('ROC Curve with Random Forest for Group Myeloid', '(AUC = ', auc, ')'))
b
ggsave(filename="../output/RF_Myeloid_roc.plot.pdf", plot=b)


#Save output for visualisation locally

RF.myeloid.varimp <- varImp
saveRDS(RF.myeloid.varimp, "David/model_output/RF.myeloid.varimp.rds")

saveRDS(custom, "David/model_output/RF.myeloid.model.rds")

test$myeloid <- as.data.frame(test$myeloid)
saveRDS(test$myeloid, "David/model_output/RF.myeloid.test$myeloid.rds")

test_y <- as.data.frame(test_y)
saveRDS(test_y, "David/model_output/RF.myeloid.test_y.rds")

pred <- as.data.frame(pred)
saveRDS(pred, "David/model_output/RF.myeloid.pred.rds")

pred_probs <- as.data.frame(pred_probs)
saveRDS(pred_probs, "David/model_output/RF.myeloid.pred_probs.rds")

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
