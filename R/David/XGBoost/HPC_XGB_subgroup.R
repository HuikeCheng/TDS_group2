rm(list=ls())



library(ROSE)
library(tidyverse)
library(pROC)
library(xgboost)
library(caret)
library(DiagrammeR)

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
#colnames(df_test)
# test x
test_X <- df_test[, c(5:29, 33:36)]

#colnames(bd)

#glimpse(df_test)
#colnames(df_test)

#glimpse(test_X)
#colnames(test_X)

# remove not needed
rm(df_cat)
rm(df_cat_test)

####### prepare model_data
# get lymhpoid cases vs controls
lym_df <- df %>% filter(!(diag_6_mon == 1 & lymphoid == 0))
#colnamea(lym_df)
# check
table(lym_df$diag_6_mon,lym_df$lymphoid)
# select the vars needed
lym_df <- lym_df[,c(29, 5:7, 33:36, 8:28)]
#summary(lym_df)
# rematch
bd <- ovun.sample(lymphoid ~., data = lym_df, method = "both",
                  p = 0.5,
                  seed = 3,
                  N = 8642)$data
# check ethnicity
table(bd$Ethnicity.0.0)

###### unregularised
bd <- bd %>% mutate(Smoke_status.0.0 = relevel(Smoke_status.0.0, ref = "Never"),
                    Ethnicity.0.0 = relevel(Ethnicity.0.0, ref = "White"),
                    Alcohol_freq.0.0 = relevel(Alcohol_freq.0.0, ref = "never"),
                    highest_Education0 = relevel(highest_Education0, ref = "Other_professional"))
summary(bd)

df <- bd # training set. 

#reorder columns to match
reorder_idx <- match(colnames(df), colnames(test_X))
test_X[reorder_idx]
test_X <- test_X[reorder_idx]
df$lymphoid <- as.factor(df$lymphoid)
test_X$lymphoid <- as.factor(test_X$lymphoid)
#sum(df$lymphoid ==1)
#sum(test_X$lymphoid ==1)


#For Practise runs
#need to randomise to get cases into small sample.
set.seed(2347723)  # Set seed
df <- df[sample(1:nrow(df)), ]     # Randomly reorder rows
                                     # Print updated data
#table(df$lymphoid ==1)

#Toy set data 
df <- df[1:200, ] #reduce size to work on Ondemand
test_df <- test_X[1:200, ] #reduce size to work on Ondemand

rm(bd)
rm(lym_df)
rm(test_X) #change for HPC
rm(df_test)

#Change size of data before running on HPC. 
#colnames(test_df)
#colnames(df)

#df <- df[, c(3,5:9, 10:32)] #full size to work on HPC
#test_df <- test_df[, c(3,5:9, 10:32)] #full size to work on HPC

#colnames(df)
#summary(df)
#str(df)

#data split
set.seed(3)
#parts = createDataPartition(df$diag_6_mon, p = .8, list = F)
train = df
#test = test_X #HPC data
test = test_df #toy data

#define predictor and response variables in training set
train_x = data.matrix(train[, -1])
train_y = train[,1]

#define predictor and response variables in testing set
test_x = data.matrix(test[, -1])
test_y = test[,1]

#define final training and testing sets
xgb_train = xgb.DMatrix(data = train_x, label = as.integer(train_y))
xgb_test = xgb.DMatrix(data = test_x, label = as.integer(test_y))
xgb_train = xgb.DMatrix(data = train_x, label = (train_y))
xgb_test = xgb.DMatrix(data = test_x, label = (test_y))

#define watchlist
#watchlist = list(train=xgb_train, test=xgb_test)

#fit XGBoost model and display training and testing data at each round
#model = xgb.train(data = xgb_train, max.depth = 3, watchlist=watchlist, nrounds = 70, objective = "binary:logistic")

#From the output we can see that the minimum testing RMSE is achieved at 26 rounds.

#tuning
grid_tune <- expand.grid(
  nrounds = c(500,1000,1500), #number of trees
  max_depth = c(2,4,6),
  eta = 0.3, #c(0.025,0.05,0.1,0.3), #Learning rate
  gamma = 0, # pruning --> Should be tuned. i.e c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0)
  colsample_bytree = 1, # c(0.4, 0.6, 0.8, 1.0) subsample ratio of columns for tree
  min_child_weight = 1, # c(1,2,3) # the larger, the more conservative the model is; can be used as a stop
  subsample = 1 # c(0.5, 0.75, 1.0) # used to prevent overfitting by sampling X% training
)

train_control <- trainControl(method = "cv",
                              number=3,
                              verboseIter = TRUE,
                              allowParallel = TRUE)
xgb_tune <- train(x = train_x,
                  y = train_y,
                  trControl = train_control,
                  tuneGrid = grid_tune,
                  method= "xgbTree",
                  verbose = TRUE)
xgb_tune

# Best tune
xgb_tune$bestTune

# Writing out the best model.

train_control <- trainControl(method = "none",
                              verboseIter = TRUE,
                              allowParallel = TRUE)

#final_grid <- expand.grid(nrounds = xgb_tune$bestTune$nrounds,
                          eta = xgb_tune$bestTune$eta,
                          max_depth = xgb_tune$bestTune$max_depth,
                          gamma = xgb_tune$bestTune$gamma,
                          colsample_bytree = xgb_tune$bestTune$colsample_bytree,
                          min_child_weight = xgb_tune$bestTune$min_child_weight,
                          subsample = xgb_tune$bestTune$subsample)

final_grid <- expand.grid(nrounds = 8,
                          eta = 0.3,
                          max_depth = 1000,
                          gamma = 0,
                          colsample_bytree = 1,
                          min_child_weight = 1,
                          subsample = 1)

xgb_model <- train(x = train_x,
                   y = train_y,
                   trControl = train_control,
                   tuneGrid = final_grid,
                   method = "xgbTree",
                   verbose = TRUE)

predict(xgb_model, test_x)
#xgb.importance(model = bst)

# Prediction:
xgb.pred <- predict(xgb_model, test_x)
#xgb.pred <- ifelse (xgb.pred > 0.5,1,0)
#xgb.pred <- as.factor(xgb.pred)
#xgb.pred <- as.numeric(xgb.pred)
#str(xgb.pred)
#str(test_y)

#test_y <- as.factor(test_y)
#test_y <- as.numeric(test_y)

#table(xgb.pred)
#table(test_y)
#str(xgb.pred)
#str(test_y)





# Confusion Matrix
#cf <- confusionMatrix(xgb.pred, test_y, mode = "everything")
cf <- confusionMatrix(factor(xgb.pred), factor(test_y),  mode = "everything")
print(cf)
#confusionMatrix(as.factor(as.numeric(xgb.pred)), as.factor(as.numeric(test_y)))



# Variable Importance
varimp<- varImp(xgb_model, scale = FALSE) #The function automatically scales the importance scores to be between 0 and 100. Set to False to avoid)
varimp_plot <- plot(varimp, main="Variable Importance with XGBoost")
varimp_plot


#print(importance_matrix)
#importance_matrix <- xgboost::xgb.importance(model=xgb_model)
#importance <- xgb.importance(feature_names = colnames(test_x), model = xgb_model)

#ROC curve
xgb.pred <-as.numeric(xgb.pred)
roc_obj <- roc(response = test_y, predictor = xgb.pred)
plot(roc_obj, main = "ROC Curve", col = "blue", print.auc = TRUE, legacy.axes = TRUE, max.auc.polygon = TRUE)

roc.plot <- plot(roc_obj, main = "ROC Curve", col = "blue", print.auc = TRUE, legacy.axes = TRUE, max.auc.polygon = TRUE)
roc.plot


#draw confusion matrix for ppt

draw_confusion_matrix <- function(cm) {
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, 'Negative', cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, 'Positive', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, 'Negative', cex=1.2, srt=90)
  text(140, 335, 'Positive', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
}  

cf_plot <- draw_confusion_matrix(cf)
cf_plot
jpeg(file = "../output/XGboost_CF_plot.jpeg")

pdf("../output/XGboost_CF_plot.pdf")
print(cf_plot)
dev.off()

pdf("../output/XGBvarimp_plot.pdf")
print(varimp_plot)
dev.off()

pdf("../output/XGBroc.plot.pdf")
print(roc.plot)
#add more plots here with print(plot_object)
dev.off()


# Visualize first tree in model
#xgb.dump(xgb_model, with_stats = TRUE)
#xgb.plot.tree(model = xgb_model, n = 0)
