rm(list=ls())

library(ROSE)
library(tidyverse)
library(pROC)
library(xgboost)
library(caret)
library(DiagrammeR)
library(gridGraphics)
library(grid)

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
df$myeloid <- as.factor(df$myeloid)
test_X$myeloid <- as.factor(test_X$myeloid)
sum(df$myeloid ==1)
sum(test_X$myeloid ==1)

#For Practise runs NEEDS SWAP for toy set
#need to randomise to get cases into small sample.
#set.seed(2347723)  # Set seed                                               #turn on  toy set
#df <- df[sample(1:nrow(df)), ]     # Randomly reorder rows                 #turn on  toy set    
#df <- df[1:200, ]                                                         #turn on  toy set
#test_df <- test_X[1:200, ]                                                #turn on toy set

# Print updated data
#table(df$lymphoid ==1)

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

#define final training and testing sets
xgb_train = xgb.DMatrix(data = train_x, label = as.integer(train_y))
xgb_test = xgb.DMatrix(data = test_x, label = as.integer(test_y))
xgb_train = xgb.DMatrix(data = train_x, label = (train_y))
xgb_test = xgb.DMatrix(data = test_x, label = (test_y))


print("checkpoint1")
#define watchlist
#watchlist = list(train=xgb_train, test=xgb_test)

#fit XGBoost model and display training and testing data at each round
#model = xgb.train(data = xgb_train, max.depth = 3, watchlist=watchlist, nrounds = 70, objective = "binary:logistic")

#From the output we can see that the minimum testing RMSE is achieved at 26 rounds.


#######################toy model - turn on BELOW for toy set #################


#toy model - turn on for toy set

#tune_grid <- expand.grid(nrounds = 200,
#             max_depth = 4,
#             eta = 0.05,
#             gamma = 0.01,
#             colsample_bytree = 0.75,
#              min_child_weight = 0,
#              subsample = 0.5)
#              train_control <- trainControl(method = "cv", number = 5)

#xgb_model <- train(x = train_x,
#            y = train_y, 
#            method = "xgbTree",
#            trControl=train_control,
#            tuneGrid = tune_grid,
#            tuneLength = 10)


#######################toy model - turn on ABOVE for toy set #################


#######################HPC model - SKIP BELOW for HPC set #################


#tuning
grid_tune <- expand.grid(
  nrounds = c(500,1000,1500), #number of trees
  max_depth = c(2,4,6),
  eta = 0.2, #c(0.025,0.05,0.1,0.3), #Learning rate
  gamma = 0, # pruning --> Should be tuned. i.e c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0)
  colsample_bytree = 1, # c(0.4, 0.6, 0.8, 1.0) subsample ratio of columns for tree
  min_child_weight = 1, # c(1,2,3) # the larger, the more conservative the model is; can be used as a stop
  subsample = 1 # c(0.5, 0.75, 1.0) # used to prevent overfitting by sampling X% training
)

train_control <- trainControl(method = "repeatedcv",
                              number=10,
                              repeats=3,
                              verboseIter = TRUE,
                              allowParallel = TRUE)
xgb_tune <- train(x = train_x,
                  y = train_y,
                  trControl = train_control,
                  tuneGrid = grid_tune,
                  method= "xgbTree",               #change "xgbDART" , "xgbTree"
                  verbose = TRUE)
xgb_tune

# Best tune
xgb_tune$bestTune


# Writing out the best model.

#train_control <- trainControl(method = "none",
 #                             verboseIter = TRUE,
  #                            allowParallel = TRUE)

final_grid <- expand.grid(nrounds = xgb_tune$bestTune$nrounds,
                          eta = xgb_tune$bestTune$eta,
                          max_depth = xgb_tune$bestTune$max_depth,
                          gamma = xgb_tune$bestTune$gamma,
                          colsample_bytree = xgb_tune$bestTune$colsample_bytree,
                          min_child_weight = xgb_tune$bestTune$min_child_weight,
                          subsample = xgb_tune$bestTune$subsample)


xgb_model <- train(x = train_x,
                   y = train_y,
                   trControl = train_control,
                   tuneGrid = final_grid,
                   method = "xgbTree",
                   verbose = TRUE)



#assess training model 
train_pred_probs <- predict(xgb_model, newdata = train_x, type = "prob")[, 2]
train_pred_probs <- as.data.frame(train_pred_probs)
saveRDS(train_pred_probs, "David/model_output/xgb.myeloid.train_prob.rds")

train_pred <- predict(xgb_model, newdata = train_x)
train_pred <- as.data.frame(train_pred)
saveRDS(train_pred, "David/model_output/xgb.myeloid.train_pred.rds")

train_y <- as.data.frame(train_y)
saveRDS(train_y, "David/model_output/xgb.myeloid.train_y.rds")


#predict with new data 

predict(xgb_model, test_x)
#xgb.importance(model = bst)

# Prediction:
xgb.pred <- predict(xgb_model, test_x)
#xgb.pred <- ifelse (xgb.pred > 0.5,1,0)


# Confusion Matrix
#cf <- confusionMatrix(xgb.pred, test_y, mode = "everything")
cm <- confusionMatrix(as.factor(xgb.pred), as.factor(test_y),  mode = "everything")
print(cm)

confusion_matrix <- as.data.frame(table(xgb.pred, test_y))

plt <- as.data.frame(cm$table)
plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))

a <- ggplot(plt, aes(Reference,Prediction, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction", title = "Confusion Matrix for XGBoost for Group Myeloid") +
  scale_x_discrete(labels=c("Negative","Positive")) +
  scale_y_discrete(labels=c("Positive","Negative"))
a
ggsave(filename="../output/XGB_myeloid_cm.plot.pdf", plot=a)

#confusionMatrix(as.factor(as.numeric(xgb.pred)), as.factor(as.numeric(test_y)))

# Variable Importance
varimp<- varImp(xgb_model, scale = TRUE) #The function automatically scales the importance scores to be between 0 and 100. Set to False to avoid)
varimp_plot <- plot(varimp, main="Variable Importance with XGBoost for Myeloid Group")
varimp_plot

pdf("../output/XGB_myeloid_varimp_plot.pdf")
print(varimp_plot)
dev.off()

#print(importance_matrix)
#importance_matrix <- xgboost::xgb.importance(model=xgb_model)
#importance <- xgb.importance(feature_names = colnames(test_x), model = xgb_model)

#ROC curve
#xgb.pred <-as.numeric(xgb.pred)
#roc_obj <- roc(response = test_y, predictor = xgb.pred)
#auc <- round(auc(response = test_y, predictor = xgb.pred),4)

#b <- ggroc(roc_obj, colour = 'steelblue', size = 2) +
#ggtitle(paste0('ROC Curve ', '(AUC = ', auc, ')'))
#ggsave(filename="../output/XGboost_roc.plot.pdf", plot=b)


pred_probs <- predict(xgb_model, test_x,  type='prob')
roc_obj <- roc(test_y, pred_probs[, 2])
#pred_probs <- ifelse (pred_probs[, 2] > 0.5,1,0)
pred_probs

roc_obj <- roc(response = test_y, predictor = as.numeric(pred_probs[, 2]))
auc <- round(auc(response = test_y, predictor = pred_probs[, 2]),4)

b <- ggroc(roc_obj, colour = 'steelblue', size = 2) +
  ggtitle(paste0('ROC Curve for XGBoost for Myeloid Group ', '(AUC = ', auc, ')'))
b
ggsave(filename="../output/XGB_myeloid_roc.plot.pdf", plot=b)

# Visualize first tree in model
#xgb.dump(xgb_model, with_stats = TRUE)
#xgb.plot.tree(model = xgb_model, n = 0)



#Save output for visualisation

xgb.myeloid.varimp <- varImp(xgb_model)$importance
saveRDS(xgb.myeloid.varimp, "David/model_output/xgb.myeloid.varimp.rds")

saveRDS(xgb_model, "David/model_output/xgb.myeloid.xgb_model.rds")

test_y <- as.data.frame(test_y)
saveRDS(test_y, "David/model_output/xgb.myeloid.test_y.rds")

xgb.pred <- as.data.frame(xgb.pred)
saveRDS(xgb.pred, "David/model_output/xgb.myeloid.pred.rds")

pred_probs <- as.data.frame(pred_probs)
saveRDS(pred_probs, "David/model_output/xgb.myeloid.pred_probs.rds")

