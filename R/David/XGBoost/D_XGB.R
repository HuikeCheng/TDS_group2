#Issues:
#  final tree
#xgb.importance


#########
library(xgboost)
library(pROC)
library(caret)
library(DiagrammeR)

########## load data
df <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group2/data/Matched_data_v3.rds")
test_df <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group2/data/Imputed_test_V_v3.rds")
df <- df[1:100, c(3,5:9, 10:32)] #reduce size to work on Ondemand
test_df <- test_df[1:100, c(3,5:9, 10:32)] #reduce size to work on Ondemand


#Change size of data before running on HPC. 

#df <- df[, c(3,5:9, 10:32)] #full size to work on HPC
#test_df <- test_df[, c(3,5:9, 10:32)] #full size to work on HPC
df$diag_6_mon <- as.factor(df$diag_6_mon)
test_df$diag_6_mon <- as.factor(test_df$diag_6_mon)
#colnames(df)
#summary(df)
#str(df)

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

#define final training and testing sets
xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)


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
  min_child_weight = 1, # c(1,2,3) # the larger, the more conservative the model
  #is; can be used as a stop
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

predict(xgb_model, test_x)

#xgb.importance(model = bst)

# Prediction:
xgb.pred <- predict(xgb_model, test_x)

#xgb.pred <- ifelse (xgb.pred > 0.5,1,0)
#str(xgb.pred)
#str(test_y)

# Confusion Matrix
cf <- confusionMatrix(xgb.pred, test_y, mode = "everything")

#confusionMatrix(as.factor(as.numeric(xgb.pred)), as.factor(as.numeric(test_y)))

print(cf)

# Variable Importance
varimp<- varImp(xgb_model, scale = FALSE) #The function automatically scales the importance scores to be between 0 and 100. Set to False to avoid)
varimp_plot <- plot(varimp, main="Variable Importance with XGBoost")

importance_matrix <- xgboost::xgb.importance(model=xgb_model)
#print(importance_matrix)
importance <- xgb.importance(feature_names = colnames(test_x), model = xgb_model)

#ROC curve
xgb.pred <-as.numeric(xgb.pred)
roc_obj <- roc(response = test_y, predictor = xgb.pred)
plot(roc_obj, main = "ROC Curve", col = "blue", print.auc = TRUE, legacy.axes = TRUE, max.auc.polygon = TRUE)

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

CF_plot <- draw_confusion_matrix(cf)
pdf("../output/XGboost_CF_plot.pdf")
print(CF_plot)
print(varimp_plot)
#add more plots here with print(plot_object)
dev.off()

#cf_sense<- cf[["byClass"]][["Sensitivity"]] #save RDS



###OLD CODE

#define final model
#final = xgb.train(data = xgb_train, max.depth = 3, watchlist=watchlist, nrounds = 26, objective = "binary:logistic")

#use model to make predictions on test data
#pred_y = predict(final, xgb_test)

#measure prediction accuracy
#mean((test_y - pred_y)^2) #mse MSE: Mean Squared Error
#caret::MAE(test_y, pred_y) #mae MAE: Mean Absolute Error
#caret::RMSE(test_y, pred_y) #rmse RMSE: Root Mean Squared Error

# Variable Importance
#importance <- xgb.importance(feature_names = colnames(xgb_test), model = final)
#xgb.plot.importance(importance_matrix = importance)

#ROC curve
#roc_obj <- roc(response = test_y, predictor = pred_y)
#plot(roc_obj, main = "ROC Curve", col = "blue", print.auc = TRUE, legacy.axes = TRUE, max.auc.polygon = TRUE)

#calibration plot
#calibration_curve <- calibrate(test_y, pred_y, method = "prob")
#plot(calibration_curve)

# Visualize first tree in model
#xgb.dump(xgb_model, with_stats = TRUE)
#xgb.plot.tree(model = xgb_model, n = 0)


#prediction <- as.numeric(pred_y > 0.5)
#print(head(prediction))

####### contingency table

#table(pred_y, test_y)
#pheatmap::pheatmap(table(pred_y, test_y))












#OLD code
#data split
#set.seed(1)
#parts = createDataPartition(df$diag_6_mon, p = .8, list = F)
#train = df[parts, ]
#test = df[-parts, ]
