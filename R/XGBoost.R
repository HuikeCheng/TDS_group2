#########
library(xgboost)
library(pROC)

########## load data
df <- readRDS("../data/Matched_data.rds")
test_df <- readRDS("../data/Imputed_test_V.rds")

######### XGBoost
X <- df[, c(5:8, 10:33)]
newX <- model.matrix(~., data = X)

######## CV
cv <- xgb.cv(data = newX, label = df$diag_6_mon, nrounds = 3, nthread = 2, nfold = 5, metrics = list("rmse","auc"),
             max_depth = 3, eta = 1, objective = "binary:logistic")
cv$evaluation_log$train_auc_mean[3]

####### tuning
out <- c(NA, NA)
niter <- c(10,50)

for (i in 1:2) {
  cv <- xgb.cv(data = newX, label = df$diag_6_mon, nrounds = niter[i], nthread = 2, nfold = 5, metrics = list("rmse","auc"),
               max_depth = 3, eta = 1, objective = "binary:logistic")
  out[i] <- cv$evaluation_log$train_auc_mean[niter[i]]
}



######## fit
bst <- xgboost(data = newX, 
               label = df$diag_6_mon, 
               max.depth = 2, 
               eta = 0.2, 
               nthread = 2, 
               nrounds = 50, 
               objective = "binary:logistic",
               verbose = 0)

bst$evaluation_log$train_logloss[100]

###### prediction
test_X <- test_df[, c(5:8, 10:33)]
test_X <- model.matrix(~., data = test_X)
pred <- predict(bst, test_X)
prediction <- as.numeric(pred > 0.5)

####### contingency table
table(prediction, test_df$diag_6_mon)
pheatmap::pheatmap(table(prediction, test_df$diag_6_mon))
### AUC
auc(roc(test_df$diag_6_mon, prediction))
plot.roc(test_df$diag_6_mon, prediction,
         percent=TRUE,
         ci = TRUE,
         print.auc = TRUE)


#######
importance_matrix <- xgb.importance(model = bst)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)

write.csv(importance_matrix, "../output/xgboost_varimp_v1.csv")
