########## load packages
library(randomForest)

########## load data
df <- readRDS("../data/Matched_data.rds")
test_df <- readRDS("../data/Imputed_test_V.rds")

######### random forest
model_data <- df[, c(3,5:8, 10:33)]
model_data$diag_6_mon <- as.factor(model_data$diag_6_mon)
#newX <- model.matrix(~., data = X)
rf <- randomForest(diag_6_mon ~ ., data=model_data, 
                   importance = TRUE,
                   proximity = TRUE)
print(rf)

#####
plot(rf)

varImpPlot(rf)
####### predict
test_X <- test_df[, c(5:8, 10:33)]

pred_r <- predict(rf, test_X, type="response", proximity = TRUE)

table(tmp, test_df$diag_6_mon)
pheatmap::pheatmap(table(tmp, test_df$diag_6_mon))

plot.roc(test_df$diag_6_mon, as.numeric(tmp),
         percent=TRUE,
         ci = TRUE,
         print.auc = TRUE)
library(pROC)
