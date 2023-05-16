#########
library(randomForest)
library(pROC)
library(caret)
library(ROCR)

########## load data
df <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group2/data/Matched_data_v3.rds")
test_df <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group2/data/Imputed_test_V_v3.rds")
df <- df[, c(3,5:9, 10:32)]
test_df <- test_df[, c(3,5:9, 10:32)]
df$diag_6_mon <- as.factor(df$diag_6_mon)
test_df$diag_6_mon <- as.factor(test_df$diag_6_mon)
colnames(df)
colnames(test_df)
summary(df)

#data split
set.seed(1)
parts = createDataPartition(df$diag_6_mon, p = .8, list = F)
train = df
test = test_df

#define predictor and response variables in training set
train_x = (train[, -1])
train_y = train[,1]

#define predictor and response variables in testing set
test_x = (test[, -1])
test_y = test[,1]


#model randomn
rf <- randomForest(diag_6_mon ~ ., data= train, 
                   importance = TRUE,
                   proximity = TRUE)
print(rf)

#####
plot(rf)

varImpPlot(rf)


#Check oob rate
oob.error.data <- data.frame(
  Trees=rep(1:nrow(rf$err.rate), times=3),
  Type=rep(c("OOB", "Healthy", "Unhealthy"), each=nrow(rf$err.rate)),
  Error=c(rf$err.rate[,"OOB"], 
          rf$err.rate[,"Healthy"], 
          rf$err.rate[,"Unhealthy"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))


#increase to 1000 trees
rf2 <- randomForest(diag_6_mon ~ ., data= train, 
                      importance = TRUE,
                      proximity = TRUE, ntree=1000)
print(rf2)

oob.error.data2 <- data.frame(
  Trees=rep(1:nrow(rf2$err.rate), times=3),
  Type=rep(c("OOB", "Healthy", "Unhealthy"), each=nrow(rf2$err.rate)),
  Error=c(rf2$err.rate[,"OOB"], 
          rf2$err.rate[,"Healthy"], 
          rf2$err.rate[,"Unhealthy"]))

ggplot(data=oob.error.data2, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))




#Tune rf
set.seed(1)
set.seed(seed)
bestmtry <- tuneRF(train_x, train_y, stepFactor=1.5, improve=1e-5, ntree=500)
print(bestmtry)

#best mtry for ntree 500 = 4
#best mtry for ntree 1000 = 5

#final model 
final <- randomForest(diag_6_mon ~ ., data = train, 
                    importance = TRUE,
                    proximity = TRUE, ntree=500, mtry = 4)

pred1 <- predict(final, newdata=test_x, type = "response")

importance(final)
varImpPlot(final)

perf = prediction(pred1, test_y)

####### contingency table
table(pred1, test_y)
pheatmap::pheatmap(table(pred1, test_y))

confusionMatrix(data = pred1, reference = test_y, mode = "everything")


### AUC
# Calculate predicted probabilities for test data
pred_probs <- predict(final, newdata = test_x, type = "prob")

# Calculate AUC using the roc() function
roc_obj <- roc(test_y, pred_probs[, 2])

# Print AUC value
print(auc(roc_obj))
plot(roc_obj, main = "ROC Curve", col = "blue", print.auc = TRUE, legacy.axes = TRUE, max.auc.polygon = TRUE)



#OLD CODE##
#data split
#set.seed(1)
#parts = createDataPartition(df$diag_6_mon, p = .8, list = F)
#train = df[parts, ]
#test = df[-parts, ]