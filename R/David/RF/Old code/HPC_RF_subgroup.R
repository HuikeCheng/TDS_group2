library(ROSE)
library(tidyverse)
library(caret)
library(randomForest)
library(pROC)
library(ROCR)


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
df


# same for test set
df_cat_test <- df_cat_test[,c(1,5,6,11,12)]
df_test <- df_test[,-c(5,6,10,11)]
df_test <- merge(df_test, df_cat_test, by = "eid", all.x = TRUE)
glimpse(df_test)
colnames(df_test)
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
colname(lym_df)
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
rm(bd)

#reorder columns to match
reorder_idx <- match(colnames(df), colnames(test_X))
test_X[reorder_idx]
test_X <- test_X[reorder_idx]

colnames(test_X)
colnames(df)
#dim(test_X)
#dim(df)
#str(test_X)
#str(df)
#head(df)
#head(test_X)

set.seed(2347723)  # Set seed
df <- df[sample(1:nrow(df)), ]     # Randomly reorder rows
df                                     # Print updated data

#table(df$lymphoid ==1)
#table(test_df$lymphoid ==1)


df <- df[1:1000, ] #reduce size to work on Ondemand
test_df <- test_X[1:1000, ] #reduce size to work on Ondemand
#colnames(df)
#colnames(test_df)

#df <- df[, c(3,5:9, 10:32)] #full size to work on HPC
#test_df <- test_df[, c(3,5:9, 10:32)] #full size to work on HPC
df$lymphoid <- as.factor(df$lymphoid)
test_df$lymphoid <- as.factor(test_df$lymphoid)
#colnames(df)
#summary(df)
#str(df)

#data split
set.seed(1)
#parts = createDataPartition(df$diag_6_mon, p = .8, list = F)
train = df
test = test_df

#define predictor and response variables in training set
train_x = (train[, -1])
train_y = train[,1]

#define predictor and response variables in testing set
test_x = (test[, -1])
test_y = test[,1]


#model randomn
rf <- randomForest(lymphoid ~ ., data= train, 
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
rf2 <- randomForest(lymphoid ~ ., data= train, 
                    importance = TRUE,
                    proximity = TRUE, ntree=1000)
print(rf2)
plot(rf2)

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
bestmtry <- tuneRF(train_x, train_y, stepFactor=1.5, improve=1e-5, ntree=250)
print(bestmtry)

#best mtry for ntree 500 = 7
#best mtry for ntree 1000 = 3
#best mtry for ntree 1500 = 7
#best mtry for ntree 1500 = 5


#Caret Tuning
NZV<- nearZeroVar(train, saveMetrics = TRUE)
NZV[NZV[,"zeroVar"] > 0, ] 
NZV[NZV[,"zeroVar"] + NZV[,"nzv"] > 0, ]


# prepare training scheme
seed <- 8
metric <- "Accuracy"
mtry <- sqrt(ncol(train))

control <- trainControl(method="repeatedcv", number=10, repeats=3)

tunegrid <- expand.grid(.mtry=mtry)

# train the model
model <- train(train$lymphoid ~., data=train, 
               method="rf", 
               preProcess="scale", 
               metric=metric, 
               tuneGrid=tunegrid, 
               trControl=control, 
               tuneLength=5)
# summarize the model
print(model)

customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes

# train model
control <- trainControl(method="repeatedcv", 
                        number=10, 
                        repeats=3,
                        verboseIter = TRUE,
                        allowParallel = TRUE)

tunegrid <- expand.grid(.mtry=c(1:15), .ntree=c(1000, 1500, 2000, 2500))

set.seed(seed)

custom <- train(train$lymphoid ~., data=train, method=customRF, metric=metric, tuneGrid=tunegrid, trControl=control)

summary(custom)

plot(custom)








#final model 
final <- randomForest(lymphoid ~ ., data = train, 
                      importance = TRUE,
                      proximity = TRUE, ntree=500, mtry = 7)

pred1 <- predict(final, newdata=test_x, type = "response")

importance(final)
varImpPlot(final)

#perf = prediction(as.numeric(pred1), as.numeric(test_y))


####### contingency table
table(pred1, test_y)
pheatmap::pheatmap(table(pred1, test_y))

cf <- confusionMatrix(data = pred1, reference = test_y, mode = "everything")


### AUC
# Calculate predicted probabilities for test data
pred_probs <- predict(final, newdata = test_x, type = "prob")

# Calculate AUC using the roc() function
roc_obj <- roc(test_y, pred_probs[, 2])

# Print AUC value
print(auc(roc_obj))
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

cf_plot <- draw_confusion_matrix(cf)

pdf("../output/XGboost_CF_plot.pdf")
print(cf_plot)
print(varimp_plot)
#add more plots here with print(plot_object)
dev.off()







#OLD CODE##
#data split
#set.seed(1)
#parts = createDataPartition(df$diag_6_mon, p = .8, list = F)
#train = df[parts, ]
#test = df[-parts, ]
