library(ROSE)
library(tidyverse)
library(caret)
library(randomForest)
library(pROC)
library(ROCR)
library(ggplot2)

rm(list=ls())
  
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
   #table(bd$Ethnicity.0.0)
   
   ###### unregularised
   
   #summary(bd)
summary(bd$Ethnicity.0.0)
str(bd$Ethnicity.0.0)
levels(bd$Ethnicity.0.0)
eth <- as.character(bd$Ethnicity.0.0)
eth[eth == "African"] <- "Black"   # Replace   african to black
eth[eth == "Caribbean"] <- "Black" # Replace african to black  
table(eth)
   
eth[eth == "Prefer not to answer"] <-"Do not know" # Replace prefer not to answer with do not know
table(eth) 
  
bd$Ethnicity.0.0 <- as.factor(eth)
  
bd <- bd %>% mutate(Smoke_status.0.0 = relevel(Smoke_status.0.0, ref = "Never"),
                                       Ethnicity.0.0 = relevel(Ethnicity.0.0, ref = "White"),
                                       Alcohol_freq.0.0 = relevel(Alcohol_freq.0.0, ref = "never"),
                                       highest_Education0 = relevel(highest_Education0, ref = "Other_professional"))
  
  table(eth)
  str(bd)
  
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
df$lymphoid <- as.factor(df$lymphoid)
test_X$lymphoid <- as.factor(test_X$lymphoid)
 #sum(df$lymphoid ==1)
   #sum(test_X$lymphoid ==1)
   

eth <- as.character(test_X$Ethnicity.0.0)
 
eth[eth == "African"] <- "Black"   # Replace   african to black
eth[eth == "Caribbean"] <- "Black" # Replace african to black  
table(eth)
eth[eth == "Prefer not to answer"] <-"Do not know" # Replace prefer not to answer with do not know
table(eth)

test_X$Ethnicity.0.0 <- as.factor(eth)
 
test_X <- test_X %>% mutate(Smoke_status.0.0 = relevel(Smoke_status.0.0, ref = "Never"),
                                                      Ethnicity.0.0 = relevel(Ethnicity.0.0, ref = "White"),
                                                      Alcohol_freq.0.0 = relevel(Alcohol_freq.0.0, ref = "never"),
                                                      highest_Education0 = relevel(highest_Education0, ref = "Other_professional"))
#For Practise runs NEEDS SWAP for toy set
#need to randomise to get cases into small sample.
set.seed(2347723)  # Set seed
df <- df[sample(1:nrow(df)), ]     # Randomly reorder rows
# Print updated data
#table(df$lymphoid ==1)

#Toy set data 
df <- df[1:200, ]                                                  #turn on  toy set
test_df <- test_X[1:200, ]                                          #turn on toy set

rm(bd)
rm(lym_df)
rm(test_X)                                                           #turn ON for HPC
rm(df_test)

#colnames(df)
#colnames(test_X)

#data split
set.seed(3)
#parts = createDataPartition(df$diag_6_mon, p = .8, list = F)
train = df
#test = test_X                                                        #turn on for HPC run
test = test_df                                                       #turn on toy data

#define predictor and response variables in training set
train_x = data.matrix(train[, -1])
train_y = train[,1]

#define predictor and response variables in testing set
test_x = data.matrix(test[, -1])
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




# prepare training scheme
seed <- 8
metric <- "Accuracy"
mtry <- sqrt(ncol(train))


# train model
control <- trainControl(method="repeatedcv", 
                        number=10, 
                        repeats=3,
                        verboseIter = TRUE,
                        allowParallel = TRUE,
                        savePredictions = "final")

tunegrid <- expand.grid(.mtry = c(3:7),
                        .ntree=c(1000, 1500, 2000, 2500))

tunegrid <- expand.grid(.mtry = c(3:7))

customRF <- list(type = "Classification",
                 library = "randomForest",
                 loop = NULL)

set.seed(seed)

custom <- train(train$lymphoid ~., 
                data=train, 
                method="rf", 
                metric=metric, 
                tuneGrid=tunegrid, 
                trControl=control,
                ntree=c(1000))

summary(custom)

plot(custom)


#final model 
final <- randomForest(lymphoid ~ ., data = train, 
                      importance = TRUE,
                      proximity = TRUE, 
                      ntree=500, 
                      mtry = 7)

predicted <- predict(final, newdata=test, type = "response")


#variable importance

importance(final)
varImpPlot(final)

#perf = prediction(as.numeric(pred1), as.numeric(test_y))


####### contingency table
table(predicted, test_y)
pheatmap::pheatmap(table(predicted, test_y))

cm <- confusionMatrix(data = predicted, reference = test_y, mode = "everything")

print(cm)

confusion_matrix <- as.data.frame(table(pred1, test_y))

plt <- as.data.frame(cm$table)
plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))

a <- ggplot(plt, aes(Reference,Prediction, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("Negative","Positive")) +
  scale_y_discrete(labels=c("Positive","Negative"))
a
ggsave(filename="../output/RF_cm.plot.pdf", plot=a)

### AUC

rocobj <- roc(test$lymphoid, as.numeric(predicted))

auc <- round(auc(test$lymphoid, as.numeric(predicted)),4)

#create ROC plot
ggroc(rocobj, colour = 'steelblue', size = 2) +
  ggtitle(paste0('ROC Curve ', '(AUC = ', auc, ')'))

ggsave(filename="../output/XGboost_roc.plot.pdf", plot=b)








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



