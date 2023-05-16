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
colnames(df)
summary(df)
str(df)

colnames(test_df)
summary(test_df)
str(test_df)


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

#For Practise runs NEEDS SWAP for toy set

#need to randomise to get cases into small sample.
#set.seed(2347723)  # Set seed                                               #turn on  toy set
#df <- df[sample(1:nrow(df)), ]     # Randomly reorder rows                 #turn on  toy set    
#df <- df[1:200, ]                                                         #turn on  toy set
#test_df <- test_X[1:200, ]                                                #turn on toy set

# Print updated data
#table(df$lymphoid ==1)

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



tunegrid <- expand.grid(.mtry = c(1:7))

customRF <- list(type = "Classification",
                 library = "randomForest",
                 loop = NULL)


set.seed(seed)

#custom <- caret::train(diag_6_mon ~., 
 #                      data=train, 
  #                     method="rf", 
   #                    metric=metric, 
    #                   tuneGrid=tunegrid, 
     #                  trControl=control,
      #                 ntree=c(250))

#print(custom)
#A <- plot(custom)
#pdf("../output/T250_All.pdf")
#print(A)
#dev.off()
#custom$finalModel
#custom$results

#custom <- caret::train(diag_6_mon ~., 
 #                      data=train, 
  #                     method="rf", 
   #                   metric=metric, 
    #                 tuneGrid=tunegrid, 
     #               trControl=control,
      #             ntree=c(500))

#print(custom)
#A <- plot(custom)
#pdf("../output/T500_All.pdf")
#print(A)
#dev.off()
#custom$finalModel
#custom$results


#custom <- caret::train(diag_6_mon ~., 
 #                     data=train, 
  #                   method="rf", 
   #                 metric=metric, 
    #               tuneGrid=tunegrid, 
     #             trControl=control,
      #           ntree=c(1000))

#print(custom)
#C <- plot(custom)
#pdf("../output/T1000_All.pdf")
#print(C)
#dev.off()
#custom$finalModel
#custom$results

custom <- caret::train(diag_6_mon ~., 
                       data=train, 
                       method="rf", 
                       metric=metric, 
                       tuneGrid=tunegrid, 
                       trControl=control,
                       ntree=c(1500))

print(custom)
D <- plot(custom)
pdf("../output/T1500_All.pdf")
print(D)
dev.off()
custom$finalModel
custom$results



custom <- caret::train(diag_6_mon ~., 
                       data=train, 
                       method="rf", 
                       metric=metric, 
                       tuneGrid=tunegrid, 
                       trControl=control,
                       ntree=c(2000))

print(custom)
E <- plot(custom)
pdf("../output/T2000_All.pdf")
print(E)
dev.off()
custom$finalModel
custom$results
