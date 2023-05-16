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

# check ethnicity
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



#For Practise runs NEEDS SWAP for toy set

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

custom <- caret::train(myeloid ~., 
                        data=train, 
                       method="rf", 
                       metric=metric, 
                       tuneGrid=tunegrid, 
                       trControl=control,
                       ntree=c(250))

print(custom)
A <- plot(custom)
pdf("../output/T250_myeloid.pdf")
print(A)
dev.off()
custom$finalModel
custom$results

custom <- caret::train(myeloid ~., 
                       data=train, 
                       method="rf", 
                       metric=metric, 
                       tuneGrid=tunegrid, 
                       trControl=control,
                       ntree=c(500))

print(custom)
A <- plot(custom)
pdf("../output/T500_myeloid.pdf")
print(A)
dev.off()
custom$finalModel
custom$results


custom <- caret::train(myeloid ~., 
                       data=train, 
                       method="rf", 
                       metric=metric, 
                       tuneGrid=tunegrid, 
                       trControl=control,
                       ntree=c(1000))

print(custom)
C <- plot(custom)
pdf("../output/T1000_myeloid.pdf")
print(C)
dev.off()
custom$finalModel
custom$results

custom <- caret::train(myeloid ~., 
                       data=train, 
                       method="rf", 
                       metric=metric, 
                       tuneGrid=tunegrid, 
                       trControl=control,
                       ntree=c(1500))

print(custom)
D <- plot(custom)
pdf("../output/T1500_myeloid.pdf")
print(D)
dev.off()
custom$finalModel
custom$results



custom <- caret::train(myeloid ~., 
                       data=train, 
                       method="rf", 
                       metric=metric, 
                       tuneGrid=tunegrid, 
                       trControl=control,
                       ntree=c(2000))

print(custom)
E <- plot(custom)
pdf("../output/T2000_myeloid.pdf")
print(E)
dev.off()
custom$finalModel
custom$results



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



