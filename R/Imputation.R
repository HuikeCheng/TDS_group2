############### Imputation
####### load packages
library(caret)
library(purrr)
library(DMwR2)
library(VIM)
####### load data
df <- readRDS("../data/archive/model_data_v2.rds")
# Note: time_to_diagnose in this dataset is for diag_6_mon, 
# if use other outcome vars, need to adjust

############################ diag_6_mon as outcome #############################
###### train-test split
set.seed(1)

train_test_split=createDataPartition(df$diag_6_mon, p = .75, 
                                     list = FALSE, 
                                     times = 1)

train=df[train_test_split, ]
test=df[-train_test_split, ]

summary(train)

############# NA's in df 
nas <- map_dbl(df, function(x) {sum(is.na(x))})
nas
which(nas>0)
################################################################################
#################################### DMwR2 #####################################
################################################################################
############# get the variables used in imputation
X_train <- train[, c(5:8, 10:33)]
X_test <- test[, c(5:8, 10:33)]

map_dbl(X_train, function(x){sum(is.na(x))})

Imputed_X_train <- DMwR2::knnImputation(X_train)
Imputed_X_test <- DMwR2::knnImputation(X_test, distData = X_train) # use train data to impute test

# get the other variables back
Imputed_train <- cbind(train[,1:4], Imputed_X_train[,1:4], train[,9], Imputed_X_train[,5:28], train[,34:56])
Imputed_test <- cbind(test[,1:4], Imputed_X_test[,1:4], test[,9], Imputed_X_test[,5:28], test[,34:56])

saveRDS(Imputed_train, "../data/Imputed_train_D_v2.rds")
saveRDS(Imputed_test, "../data/Imputed_test_D_v2.rds")

################################################################################
####################################### VIM ####################################
################################################################################
vars_2_imp <- colnames(df)[nas > 0]
vars_dist <- colnames(X_train)

Imputed_train <- VIM::kNN(data = train, variable = vars_2_imp, dist_var = vars_dist)
Imputed_test <- VIM::kNN(data = test, variable = vars_2_imp, dist_var = vars_dist)

saveRDS(Imputed_train, "../data/Imputed_train_V_v2.rds")
saveRDS(Imputed_test, "../data/Imputed_test_V_v2.rds")

############# try
# X_train_tmp <- X_train[sample(1:nrow(X_train), 1000),]
# map_dbl(X_train_tmp, function(x) {sum(is.na(x))})
# Imputed_X_train_tmp <- knnImputation(X_train_tmp)
# map_dbl(Imputed_X_train_tmp, function(x) {sum(is.na(x))})

# train_tmp <- train[sample(1:nrow(X_train), 1000),]
# map_dbl(train_tmp, function(x) {sum(is.na(x))})
# Imputed_train_tmp <- VIM::kNN(data = train_tmp, variable = vars_2_imp, dist_var = vars_dist)
# map_dbl(Imputed_train_tmp, function(x) {sum(is.na(x))})
