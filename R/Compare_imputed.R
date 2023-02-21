########### import the imputed datasets
Imputed_train_D <- readRDS("../data/Imputed_train_D_v2.rds")
Imputed_train_V <- readRDS("../data/Imputed_train_V_v2.rds")

########### use the indicator cols in V to select the imputed rows
tmp <- apply(Imputed_train_V[,57:74], 1, sum)
which(tmp>0)

# imputed cols
colnames(Imputed_train_V[57:74])
imputed_cols <- c(6, 8, 11, 12, 13, 21:33)
imputed_V <- Imputed_train_V[which(tmp>0), c(1,imputed_cols)]
imputed_D <- Imputed_train_D[which(tmp>0), c(1,imputed_cols)]

# for each variable, compare distribution
par(mfrow=c(3,3))
for (i in seq_along(imputed_V)) {
  plot(imputed_V[,i], imputed_D[,i], 
       xlab = "V", ylab = "D", 
       main = colnames(imputed_V)[i])
}

cor_data <- vector("numeric", length = 18)
for (i in seq_along(imputed_V)) {
  if(class(imputed_V[,i]) == "numeric") {
    cor_data[i] <- cor(imputed_V[,i], imputed_D[,i])
  }
}
cor_data


chisq.test(imputed_V$Alcohol_freq.0.0, imputed_D$Alcohol_freq.0.0)
chisq.test(imputed_V$highest_Education0, imputed_D$highest_Education0)
table(imputed_V$highest_Education0, imputed_D$highest_Education0)

######### find the outlier
which(imputed_D$Lymphocyte_count.0.0 > 50)
imputed_D[65,]
imputed_V[65,]
#eid = 1216903

###############################################################################
################################# test sets ###################################
###############################################################################

########### import the imputed datasets
Imputed_test_D <- readRDS("../data/Imputed_test_D_v2.rds")
Imputed_test_V <- readRDS("../data/Imputed_test_V_v2.rds")

########### use the indicator cols in V to select the imputed rows
tmp <- apply(Imputed_test_V[,57:74], 1, sum)
which(tmp>0)

# imputed cols
colnames(Imputed_test_V[57:74])
imputed_cols <- c(6, 8, 11, 12, 13, 21:33)
imputed_V <- Imputed_test_V[tmp>0, c(1, imputed_cols)]
imputed_D <- Imputed_test_D[tmp>0, c(1, imputed_cols)]

# for each variable, compare distribution
par(mfrow=c(3,3))
for (i in seq_along(imputed_V)) {
  plot(imputed_V[,i], imputed_D[,i], 
       xlab = "V", ylab = "D", 
       main = colnames(imputed_V)[i])
}

cor_data <- vector("numeric", length = 19)
for (i in seq_along(imputed_V)) {
  if(class(imputed_V[,i]) == "numeric") {
    cor_data[i] <- cor(imputed_V[,i], imputed_D[,i])
  }
}
cor_data


chisq.test(imputed_V$Alcohol_freq.0.0, imputed_D$Alcohol_freq.0.0)
chisq.test(imputed_V$highest_Education0, imputed_D$highest_Education0)
chisq.test(imputed_V$Smoke_status.0.0, imputed_D$Smoke_status.0.0)
table(imputed_V$highest_Education0, imputed_D$highest_Education0)
