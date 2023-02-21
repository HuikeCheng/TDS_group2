############
library(purrr)
library(tidyr)
library(rlist)
library(dplyr)
source("R/Functions.R")
########## load data
mydata <- readRDS("data/data_healthy_control.rds")

########## get eid and ICD codes
mydata <- mydata[,c(1, 45:56)]

########## create vector of ICD codes to look for
icd_codes <- c(paste0("C", 81:96), 
               "D45", "D46", "D47", 
               as.character(200:208), 
               "2384", "2386", "2387")

######### alter NA values in data
mylist <- as.list(rep("0", 13))
names(mylist) <- colnames(mydata)
mydata <- replace_na(mydata, mylist)

########## for each icd_code, search through icd1 - 4 columns, if match, record as HES
output <- vector("list", length = length(icd_codes))
for (i in seq_along(icd_codes)) {
  icd <- icd_codes[i]
  hes <- mydata[,2] == icd | mydata[,3] == icd | mydata[,4] == icd | mydata[,5] == icd
  ukb <- mydata[,6] == icd | mydata[,7] == icd | mydata[,8] == icd | mydata[,9] == icd | 
    mydata[,10] == icd | mydata[, 11] == icd | mydata[,12] == icd | mydata[,13] == icd
  out <- cmp_lgl(hes, ukb, "Both", "HES", "UKB", "None")
  output[[i]] <- out
  names(output)[i] <- icd
}

######## turn output to data frame
ICD_source <- as.data.frame(list.cbind(output))
######## replace None with NA
ICD_source <- na_if(ICD_source, "None")
######## find ICD cols which are empty
em_col <- which(map_dbl(ICD_source, count_not_na) == 0)
####### remove these cols
ICD_source <- ICD_source[,-em_col]
###### add eid column
ICD_source$eid <- mydata$eid
ICD_source <- ICD_source[,c(24,1:23)]

# saveRDS(ICD_source, "data/ICD_source.rds")

####### dummy data version
mylist <- as.list(rep("0", 24))
names(mylist) <- colnames(ICD_source)
###### replace NA with 0
ICD_data <- replace_na(ICD_source, mylist)

#### replace none zero with 1
ICD_data[ICD_data != "0"] <- "1"
ICD_data$eid <- ICD_source$eid

#### change icd columns to numeric
for (i in 2:24) {
  ICD_data[, i] <- as.numeric(ICD_data[, i])
}

# saveRDS(ICD_data, "data/ICD_data.rds")


################################ exploration ##################################
# find people with ICD9 but no ICD10
find_rec <- function(x) {
  a <- sum(x[2:18])
  b <- sum(x[19:24])
  d <- a == 0 & b != 0
  return(d)
}

output <- vector("logical", length = nrow(ICD_data))
for (i in 1:nrow(ICD_data)) {
  output[i] <- find_rec(ICD_data[i,])
}

sum(output)
a <- ICD_data[which(output),c(19:24)]
map(a, sum)
