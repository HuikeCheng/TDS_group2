########## load library
library(tidyverse)
library(rlist)

########## load data
intermediate_data <- readRDS("data/intermediate_data.rds")
outcome_data <- readRDS("data/outcome_data.rds")

############ get UK Biobank icd10 blood cancer cases
icd10_codes<-c(paste0("C", c(81:96,810:969)), paste0("D", c(45:47,450:479)))
a <- vector(mode = "list", length = 17)
for (i in c(1:17)) {
  j = i + 73
  a[[i]] <- intermediate_data[,j] %in% icd10_codes
}
b1 <- list.rbind(a)
dim(b1)
index <- colSums(b1) != 0
sum(index)

############## get UK Biobank icd9 blood cancer cases
icd9_codes<-c(c(200:208, 2001:2089), 2384, 2386, 2387)
a <- vector(mode = "list", length = 15)
for (i in c(1:15)) {
  j = i + 90
  a[[i]] <- intermediate_data[,j] %in% icd9_codes
}
b2 <- list.rbind(a)
dim(b2)
index <- colSums(b2) != 0
sum(index)

########### combine icd9 and icd10 cases from UK Biobank
b <- rbind(b1,b2)
dim(b)
index_icd <- colSums(b) != 0
sum(index_icd)

case_icd <- intermediate_data[index_icd,c(74:105)]

################# using outcome data from registries
index_reg <- which(outcome_data$case == 1)
case_reg <- intermediate_data[index_reg,c(74:105)]

index3 <- which(!(rownames(case_reg) %in% rownames(case_icd)))
case_prob <- case_reg[index3,]

index.both <- intersect(rownames(case_reg), rownames(case_icd))
table(rownames(case_reg) %in% index.both)
table(rownames(case_icd) %in% index.both)




############## using outcome data from hes(full code without .)
outcome_full_code <- readRDS("data/outcome_full_code.rds")

index_reg2 <- which(output_full_code$case == 1)
case_reg2 <- intermediate_data[index_reg2, c(74:105)]

index4 <- which(!(rownames(case_reg2) %in% rownames(case_icd)))
case_prob2 <- case_reg2[index4,]




############## using outcome data from hes(full code with dot)
output_final <- readRDS("outcome_definition/Outputs/output_final.rds")
index_reg3 <- which(output_final$case == 1)
case_reg3 <- intermediate_data[index_reg3, c(74:105)]

index5 <- which(!(rownames(case_reg3) %in% rownames(case_icd)))
case_prob3 <- case_reg3[index5,]
