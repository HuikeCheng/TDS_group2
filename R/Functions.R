################# load packages
library(purrr)
library(dplyr)
library(rlist)

############## get first 3 characters ###############
get_first3 <- function(x) {substr(x, 1, 3)}

############## count NAs
count_not_na <- function(x) {sum(!is.na(x))}

############# percentage na
prct_na <- function(x) {round(sum(is.na(x))/length(x),3)}

############# get nrow for an element ##############
# get_ele_row <- function(x) {dim(x)[[1]]}

############ remove duplicated rows for each element ############
rm_dup_row <-function(x) {x[!duplicated(cbind(x$HES_ICD, x$REG_ICD)),]}

############ expand data
expand_data <- function(x) {
  icd <- c(x$HES_ICD, x$REG_ICD)
  icd <- icd[!is.na(icd)]
  icd <- unique(icd)
  out <- c(x$eid[1], icd)
  names(out)[[1]] <- "eid"
  return(out)
}

############ extend each element to length of 5 ########
extend_data <- function(x, req_length = 9) {
  if (length(x) < req_length) {
    n_na <- req_length - length(x)
    x <- c(x, rep(NA, n_na))
  } else {x <- x}
}

