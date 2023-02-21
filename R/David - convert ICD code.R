test <- ICD_data

str(test)

str(ICD_data$"C91"==1)

sum(test$"C85" ==1)
sum(test$"208" ==1)

# ICD 9 - 200
test$"C85"[test$"200" == 1] <- 1
test$"200"[test$"200" == 1] <- 0


# ICD 9 - 201
test$"C81"[test$"201" == 1] <- 1
test$"201"[test$"201" == 1] <- 0


# ICD 9 - 202 - exclude
# ICD 9 - 203 - NA

# ICD 9 - 204

test$"C91"[test$"204" == 1] <- 1
test$"204"[test$"204" == 1] <- 0

# ICD 9 - 205 
test$"C92"[test$"205" == 1] <- 1
test$"205"[test$"205" == 1] <- 0


# ICD 9 - 206  - NA
# ICD 9 - 207  - NA


# ICD 9 - 208x

test$"C92"[test$"208" == 1] <- 1
test$"208"[test$"208" == 1] <- 0


#check
sum(ICD_data$"C85" ==1)
sum(ICD_data$"200" ==1)
sum(test$"C85" ==1)
sum(test$"200" ==1)

sum(ICD_data$"C81" ==1)
sum(ICD_data$"201" ==1)
sum(test$"C81" ==1)
sum(test$"201" ==1)

sum(ICD_data$"C91" ==1)
sum(ICD_data$"204" ==1)
sum(test$"C91" ==1)
sum(test$"204" ==1)

sum(ICD_data$"C92" ==1)
sum(ICD_data$"205" ==1)
sum(test$"C92" ==1)
sum(test$"205" ==1)

sum(ICD_data$"C92" ==1)
sum(ICD_data$"208" ==1)
sum(test$"C92" ==1)
sum(test$"208" ==1)

#final for ICD data


# ICD 9 - 200
ICD_data$"C85"[ICD_data$"200" == 1] <- 1
ICD_data$"200"[ICD_data$"200" == 1] <- 0


# ICD 9 - 201
ICD_data$"C81"[ICD_data$"201" == 1] <- 1
ICD_data$"201"[ICD_data$"201" == 1] <- 0


# ICD 9 - 202 - exclude
# ICD 9 - 203 - NA

# ICD 9 - 204

ICD_data$"C91"[ICD_data$"204" == 1] <- 1
ICD_data$"204"[ICD_data$"204" == 1] <- 0

# ICD 9 - 205 
ICD_data$"C92"[ICD_data$"205" == 1] <- 1
ICD_data$"205"[ICD_data$"205" == 1] <- 0


# ICD 9 - 206  - NA
# ICD 9 - 207  - NA


# ICD 9 - 208x

ICD_data$"C92"[ICD_data$"208" == 1] <- 1
ICD_data$"208"[ICD_data$"208" == 1] <- 0
