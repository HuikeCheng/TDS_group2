library(tidyverse)

head(test)

test <- ICD_data

str(test)
summary(test)

test <- test %>% mutate(lymphoid =
                      ifelse(C81 == 1 | C82 ==1 | C83 ==1 | C84 ==1 | C85 ==1 | C86 ==1 | C88 ==1 | C90 ==1, 1, 0))

summary(test)
#check
table(test$lymphoid) #3702 cases

sum(test$C81+test$C82+test$C83+test$C84+test$C85+test$C86+test$C88+test$C90) #4268 cases likely due to rows having more than 1 cancer

test <- test%>% mutate(myeloid =
                 ifelse(C92 == 1 | C93 ==1 | C94 ==1 | D45 ==1 | D46 ==1, 1, 0))


test <- test%>% mutate(H_D_cell =
                 ifelse(C96 == 1, 1, 0))


test <- test%>% mutate(other =
                 ifelse(C95 == 1 | D47 ==1, 1, 0))

summary(test)

# for the ICD_data 

ICD_data <- ICD_data %>% mutate(lymphoid =
                          ifelse(C81 == 1 | C82 ==1 | C83 ==1 | C84 ==1 | C85 ==1 | C86 ==1 | C88 ==1 | C90 ==1, 1, 0))

ICD_data <- ICD_data%>% mutate(myeloid =
                         ifelse(C92 == 1 | C93 ==1 | C94 ==1 | D45 ==1 | D46 ==1, 1, 0))

ICD_data <- ICD_data%>% mutate(H_D_cell =
                         ifelse(C96 == 1, 1, 0))

ICD_data <- ICD_data%>% mutate(other =
                         ifelse(C95 == 1 | D47 ==1, 1, 0))