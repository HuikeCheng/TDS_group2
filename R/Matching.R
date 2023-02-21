######### load package
library(MatchIt)
library(dplyr)
## Load data
Imputed_train <- readRDS("../data/Imputed_train_V_v2.rds")

table(Imputed_train$diag_6_mon)
5485*2
#####
set.seed(1)
Matching <- matchit(diag_6_mon ~ date_recr + Sex + Ethnicity.0.0 + Age_recr_continuous.0.0,
                        data = Imputed_train)
Matched_ctrl <- Imputed_train %>% filter(rownames(Imputed_train) %in% Matching[["match.matrix"]])

Cases <- Imputed_train[Imputed_train$diag_6_mon == 1, ]

Matched_data <- rbind(Cases, Matched_ctrl)

##### remove the imputation indicator cols
Matched_data <- Matched_data[,-c(57:74)]

######## save data
# saveRDS(Matched_data, "../data/Matched_data_v2.rds")
