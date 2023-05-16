library(h2o)
library(tidyverse)
library(lares)

invisible(h2o.init())

#set up 
df <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group2/data/Matched_data_v3.rds")
df <- df[, c(3,5:9, 10:32)]
df$diag_6_mon <- as.factor(df$diag_6_mon)
#colnames(df)
#summary(df)
#str(df)

#data split
set.seed(1)
parts = createDataPartition(df$diag_6_mon, p = .8, list = F)
train = df[parts, ]
test = df[-parts, ]

train_h = as.h2o(train)
test_h = as.h2o(test)

y = 'diag_6_mon'
pred = setdiff(names(train), y)

train[,y] = as.factor(train[,y])
test[,y] = as.factor(test[,y])

aml = h2o.automl(x = pred, y = y,
                 training_frame = train_h,
                 max_models = 20,
                 seed = 1,
                 max_runtime_secs = 120
)

# Explain leader model & compare with all AutoML models
exa <- h2o.explain(aml, test)
exa

# Explain a single H2O model (e.g. leader model from AutoML)
exm <- h2o.explain(aml@leader, test)
exm

plot(aml)

# AutoML Leaderboard
lb = aml@leaderboard
lb


# prediction result on test data
prediction = h2o.predict(aml@leader, test_h) %>%
  as.data.frame()

aml$metrics

# create a confusion matrix
caret::confusionMatrix(test$diag_6_mon, prediction$predict)


# close h2o connection
h2o.shutdown(prompt = F)



