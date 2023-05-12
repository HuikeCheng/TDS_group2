# keep a record for some codes for rmarkdown file
library(tidyverse)
library(tableone)
###### TableOne
# df <- readRDS("../data/Matched_data_v2.rds")
# df1 <- readRDS("../data/Matched_data_v3.rds")
# df1 <- df1 %>% mutate(Smoke_status.0.0 = df$Smoke_status.0.0,
#                       Alcohol_freq.0.0 = df$Alcohol_freq.0.0,
#                       highest_Education0 = df$highest_Education0,
#                       Sex = df$Sex)
# # hist(df$Age_recr_continuous.0.0) # left skewed
# # hist(df$BMI.0.0) # right skewed
# # category names
# df1$Alcohol_freq.0.0 <- recode(df1$Alcohol_freq.0.0,
#                                      "daily" = "Daily",
#                                      "weekly3_4" = "3-4 times per week",
#                                      "weekly1_2" = "1-2 times per week",
#                                      "monthly" = "Monthly",
#                                      "rarely" = "Rarely",
#                                      "never" = "Never")
# df1$highest_Education0 <- recode(df1$highest_Education0,
#                                  "A_level" = "A level",
#                                  "Other_professional" = "Other professional")
# df1$Ethnicity.0.0 <- recode(df1$Ethnicity.0.0,
#                                      "Black" = "African",
#                                      "Do not know" = "Prefer not to answer")
# 
# tab <- CreateTableOne(data = df1[,-c(1,2,4,12:36)], strata = "diag_6_mon")
# tab
# tabMat <- print(tab, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE,
#                 nonnormal = c("Age_recr_continuous.0.0", "BMI.0.0"))
# write.csv(tabMat, file = "../output/TableOne.csv")
# tab1 <- read.csv("../output/TableOne.csv")
# colnames(tab1) <- c("", "Control", "Cases", "p", "test")
# tab1 <- tab1[-2,]
# tab2 <- tab1[c(1,2,16,15,3:14,17:31),]
# tab2 <- tab2[c(1:5, 7,8,6, 9:31), ]
# tab2 <- tab2[c(1:12, 16,14,15,13, 17:31), ]
# tab2 <- tab2[c(1:17, 20,21,19,22,23,18, 24:31), ]
# write.csv(tab2, file = "../output/TableOne.csv")
# tab2 <- read.csv("../output/TableOne.csv")
# tab2 <- tab2[,-1]
# colnames(tab2)[1] <- c("")
# tab2[3,1] <- "Age at recruitment (median[IQR])"
# tab2[4,1] <- "BMI (median[IQR])"
# tab2[5,1] <- "Smoking status (%)"
# tab2[9,1] <- "Ethnicity (%)"
# tab2[17,1] <- "Alcohol frequency (%)"
# tab2[24,1] <- "Highest education (%)"
# tab2 <- tab2[c(1:4, 9:16, 5:8, 17:31),]
# write.csv(tab2, file = "../output/TableOne.csv")
