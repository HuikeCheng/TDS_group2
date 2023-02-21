######### load packages
library(dplyr)
library(purrr)
library(tsne)
library(umap)
library(ggplot2)
######### load data
df <- readRDS("../data/Matched_data_v2.rds")

############################### prepare dataset ################################
####### Code alcohol_freq, education and smoking as ordinal
df$Alcohol_freq.0.0 <- as.character(df$Alcohol_freq.0.0)
df$Alcohol_freq.0.0 <- recode(df$Alcohol_freq.0.0, 
                              "daily" = "6",
                              "weekly3_4" = "5",
                              "weekly1_2" = "4",
                              "monthly" = "3",
                              "rarely" = "2",
                              "never" = "1")
df$Alcohol_freq.0.0 <- as.numeric(df$Alcohol_freq.0.0)

#### highest education
df$highest_Education0 <- as.character(df$highest_Education0)
df$highest_Education0 <- recode(df$highest_Education0, 
                                "College" = "1",
                                "A_level" = "2",
                                "GCSE" = "3",
                                "CSE" = "4",
                                "NVQ" = "5",
                                "Other_professional" = "6",
                                "Others" = "7")
df$highest_Education0 <- as.numeric(df$highest_Education0)

####### smoking
df$Smoke_status.0.0 <- as.character(df$Smoke_status.0.0)
df$Smoke_status.0.0 <- recode(df$Smoke_status.0.0, 
                              "Never" = "1",
                              "Previous" = "2",
                              "Current" = "3")
df$Smoke_status.0.0 <- as.numeric(df$Smoke_status.0.0)

####### sex
df$Sex <- ifelse(df$Sex == "Female", 0, 1)
####### Ethnicity -> dummy variables
X <- df[,c(5:8, 10:33)]
newX <- model.matrix(~., data = X)[,-1]

######### T-sne plot
set.seed(0)
tsne <- tsne(newX, initial_dims = 2)
tsne <- data.frame(tsne)
tsne$y <- as.factor(df$diag_6_mon)

plt_tsne <- ggplot(data = tsne, aes(X1, X2, color = y)) + 
  geom_point() +
  ggtitle("T-sne")

######### UMAP
plot.umap = umap(newX, n_components = 2, random_state = 15) 
layout <- plot.umap[["layout"]] 
layout <- data.frame(layout) 
layout$y <- as.factor(df$diag_6_mon)

plt_umap <- ggplot(data = layout, aes(X1, X2, color = y)) + 
  geom_point() +
  ggtitle("UMAP")

####### save plot
pdf("../output/clus_plots.pdf")
print(plt_tsne)     # Plot 1 --> in the first page of PDF
print(plt_umap)     # Plot 2 ---> in the second page of the PDF
dev.off() 

######### try
# tsne
# index <- sample(c(1:10970), 200)
# tmp <- newX[index,]
# tsne <- tsne(tmp, initial_dims = 2)
# tsne <- data.frame(tsne)
# tmp1 <- tsne
# tmp1$y <- df$diag_6_mon[index]
# tmp1$y <- as.factor(tmp1$y)
# 
# plt_tsne <- ggplot(data = tmp1, aes(X1, X2, color = y)) + geom_point()

# umap
# plot.umap = umap(tmp, n_components = 2, random_state = 15) 
# layout <- plot.umap[["layout"]] 
# layout <- data.frame(layout) 
# tmp2 <- layout
# tmp2$y <- as.factor(df$diag_6_mon[index])
# 
# plt_umap <- ggplot(data = tmp2, aes(X1, X2, color = y)) + geom_point()
# plt_umap
