######### load package
library(hopkins)
library(NbClust)
library(factoextra)
library(sharp)
library(tidyverse)
library(ggplot2)

######## load data
df <- readRDS("../data/Matched_data_v2.rds")

########### select the blood counts
bcdata <- df[,13:33]
bcdata <- scale(bcdata)

######### clustering tendency
hopkins(bcdata)
#fviz_dist(dist(tmp), show_labels = FALSE)

######## predict optimal number of clusters
#fviz_nbclust(bcdata, pam, method = "silhouette")+ theme_classic() # 2

#################################### K Means ###################################
kmc <- Clustering(xdata = bcdata, 
                  nc = c(2,3,4),
                  seed = 1,
                  row = TRUE,
                  implementation = KMeansClustering,
                  scale = TRUE)
# params
index <- which.max(kmc$Sc)
kmc$nc[index]
kmc$Sc[index]

# result
tmp1 <- df %>% mutate(kmc = Clusters(kmc),
                      kmc = as.factor(kmc))
# plots
plot1 <- ggplot(aes(x=Wbc_count.0.0, y=Rbc_count.0.0), data = tmp1) +
  geom_point(aes(color = kmc)) + 
  theme_bw() +
  ggtitle("KMeans") +
  xlab("WBC count") +
  ylab("RBC count") +
  theme(legend.title=element_blank())

plot2 <- ggplot(aes(x=Reticulocyte_count.0.0, y=Ir_fraction.0.0), data = tmp1) +
  geom_point(aes(color = kmc)) + 
  theme_bw() +
  ggtitle("KMeans") +
  xlab("Reticulocyte count") +
  ylab("Immature reticulocyte fraction") +
  theme(legend.title=element_blank())

pdf("../output/kmc.pdf")
print(plot1)
print(plot2)
#plot(kmc)
dev.off()

######## save output data
saveRDS(tmp1, "../data/clus_membership.rds")

############################# PAM Clustering ###################################
# pc <- Clustering(xdata = bcdata, 
#                  seed = 1,
#                  row = TRUE,
#                  implementation = PAMClustering,
#                  scale = TRUE)
# # params
# index <- which.max(pc$Sc)
# pc$nc[index]
# pc$Sc[index]
# 
# # result
# tmp1 <- tmp1 %>% mutate(pc = Clusters(pc),
#                         pc = as.factor(pc))
# # plots
# plot1 <- ggplot(aes(x=Wbc_count.0.0, y=Rbc_count.0.0), data = tmp1) +
#   geom_point(aes(color = pc)) + 
#   theme_bw() +
#   ggtitle("PAM") +
#   xlab("WBC count") +
#   ylab("RBC count") +
#   theme(legend.title=element_blank())
# 
# plot2 <- ggplot(aes(x=Reticulocyte_count.0.0, y=Ir_fraction.0.0), data = tmp1) +
#   geom_point(aes(color = pc)) + 
#   theme_bw() +
#   ggtitle("PAM") +
#   xlab("WBC count") +
#   ylab("RBC count") +
#   theme(legend.title=element_blank())
# 
# pdf("../output/pc.pdf")
# print(plot1)
# print(plot2)
# #plot(pc)
# dev.off()

############################# GMM Clustering ###################################
# gc <- Clustering(xdata = bcdata, 
#                  seed = 1,
#                  row = TRUE,
#                  implementation = GMMClustering,
#                  scale = TRUE)
# # params
# index <- which.max(gc$Sc)
# gc$nc[index]
# gc$Sc[index]
# 
# # result
# tmp1 <- tmp1 %>% mutate(gc = Clusters(gc),
#                         gc = as.factor(gc))
# # plots
# plot1 <- ggplot(aes(x=Wbc_count.0.0, y=Rbc_count.0.0), data = tmp1) +
#   geom_point(aes(color = gc)) + 
#   theme_bw() +
#   ggtitle("GMM") +
#   xlab("WBC count") +
#   ylab("RBC count") +
#   theme(legend.title=element_blank())
# 
# plot2 <- ggplot(aes(x=Reticulocyte_count.0.0, y=Ir_fraction.0.0), data = tmp1) +
#   geom_point(aes(color = gc)) + 
#   theme_bw() +
#   ggtitle("GMM") +
#   xlab("Reticulocyte count") +
#   ylab("Immature reticulocyte fraction") +
#   theme(legend.title=element_blank())
# 
# pdf("../output/gc.pdf")
# print(plot1)
# print(plot2)
# #plot(gc)
# dev.off()
# 
# ######## save output data
# saveRDS(tmp1, "../data/clus_membership.rds")

################################################################################
##################################### try ######################################
################################################################################
# set.seed(1)
# tmp <- bcdata[sample(c(1:10970), 100),]
# tmp <- as.data.frame(tmp)
# ########## K Means
# kmc <- Clustering(xdata = tmp, 
#                   nc = c(2,3,4,5,6,7,8),
#                   seed = 1,
#                   row = TRUE,
#                   implementation = KMeansClustering,
#                   scale = TRUE)
# # params
# index <- which.max(kmc$Sc)
# kmc$nc[index]
# kmc$Sc[index]
# 
# # result
# tmp1 <- tmp %>% mutate(kmc = Clusters(kmc),
#                       kmc = as.factor(kmc))
# # plots
# plot1 <- ggplot(aes(x=Wbc_count.0.0, y=Rbc_count.0.0), data = tmp1) +
#   geom_point(aes(color = kmc)) + 
#   theme_bw() +
#   ggtitle("KMeans") +
#   xlab("WBC count") +
#   ylab("RBC count") +
#   theme(legend.title=element_blank())
# 
# plot2 <- ggplot(aes(x=Reticulocyte_count.0.0, y=Ir_fraction.0.0), data = tmp1) +
#   geom_point(aes(color = kmc)) + 
#   theme_bw() +
#   ggtitle("KMeans") +
#   xlab("WBC count") +
#   ylab("RBC count") +
#   theme(legend.title=element_blank())
# 
# pdf("../output/test.pdf")
# print(plot1)
# print(plot2)
# plot(kmc)
# dev.off()
# 
# ######### Hierarchical Clustering
# hc <- Clustering(xdata = tmp, 
#                  seed = 1,
#                  row = TRUE,
#                  implementation = HierarchicalClustering,
#                  scale = TRUE)
# # params
# index <- which.max(hc$Sc)
# hc$nc[index]
# hc$Sc[index]
# 
# # result
# tmp1 <- tmp1 %>% mutate(hc = Clusters(hc),
#                         hc = as.factor(hc))
# # plots
# plot1 <- ggplot(aes(x=Wbc_count.0.0, y=Rbc_count.0.0), data = tmp1) +
#   geom_point(aes(color = hc)) + 
#   theme_bw() +
#   ggtitle("Hierarchical clustering") +
#   xlab("WBC count") +
#   ylab("RBC count") +
#   theme(legend.title=element_blank())
# 
# plot2 <- ggplot(aes(x=Reticulocyte_count.0.0, y=Ir_fraction.0.0), data = tmp1) +
#   geom_point(aes(color = hc)) + 
#   theme_bw() +
#   ggtitle("Hierarchical clustering") +
#   xlab("WBC count") +
#   ylab("RBC count") +
#   theme(legend.title=element_blank())
# 
# pdf("../output/test.pdf")
# print(plot1)
# print(plot2)
# plot(hc)
# dev.off()
# 
# ########## PAM Clustering
# pc <- Clustering(xdata = tmp, 
#                  seed = 1,
#                  row = TRUE,
#                  implementation = PAMClustering,
#                  scale = TRUE)
# # params
# index <- which.max(pc$Sc)
# pc$nc[index]
# pc$Sc[index]
# 
# # result
# tmp1 <- tmp1 %>% mutate(pc = Clusters(pc),
#                         pc = as.factor(pc))
# # plots
# plot1 <- ggplot(aes(x=Wbc_count.0.0, y=Rbc_count.0.0), data = tmp1) +
#   geom_point(aes(color = pc)) + 
#   theme_bw() +
#   ggtitle("PAM") +
#   xlab("WBC count") +
#   ylab("RBC count") +
#   theme(legend.title=element_blank())
# 
# plot2 <- ggplot(aes(x=Reticulocyte_count.0.0, y=Ir_fraction.0.0), data = tmp1) +
#   geom_point(aes(color = pc)) + 
#   theme_bw() +
#   ggtitle("PAM") +
#   xlab("WBC count") +
#   ylab("RBC count") +
#   theme(legend.title=element_blank())
# 
# pdf("../output/test.pdf")
# print(plot1)
# print(plot2)
# plot(pc)
# dev.off()
# 
# ####### GMM Clustering
# gc <- Clustering(xdata = tmp, 
#                  seed = 1,
#                  row = TRUE,
#                  implementation = GMMClustering,
#                  scale = TRUE)
# # params
# index <- which.max(gc$Sc)
# gc$nc[index]
# gc$Sc[index]
# 
# # result
# tmp1 <- tmp1 %>% mutate(gc = Clusters(gc),
#                         gc = as.factor(gc))
# # plots
# plot1 <- ggplot(aes(x=Wbc_count.0.0, y=Rbc_count.0.0), data = tmp1) +
#   geom_point(aes(color = gc)) + 
#   theme_bw() +
#   ggtitle("GMM") +
#   xlab("WBC count") +
#   ylab("RBC count") +
#   theme(legend.title=element_blank())
# 
# plot2 <- ggplot(aes(x=Reticulocyte_count.0.0, y=Ir_fraction.0.0), data = tmp1) +
#   geom_point(aes(color = gc)) + 
#   theme_bw() +
#   ggtitle("GMM") +
#   xlab("WBC count") +
#   ylab("RBC count") +
#   theme(legend.title=element_blank())
# 
# pdf("../output/test.pdf")
# print(plot1)
# print(plot2)
# plot(gc)
# dev.off()
