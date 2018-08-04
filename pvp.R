setwd("D:/R/clustering")
library(readxl)
library(tidyverse)
library(NbClust)
characters <- read_excel("C:/Users/OhWhee/Desktop/characters.xlsx")
pvp <-characters%>%
  select(char_name, 13:14)
set.seed(1123)
k <- kmeans(pvp[,2:3], 4, iter.max = 20, nstart = 20)
plot(pvp, col = clus.1$cluster)

pvp_clustered <- pvp%>%
  mutate(cluster = k$cluster)
write.csv(look, file = "pvp.csv")



clus.1 <- kmeans(pvp[,2:3], 3, iter.max=15, algorithm = "Lloyd")
clus.2 <- kmeans(pvp$pvpkills, centers=clus.1$centers,iter.max=1, algorithm = "Lloyd")
clus.3 <- kmeans(pvp$pvpkills, centers=clus.2$centers,iter.max=1, algorithm = "Lloyd")

clus.4 <- kmeans(pvp$pvpkills, centers=clus.3$centers,iter.max=1, algorithm = "Lloyd")
clus.5 <- kmeans(pvp$pvpkills, centers=clus.4$centers,iter.max=1, algorithm = "Lloyd")
clus.6 <- kmeans(pvp$pvpkills, centers=clus.5$centers,iter.max=1, algorithm = "Lloyd")

clus.7 <- kmeans(pvp$pvpkills, centers=clus.6$centers,iter.max=1, algorithm = "Lloyd")
clus.8 <- kmeans(pvp$pvpkills, centers=clus.7$centers,iter.max=1, algorithm = "Lloyd")
clus.9 <- kmeans(pvp$pvpkills, centers=clus.8$centers,iter.max=1, algorithm = "Lloyd")
clus.10 <- kmeans(pvp$pvpkills, centers=clus.9$centers,iter.max=1, algorithm = "Lloyd")
clus.11 <- kmeans(pvp$pvpkills, centers=clus.10$centers,iter.max=1, algorithm = "Lloyd")
clus.12 <- kmeans(pvp$pvpkills, centers=clus.11$centers,iter.max=1, algorithm = "Lloyd")
clus.13 <- kmeans(pvp$pvpkills, centers=clus.12$centers,iter.max=1, algorithm = "Lloyd")
clus.14 <- kmeans(pvp$pvpkills, centers=clus.13$centers,iter.max=1, algorithm = "Lloyd")
clus.15 <- kmeans(pvp$pvpkills, centers=clus.14$centers,iter.max=1, algorithm = "Lloyd")

look <- cbind(pvp, clus.1$cluster)
clsnum <- NbClust(pvp[,2:3], distance = "euclidean", method = "kmeans", index = "kl")