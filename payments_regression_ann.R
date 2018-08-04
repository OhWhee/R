setwd("D:/R/ann")
library(nnet)
library(tidyverse)
library(readxl)
ann <- read_excel("D:/R/ann/ann.xlsx")


pred.n <- 12
g.series <- as.vector(log(t(ann[1:25,2])))
n.obs <- length(g.series)

g.2 <- matrix(rep(0, 13*13), nrow = 13, ncol = 13)
for (i in 1:13)
{
  g.2[i, ] <- g.series[i:(12 + i)]
}

i.seed <- 2
set.seed(12345+i.seed)

g.net <- nnet(g.2[, 1:12], g.2[, 13], size = 3, 
              linout = TRUE, rang=0.1, decay=0.001, maxit = 10000)
g.forecast <- g.2[nrow(g.2), -1] 

pred.1 <- rep(-9999, pred.n)
for (i in 1:pred.n)
{
  pred.1[i] <- predict(g.net, g.forecast, type = "raw")
  g.forecast <- c( g.forecast[-1], pred.1[i])
}

exp(pred.1[1:5])



#size = 3,decay=0.001