setwd("D:/R/regression")
library(dplyr)
library(readxl)
library(car)
reg <- read_excel("D:/R/regression/ нига1.xlsx")

realisation <- reg$`—умма по полю Release`
realisation[26:(25+11)] <- NA
times <- reg$times
times[26:(25+11)] <- 31:42

jul <- rep(c(1,0,0,0,0,0,0,0,0,0,0,0), 2+1)
aug <- rep(c(0,1,0,0,0,0,0,0,0,0,0,0), 2+1)
sep <- rep(c(0,0,1,0,0,0,0,0,0,0,0,0), 2+1)
oct <- rep(c(0,0,0,1,0,0,0,0,0,0,0,0), 2+1)
nov <- rep(c(0,0,0,0,1,0,0,0,0,0,0,0), 2+1)
dec <- rep(c(0,0,0,0,0,1,0,0,0,0,0,0), 2+1)
jan <- rep(c(0,0,0,0,0,0,1,0,0,0,0,0), 2+1)
feb <- rep(c(0,0,0,0,0,0,0,1,0,0,0,0), 2+1)
mar <- rep(c(0,0,0,0,0,0,0,0,1,0,0,0), 2+1)
apr <- rep(c(0,0,0,0,0,0,0,0,0,1,0,0), 2+1)
may <- rep(c(0,0,0,0,0,0,0,0,0,0,1,0), 2+1)
jun <- rep(c(0,0,0,0,0,0,0,0,0,0,0,1), 2+1)

regdf <- data.frame(realisation, times, jul, aug, sep, oct, nov, dec, jan, feb, mar, apr, may, jun)

res.01 <- lm(realisation ~ times + aug+sep+oct+nov+dec+jan+feb+mar+apr+may+jun)
summary(res.01)
var1 <- data.frame(res.01$fitted.values)
fitted <- cbind(reg, var1)

plot(reg$`—умма по полю Release`, type="l", col = "green")
lines(res.01$fitted.values, col = "red")

predicted <- data.frame(predict.lm(res.01, regdf))
regdf <- cbind(regdf, predicted)
plot(predicted$predict.lm.res.01..regdf., col = "red", type="l")
lines(reg$`—умма по полю Release`, col="blue")



regdf2 <- ts(data = reg$`—умма по полю Release`, frequency = 12, start = c(2015, 7))
regdf2HW <- HoltWinters(regdf2, seasonal = "additive")
plot(regdf2HW)
regdf2HW$fitted
regdf2HW_predict <- predict(regdf2HW, n.ahead = 12)
plot(fitted(regdf2HW))
ts.plot(regdf2, regdf2HW$fitted[,1], regdf2HW_predict, col = c("blue", "orange", "red"))

write.csv(regdf, file = "prediction.csv")
