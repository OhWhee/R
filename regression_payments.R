setwd("D:/R/regression")
library(dplyr)
library(readxl)
library(car)
reg <- read_excel("D:/R/regression/ нига1.xlsx")

real <- reg$`—умма по полю Release`
real[26:(25+11)] <- NA
payments <- reg$Payments
payments[26:(25+11)] <- NA
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

regdf <- data.frame(payments, times, jul, aug, sep, oct, nov, dec, jan, feb, mar, apr, may, jun)

res.01 <- lm(payments ~ aug+sep+oct+nov+dec+jan+feb+mar+apr+may+jun)
summary(res.01)
fittedvals <- data.frame(res.01$fitted.values)
fittedvals2 <- data.frame(res.01$fitted.values)

fittedtbl <- cbind(reg, fittedvals, fittedvals2)
predicted <- data.frame(predict.lm(res.01, regdf))

plot(predicted$predict.lm.res.01..regdf., type="l", col = "red")
lines(res.01$fitted.values, col = "red")
lines(reg$Payments, col = "blue")
write.csv(regdf, file = "predictionpayments.csv")
