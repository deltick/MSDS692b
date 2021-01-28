library(dynlm)
library(forecast)
library(tseries)
library(vars)

library(readxl)
USMonthlyEconData <- read_excel("Data/USMonthlyEconData.xlsx")

usgdp <- ts(USMonthlyEconData$USGDP, start=c(1960, 01), frequency=12)
ushs  <- ts(USMonthlyEconData$USHousingStarts, start=c(1960, 01), frequency=12)
usemp <- ts(USMonthlyEconData$USUnemployment, start=c(1960, 01), frequency=12)
#plot(usgdp)

usgdp <- window(usgdp, start=c(2000, 10), end=c(2019, 12))
ushs  <- window(ushs, start=c(2000, 10), end=c(2019, 12))
usemp <- window(usemp, start=c(2000, 10), end=c(2019, 12))

usmonthly <- cbind(usgdp, ushs, usemp)

plot(usmonthly)

acf(usmonthly[,1], main="U.S. GDP")
acf(usmonthly[,2], main="U.S. Housing Starts")
acf(usmonthly[,3], main="U.S. Unemployment Rate")


