library(readxl)
library(TSstudio)
library(dplyr)


####### US World Uncertainty Index
USUncertainty <- read_excel("C:/Users/John/Desktop/USUncertainty.xlsx", skip = 10)
USU <- ts(USUncertainty$WUIUSA, start = c(1952, 1), frequency = 4)
autoplot(USU)
USU2000 <- window(USU, start=c(2000, 1), end=c(2019,4))
autoplot(USU2000)
USUdf <- ts_reshape(USU2000, type = "long", frequency = quarter)
USUdf <- rename(USUdf, USWUI=value)

####### US Housing Starts
USHousingStartsV2 <- read_excel("C:/Users/John/Desktop/USHousingStartsV2.xlsx", skip = 10)
USHS <- ts(USHousingStartsV2$HOUST, start = c(1959, 1), frequency = 12)
autoplot(USHS)
USHS2000 <- window(USU, start=c(2000, 1), end=c(2020, 1))
autoplot(USHS2000)
USHSdf <- ts_reshape(USHS2000, type = "long", frequency = month)
USHSdf <- rename(USHSdf, USHousingStarts=value)

