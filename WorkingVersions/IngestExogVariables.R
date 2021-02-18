library(readxl)
library(TSstudio)
library(dplyr)
library(forecast)

urlfile="https://raw.githubusercontent.com/deltick/MSDS692b/main/Data/USBPTS.csv"
inputTS<-read_csv(url(urlfile))
inputSel2 <- inputTS %>% select("Southwest Border")
inputSel2 <- dplyr::rename(inputSel2, Value = "Southwest Border")
swb.ts <- ts(inputSel2, start=c(2000,1), frequency=12)


urlfile2="https://raw.githubusercontent.com/deltick/MSDS692b/main/Data/USMonthlyEconData.csv"
inputEcon <- read_csv(url(urlfile2))


gdp <- inputEcon %>% select(USGDP)
housing <- inputEcon %>% select(USHousingStarts)
unemp <- inputEcon %>% select(USUnemployment)


gdp.ts <- ts(gdp, start=c(1960, 1), frequency=12)
gdp.ts <- window(gdp.ts, start=c(2000, 1), end=c(2019,12))
forecast::autoplot(gdp.ts)

housing.ts <- ts(housing, start=c(1960, 1), frequency=12)
housing.ts <- window(housing.ts, start=c(2000, 1), end=c(2019,12))
forecast::autoplot(housing.ts)

unemp.ts <- ts(unemp, start=c(1960, 1), frequency=12)
unemp.ts <- window(unemp.ts, start=c(2000, 1), end=c(2019,12))
forecast::autoplot(unemp.ts)

econ.ts <- cbind(gdp.ts, housing.ts, unemp.ts)
forecast::autoplot(econ.ts)

swb.scaled.ts <- scale(swb.ts)
gdp.scaled.ts <- scale(gdp.ts)
housing.scaled.ts <- scale(housing.ts)
unemp.scaled.ts <- scale(unemp.ts)

combined.ts <- cbind(swb.scaled.ts, gdp.scaled.ts, housing.scaled.ts, unemp.scaled.ts )
autoplot(combined.ts)




