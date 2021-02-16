library(readxl)
library(TSstudio)
library(dplyr)
library(forecast)
library(readr)
#library(timeSeries)
library(vars)
library(dynlm)
library(ggplot2)
library(tseries)
require(MTS)
library(tidyverse)
## Read and format input files from Github repo

urlfile="https://raw.githubusercontent.com/deltick/MSDS692b/main/Data/USBPTS.csv"
inputTS<-read_csv(url(urlfile))
inputSel2 <- inputTS %>% dplyr::select(xDate, "Southwest Border")
inputSel2 <- dplyr::rename(inputSel2, Value = "Southwest Border")
swb.ts <- ts(inputSel2, start=c(2000,1), frequency=12)

urlfile2="https://raw.githubusercontent.com/deltick/MSDS692b/main/Data/USMonthlyEconData.csv"
inputEcon <- read_csv(url(urlfile2))

## Create separate vectors for econ variables

gdp <- inputEcon %>% select(USGDP)
housing <- inputEcon %>% select(USHousingStarts)
unemp <- inputEcon %>% select(USUnemployment)

## Create time series object and set matching windows

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

## Center and scale all variables

swb.scaled.ts <- scale(swb.ts)
gdp.scaled.ts <- scale(gdp.ts)
housing.scaled.ts <- scale(housing.ts)
unemp.scaled.ts <- scale(unemp.ts)

## Combine series and plot

combined.ts <- cbind(swb.scaled.ts, gdp.scaled.ts, housing.scaled.ts, unemp.scaled.ts )
autoplot(combined.ts) +
  ggtitle('Plot of Scaled Series')

## #Examine correlations between vectors

cor(combined.ts)

acf(combined.ts[,3],main="ACF Housing Starts")
pacf(combined.ts[,3],main="PACF Housing Starts")

# Look at cross-correlations 

ccf(combined.ts[,1], combined.ts[,3],main="CCF Housing Starts")
ccf(combined.ts[,1], combined.ts[,2],main="CCF GDP")
ccf(combined.ts[,1], combined.ts[,4],main="CCF Unemployment")

## ADF Tests:  Null hypothesis--Not stationary.  p-value <0.5 reject Null

adf.test(combined.ts[,1])
adf.test(combined.ts[,2])
adf.test(combined.ts[,3])
adf.test(combined.ts[,4])

ndiffs((combined.ts[,1]))
ndiffs((combined.ts[,2]))
ndiffs((combined.ts[,3]))
ndiffs((combined.ts[,4]))


## Split data set
swb.train <- window(swb.ts, end=c(2017,12))
swb.test  <- window(swb.ts, start=c(2018,1))

## Univariate Test with Holt-Winters

fit.holt<-holt(swb.train, h=24, initial="optimal")
summary(fit.holt)
plot(forecast(fit.holt))
lines(swb.test, type='o')

## Univariate Test with auto ARIMA

fit.arima <- auto.arima(swb.train)
summary(fit.arima)
plot(forecast(fit.arima))
lines(swb.test, type='o')

## Time Series Regression
x=combined.ts[,1]
y=combined.ts[,3]
fit.lm <- lm(y~x)
summary(fit.lm)
plot.ts(fit.lm$residuals)
Acf(fit.lm$residuals)
dwtest(fit.lm)


### Grainger Causality

ndiffs(x, test="kpss")
ndiffs(y, test="kpss")

Granger <- cbind(y,x)
dGranger <- diff(Granger)

lag=VARselect(dGranger, lag.max=12)
lag$selection

lag12 <- VAR(dGranger, p=12)
summary(lag12)

serial.test(lag12, type="PT.asymptotic")
## Reject null:  Residuals are not correlated

x2y <- causality(lag12, cause="x")
y2x <- causality(lag12, cause="y")

x2y$Granger
y2x$Granger

predict(lag12, n.ahead=24, ci=0.95)

plot(forecast(lag12))


bv.cusum <- stability(lag12, type = "OLS-CUSUM")
plot(bv.cusum)


###################################################################################################
# require(tidyverse)
# require(tidymodels)
# require(data.table)
# require(tidyposterior)
# require(tsibble)  #tsibble for time series based on tidy principles
# require(fable)  #for forecasting based on tidy principles
# require(ggfortify)  #for plotting timeseries
# require(forecast)  #for forecast function
# require(tseries)
# require(chron)
# require(lubridate)
# require(directlabels)
# require(zoo)
# require(lmtest)
# require(TTR)  #for smoothing the time series
# require(MTS)
# require(vars)
# require(fUnitRoots)
# require(lattice)
# require(grid)

plot(combined.ts)

autoplot(combined.ts) +
  ggtitle("Time Series Plot of the `EuStockMarkets' Time-Series") +
  theme(plot.title = element_text(hjust = 0.5))

apply(combined.ts, 2, adf.test)

stnry = diffM(combined.ts) #difference operation on a vector of time series. Default order of differencing is 1.

plot.ts(stnry)

VARselect(stnry, 
          type = "none", #type of deterministic regressors to include. We use none becasue the time series was made stationary using differencing above. 
          lag.max = 12)

var.a <- vars::VAR(stnry,
                   lag.max = 10, #highest lag order for lag length selection according to the choosen ic
                   ic = "AIC", #information criterion
                   type = "none") #type of deterministic regressors to include
summary(var.a)

serial.test(var.a)

#selecting the variables
# Granger test for causality
#for causality function to give reliable results we need all the variables of the multivariate time series to be stationary. 
causality(var.a, #VAR model
          cause = c("housing.scaled.ts"))

fcast = predict(var.a, n.ahead = 25) # we forecast over a short horizon because beyond short horizon prediction becomes unreliable or uniform
par(mar = c(2.5,2.5,2.5,2.5))
plot(fcast)

swb.unscaled <- swb.scaled.ts * attr(swb.scaled.ts, 'scaled:scale') + attr(swb.scaled.ts, 'scaled:center')
plot(swb.unscaled)

swb.forecast <- fcast$fcst$swb.scaled.ts[,1]

swb.forecast.unscaled <- swb.forecast* attr(swb.scaled.ts, 'scaled:scale') + attr(swb.scaled.ts, 'scaled:center')

swb.combined.ts <- ts(c(swb.unscaled, swb.forecast.unscaled), start=c(2000,1), frequency=12)

autoplot(swb.combined.ts)

