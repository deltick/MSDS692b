library (readr)
library(tsibble)
library(tsibbledata)
library(fabletools)
library(fable)
library(dplyr)
library(reshape)
library(feasts)
library(timetk)
library(tidyverse)
library(ggplot2)
library(anomalize)
library(aTSA)
library(tsbox)
library(tsoutliers)
library(tidyverse)
library(tidyquant)
library(anomalize)
library(tseries)
library(forecast)



urlfile="https://raw.githubusercontent.com/deltick/MSDS692b/main/Data/USBPTS.csv"

inputTS<-read_csv(url(urlfile))

# inputSel <- inputTS %>% select(xDate, 'San Diego', 'El Paso', 'Laredo', 'Rio Grande Valley', 'Del Rio', 'El Centro', 'Tucson', 'Yuma')
# 
# inputSel <- dplyr::rename(inputSel, SDC = 'San Diego')
# inputSel <- dplyr::rename(inputSel, ELP = 'El Paso')
# inputSel <- dplyr::rename(inputSel, LRT = 'Laredo')
# inputSel <- dplyr::rename(inputSel, RGV = 'Rio Grande Valley')
# inputSel <- dplyr::rename(inputSel, DRT = 'Del Rio')
# inputSel <- dplyr::rename(inputSel, ELC = 'El Centro')
# inputSel <- dplyr::rename(inputSel, TUS = 'Tucson')
# inputSel <- dplyr::rename(inputSel, YUM = 'Yuma')
# 
# ##inputSel <- select(inputSel, -xDate)
# 
# inputMelt <- melt(as.data.frame(inputSel), id="xDate") #, variable_name = "count")
# 
# #inputTs2 <- ts(inputSel, start=c(2000, 1), frequency=12)
# 
# inputTsb <- as_tsibble(inputMelt, index=xDate, key=variable)
# 
# #inputTsb %>% autoplot(vars(SD, RGV, LRT, ELP))
# 
# inputTsb %>% autoplot()


inputSel2 <- inputTS %>% select(xDate, "Southwest Border")
inputSel2 <- dplyr::rename(inputSel2, SWB = "Southwest Border")
inputMelt2 <- melt(as.data.frame(inputSel2), id="xDate")

inputMelt2 <- inputMelt2 %>%
  mutate(Month = yearmonth(xDate)) %>%
  as_tsibble(index = Month)

inputMelt2 <- select(inputMelt2, -xDate)

inputTsb2 <- as_tsibble(inputMelt2, index=Month, key=variable)
inputTsb2 %>% autoplot()

### EDA
head(inputSel2)
tail(inputSel2)

summary(inputSel2)
hist(inputSel2$SWB)
boxplot(inputSel2$SWB)
#boxplot.stats(inputSel2$SWB)$out

### Decompose TS

#swb <- inputTsb2
#x.ts <- ts(swb$value, start=c(2000, 01), frequency=12)

dcmp <- swb %>%
  model(STL(value))
components(dcmp)

autoplot(swb, value, color="gray") +
  autolayer(components(dcmp), trend, color="red")

components(dcmp) %>% 
  autoplot()


### timetk anomaly detection

swbTib <- as_tibble(inputSel2)

swbTib %>%
  plot_anomaly_diagnostics(xDate, SWB, .facet_ncol = 2)

swbTib %>%
  tk_anomaly_diagnostics(xDate, SWB )

## anomaly detection with tsoutliers

outliers_excess_ts <- tso(x.ts, types = c("TC", "AO", "LS", "IO", "SLS"))
#ou<-outliers_excess_ts <- tso(x.ts, types = c("TC", "AO", "LS", "IO", "SLS"))
outliers_excess_ts
plot(outliers_excess_ts)


### EDA Tests using aTSA for ADF and tsbox to convert

x.ts <- ts(swb$value, start=c(2000, 01), frequency=12)
adf.test(x.ts)

## Stationarity Tests
tseries::adf.test(x.ts, k = 0)
tseries::kpss.test(x.ts, null = "Trend")
tseries::kpss.test(x.ts, null = "Level")
#par(mfrow = c(1,2))
acf(ts(diff((x.ts))),main='ACF Housing Prices South')
pacf(ts(diff((x.ts))),main='PACF Housing Prices South')

## Differencing  from predictive analytics hw 6
ndiffs(x.ts)
xDiff.ts <- (diff(x.ts))
plot((xDiff.ts),ylab='2  x Differenced Housing Prices')
adf.test(xDiff.ts)
kpss.test(xDiff.ts)

AR1 = auto.arima(x.ts, approximation=FALSE,trace=FALSE)
summary(AR1)

res <- resid(AR1)
Box.test(res, type=  "Box-Pierce" , lag = 4, fitdf = 2)
#Box.test(res, type = "Ljung-Box", lag = 4, fitdf = 2)
forecast::checkresiduals(AR1)


x1 <- (AR1$x)
x2 <- (AR1$fitted)
library(ggplot2)
x1df <- data.frame(Y=as.matrix(x1), date=time(x1))
x2df <- data.frame(Y=as.matrix(x2), date=time(x2))
p = ggplot() + 
  geom_line(data = x1df, aes(x = date, y = Y), color = "blue") +
  geom_line(data = x2df, aes(x = date, y = Y), color = "red") +
  xlab('Date') +
  ylab('Forecast vs Actual')
print(p)


par(mfrow=c(1,2))
acf(ts(AR1$residuals),main='ACF Residual')
pacf(ts(AR1$residuals),main='PACF Residual')

plot(forecast(AR1,24))

fit <- stl(x.ts, t.window=4, s.window="periodic", robust=TRUE)
fit %>% seasadj() %>% naive() %>%
  autoplot() + ylab("Housing Prices") +
  ggtitle("STL Forecast")

fit <- stl(xDiff.ts, t.window=4, s.window="periodic", robust=TRUE)
fit %>% seasadj() %>% naive() %>%
  autoplot() + ylab("Housing Prices") +
  ggtitle("STL Forecast")


## anomaly detection with tsoutliers

outliers_excess_ts <- tso(x.ts, types = c("TC", "AO", "LS", "IO", "SLS"))
#ou<-outliers_excess_ts <- tso(x.ts, types = c("TC", "AO", "LS", "IO", "SLS"))
outliers_excess_ts
plot(outliers_excess_ts)

# time index where the outliers have been detected
(outliers_idx <- outliers_excess_ts$outliers$ind)

