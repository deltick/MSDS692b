### Neural Net from Bookdown book

require(tidyverse)
require(tidymodels)
require(data.table)
require(tidyposterior)
require(tsibble)  #tsibble for time series based on tidy principles
require(fable)  #for forecasting based on tidy principles
require(ggfortify)  #for plotting timeseries
require(forecast)  #for forecast function
require(tseries)
#require(chron)
require(lubridate)
#require(directlabels)
require(zoo)
require(lmtest)
require(TTR)  #for smoothing the time series
require(MTS)
require(vars)
require(fUnitRoots)
require(lattice)
require(grid)
library(sweep)   # Broom tidiers for forecast pkg
library(broom)
library (readr)
library(timetk)
library(tsibbledata)
library(fabletools)

library(dplyr)
library(reshape)


urlfile="https://raw.githubusercontent.com/deltick/MSDS692b/main/Data/USBPTS.csv"

inputTS<-read_csv(url(urlfile))

inputSel2 <- inputTS %>% select(xDate, "Southwest Border")
inputSel2 <- dplyr::rename(inputSel2, SWB = "Southwest Border")
inputMelt2 <- melt(as.data.frame(inputSel2), id="xDate")

inputMelt2 <- inputMelt2 %>%
  mutate(Month = yearmonth(xDate)) %>%
  as_tsibble(index = Month)

inputMelt2 <- select(inputMelt2, -xDate)

inputTsb2 <- as_tsibble(inputMelt2, index=Month, key=variable)
inputTsb2 %>% autoplot()


swb <- inputTsb2
x.ts <- ts(swb$value, start=c(2000, 01), frequency=12)


fit = nnetar(x.ts)
nnetforecast <- forecast(fit, h = 24, PI = T)  #Prediction intervals do not come by default in neural net forecasts, in constrast to ARIMA or exponential smoothing model
autoplot(nnetforecast) + theme(plot.title = element_text(hjust = 0.5))

sweep::sw_glance(fit) 

sweep::sw_augment(fit )%>%head()

forecast::forecast(fit,h=12,level = c(90, 95))%>%tk_tbl()

