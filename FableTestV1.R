library(tsm)
library(vars)
library(mFilter)
library(readxl)
library(dplyr)
library(readr)

#library(devtools)
#devtools::install_github("KevinKotze/tsm")
TestV1 <- read_csv("C:/Users/John/Desktop/USBPTS.csv")
t1 <- select(TestV1, xDate, "Southwest Border")

t1 <- rename(t1, date=xDate)
t1 <- rename(t1, value="Southwest Border")

t1TS <- ts(t1$value, start = c(2000, 1), frequency = 12)

USHS2000 <- window(USHS, start=c(2000, 1), end=c(2020, 1))

plot(cbind (t1TS, USHS2000))

library(fable)
library(tsibble)
library(tsibbledata)
library(lubridate)
library(dplyr)
library(tsibble)

t1TS %>% as_tsibble()

t1tb <- as_tsibble(t1TS)

t1tb  %>% 
  model(
    ets = ETS(box_cox(value, 0.3)),
    arima = ARIMA(log(value)),
    snaive = SNAIVE(value)
  ) %>%
  forecast(h = "2 years") %>%
  autoplot(filter(t1tb)) #, year(Month) > 2010), level = NULL)







