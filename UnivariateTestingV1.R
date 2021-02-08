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

swb.ts <- ts(swb$value, start=c(2000, 01), frequency=12)
#swbWindow.ts <- window(x.ts, start=c(2008, 1), end=c(2019, 4))

swbWindow.ts <- swb.ts 

autoplot(swbWindow.ts)


swbWindow.ts %>% as_tsibble()
swbWindow.tsb <- as_tsibble(swbWindow.ts)

train <- swbWindow.tsb %>%
  filter(year(index) <= 2017)
fit <- train %>%
  model(
    ets = ETS(value),
    arima = ARIMA(value),
    snaive = SNAIVE(value),
    lm1 = TSLM((value) ~ trend() + season()),
    #nn = NNETAR(box_cox(value, 0.15)),
    rw = RW(value)
  ) #%>%
#mutate(mixed = (ets + arima + snaive) / 3)

fit %>%
  select(arima) %>%
  report()


fit %>%
  select(ets) %>%
  report()

fit %>%
  select(snaive) %>%
  report()

fit %>%
  select(lm1) %>%
  report()

#fit %>%
#  select(nn) %>%
#  report()


fit %>%
  select(rw) %>%
  report()

#library(feasts)
fit %>%
  select(arima) %>%
  gg_tsresiduals()

fit %>%
  select(arima) %>%
  glance

fit %>%
  select(ets) %>%
  gg_tsresiduals()

fc <- fit %>% forecast(h = "2 years")

fc %>%
  autoplot(swbWindow.tsb, level = NULL) 
  ggtitle("Southwest Border Encounter Forecasts") +
  xlab("Year") +
  ylab("Encounters") +
  guides(colour = guide_legend(title = "Forecast"))


#accuracy(fc, swbWindow.tsb)


fc_accuracy <- accuracy(fc, swbWindow.tsb,
                        measures = list(
                          point_accuracy_measures,
                          interval_accuracy_measures,
                          distribution_accuracy_measures
                        )
)

fc_accuracy %>%
  group_by(.model) %>%
  summarise(
    RMSE = mean(RMSE),
    MAE = mean(MAE),
    MASE = mean(MASE),
    Winkler = mean(winkler),
    CRPS = mean(CRPS),
    MAPE = mean(MAPE)
  ) %>%
  arrange(RMSE)

knitr::kable(fc_accuracy)
