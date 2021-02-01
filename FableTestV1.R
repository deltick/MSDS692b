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

t1 <- dplyr::rename(t1, date=xDate)
t1 <- dplyr::rename(t1, value="Southwest Border")

t1TS <- ts(t1$value, start = c(2000, 1), frequency = 12)

USHS2000 <- window(USHS, start=c(2000, 1), end=c(2020, 1))

plot(cbind (t1TS, USHS2000))

library(fable)
library(tsibble)
library(tsibbledata)
library(lubridate)
library(dplyr)
library(tsibble)
library(fabletools)

t1TS %>% as_tsibble()

t1tb <- as_tsibble(t1TS)

fit<- t1tb  %>% 
  model(
    ets = ETS(box_cox(value, 0.3)),
    arima = ARIMA(log(value)),
    snaive = SNAIVE(value) #,
    #lm1 = TSLM((value) ~ trend() + season()),
    #nn = NNETAR(box_cox(value, 0.15))
  ) %>%
  forecast(h = "2 years") %>%
  autoplot((t1tb)) #, year(Month) > 2010), level = NULL)


## https://fable.tidyverts.org/articles/fable.html

fit2<- t1tb  %>% 
  model(
    ets = ETS(box_cox(value, 0.3)),
    arima = ARIMA(log(value)),
    snaive = SNAIVE(value)
  )

fit2 %>%
  #filter(Region == "Snowy Mountains", Purpose == "Holiday") %>%
  select(arima) %>%
  report()

fit2

fit2 %>%
  augment()
fit2 %>%
  accuracy() %>%
  arrange(MASE)

fit2 %>%
  glance()

forecast1 <- fit2 %>%
  forecast(h = "5 years")

forecast1%>%
  hilo(level = c(80, 95))

forecast1 %>%
  autoplot(t1tb)


##https://robjhyndman.com/hyndsight/fable/

train <- t1tb %>%
  filter(year(index) <= 2015)
fit <- train %>%
  model(
    ets = ETS(value),
    arima = ARIMA(value),
    snaive = SNAIVE(value)
  ) %>%
  mutate(mixed = (ets + arima + snaive) / 3)

fc <- fit %>% forecast(h = "4 years")

fc %>%
  autoplot(t1tb, level = NULL)

accuracy(fc, t1tb)


fc_accuracy <- accuracy(fc, t1tb,
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
    CRPS = mean(CRPS)
  ) %>%
  arrange(RMSE)


#### fable lm model)


fitlm <- t1tb  %>%
  model(lm = TSLM(log(value) ~ trend() + season()))

fclm <- fitlm %>% forecast(h = "4 years")

fclm %>%
  autoplot(t1tb)


#### fable nnet model

fitnn <- t1tb  %>%
  model(nn = NNETAR(box_cox(value, 0.15)))

fcnn <- fitnn %>% forecast(h = "4 years")

fcnn %>%
  autoplot(t1tb)

accuracy(fcnn, t1tb,  ##need to fix this
         measures = list(
           point_accuracy_measures,
           interval_accuracy_measures,
           distribution_accuracy_measures
         ))


