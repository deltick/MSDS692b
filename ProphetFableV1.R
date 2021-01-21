library(fable.prophet)

inputTsb %>% autoplot(value)



fit <- inputTsb %>% model(
  prophet = prophet(value ~ xDate)
)

fit

components(fit)

components(fit)  %>% autoplot()

fc <- fit %>%
  forecast(h = "3 years")

fc %>% autoplot(inputTsb)

fc %>% accuracy(inputTsb)



fit2 <- inputTsb %>% model(
  prophet = prophet(value ~ growth("linear") + season("year", type="additive"))
)

components(fit2) %>% autoplot()

fc2 <- fit2 %>%
  forecast(h = "3 years")

fc2 %>% autoplot(inputTsb)

fc2 %>% accuracy(inputTsb)


###############################################





library(lubridate)


recent_production <- inputTsb %>%
  filter(year(xDate) >= 2010)
recent_production %>%
  autoplot(value) 


fit3 <- recent_production %>%
  model(TSLM(value ~ trend() + season()))
report(fit3)

