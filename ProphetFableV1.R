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


fit2 <- inputTsb %>% model(
  prophet = prophet(value ~ growth("linear") + season("year", type="additive"))
)

components(fit2) %>% autoplot()

fc2 <- fit2 %>%
  forecast(h = "3 years")

fc2 %>% autoplot(inputTsb)

