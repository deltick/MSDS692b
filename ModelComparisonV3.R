library(tidymodels)
library(modeltime)
library(tidyverse)
library(lubridate)
library(timetk)
library(astsa)
library(readxl)
library(dplyr)

m750 <- m4_monthly %>% filter(id == "M750")

TestV1 <- read_csv("C:/Users/JJT/Desktop/USBPTS.csv")
t1 <- select(TestV1, xDate, "Southwest Border")

t1 <- dplyr::rename(t1, date=xDate)
t1 <- dplyr::rename(t1, value="Southwest Border")

interactive <- FALSE

t1 %>%
  plot_time_series(date, value, .interactive = interactive)

m750 <-t1

splits <- initial_time_split(m750, prop = 0.9)

model_fit_arima_no_boost <- arima_reg() %>%
  set_engine(engine = "auto_arima") %>%
  fit(value ~ date, data = training(splits))

(model_fit_arima_no_boost)

model_fit_arima_boosted <- arima_boost(
  min_n = 2,
  learn_rate = 0.015
) %>%
  set_engine(engine = "auto_arima_xgboost") %>%
  fit(value ~ date + as.numeric(date) + factor(month(date, label = TRUE), ordered = F),
      data = training(splits))

model_fit_ets <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(value ~ date, data = training(splits))

model_fit_prophet <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(value ~ date, data = training(splits))

model_fit_lm <- linear_reg() %>%
  set_engine("lm") %>%
  fit(value ~ as.numeric(date) + factor(month(date, label = TRUE), ordered = FALSE),
      data = training(splits))


model_spec_mars <- mars(mode = "regression") %>%
  set_engine("earth") 
recipe_spec <- recipe(value ~ date, data = training(splits)) %>%
  step_date(date, features = "month", ordinal = FALSE) %>%
  step_mutate(date_num = as.numeric(date)) %>%
  step_normalize(date_num) %>%
  step_rm(date)

wflw_fit_mars <- workflow() %>%
  add_recipe(recipe_spec) %>%
  add_model(model_spec_mars) %>%
  fit(training(splits))


models_tbl <- modeltime_table(
  model_fit_arima_no_boost,
  model_fit_arima_boosted,
  model_fit_ets,
  model_fit_prophet,
  model_fit_lm,
  wflw_fit_mars
)
models_tbl

calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(splits))
calibration_tbl


calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = m750
  ) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
    .interactive      = interactive
  )


calibration_tbl %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = interactive
  )


refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = m750)
refit_tbl %>%
  modeltime_forecast(h = "3 years", actual_data = m750) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
    .interactive      = interactive
  )


########################################################################################


# fcast <- refit_tbl %>%
#   modeltime_forecast(h = "3 years", actual_data = m750) # %>%
#   plot_modeltime_forecast(
#     .legend_max_width = 25, # For mobile screens
#     .interactive      = interactive
#   )
# 
# dt <-fcast$data$.index
# y <- fcast$data$.value
# 
# fcastTS <- ts(y, start=c(2000, 1), frequency = 12)
# 
# 
# plot(fcastTS)
# 
# str(fcastTS)
# 
# fx <- select(fcast, .model_desc, .key, .index, .value)
# 
# SumFYMonthV3 <- fx %>% 
#   group_by(.model_desc, .index) %>% 
#   summarise(Count = sum(.value, na.rm=TRUE))
# 
