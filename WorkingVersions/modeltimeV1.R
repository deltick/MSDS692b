# Time Series ML
library(tidymodels)
library(modeltime)
library(modeltime.ensemble)

# Core
library(tidyverse)
library(timetk)

interactive <- FALSE

TestV1 <- read_csv("C:/Users/JJT/Desktop/USBPTS.csv")
t1 <- select(TestV1, xDate, "Southwest Border")

t1 <- dplyr::rename(t1, date=xDate)
t1 <- dplyr::rename(t1, value="Southwest Border")

interactive <- FALSE

t1 %>%
  plot_time_series(date, value, .interactive = interactive)

m750 <-t1
t1 %>%
  plot_seasonal_diagnostics(date, value, .interactive = FALSE)

splits <- time_series_split(m750, assess = "2 years", cumulative = TRUE)

splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, value, .interactive = interactive)

recipe_spec <- recipe(value ~ date, training(splits)) %>%
  step_timeseries_signature(date) %>%
  step_rm(matches("(.iso$)|(.xts$)")) %>%
  step_normalize(matches("(index.num$)|(_year$)")) %>%
  step_dummy(all_nominal()) %>%
  step_fourier(date, K = 1, period = 12)

recipe_spec %>% prep() %>% juice()

model_spec_arima <- arima_reg() %>%
  set_engine("auto_arima")

wflw_fit_arima <- workflow() %>%
  add_model(model_spec_arima) %>%
  add_recipe(recipe_spec %>% step_rm(all_predictors(), -date)) %>%
  fit(training(splits))

model_spec_prophet <- prophet_reg() %>%
  set_engine("prophet")

wflw_fit_prophet <- workflow() %>%
  add_model(model_spec_prophet) %>%
  add_recipe(recipe_spec %>% step_rm(all_predictors(), -date)) %>%
  fit(training(splits))


model_spec_glmnet <- linear_reg(
  mixture = 0.9,
  penalty = 4.36e-6
) %>%
  set_engine("glmnet")

wflw_fit_glmnet <- workflow() %>%
  add_model(model_spec_glmnet) %>%
  add_recipe(recipe_spec %>% step_rm(date)) %>%
  fit(training(splits))

m750_models <- modeltime_table(
  wflw_fit_arima,
  wflw_fit_prophet,
  wflw_fit_glmnet
)

m750_models

ensemble_fit <- m750_models %>%
  ensemble_average(type = "mean")

ensemble_fit

# Calibration
calibration_tbl <- modeltime_table(
  ensemble_fit
) %>%
  modeltime_calibrate(testing(m750_splits))

# Forecast vs Test Set
calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(m750_splits),
    actual_data = m750
  ) %>%
  plot_modeltime_forecast(.interactive = interactive)


modeltime_accuracy(calibration_tbl)

refit_tbl <- calibration_tbl %>%
  modeltime_refit(m750)

refit_tbl %>%
  modeltime_forecast(
    h = "2 years",
    actual_data = m750
  ) %>%
  plot_modeltime_forecast(.interactive = interactive)

