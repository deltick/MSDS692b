# John J. Tamer
# MSDS692 Practicum 1 Final Project

# LIBRARIES ----
#library(gtrendsR)
library(tidymodels)
library(modeltime)
library(tidyverse)
library(timetk)
library(lubridate)
library(dplyr)
library(readr)
library(fable)
library(tsibble)
#library(tsibbledata)
library(yardstick)
library(reshape2)
library(knitr)

# 1.0 LOAD DATA FROM REPO ----

urlfile="https://raw.githubusercontent.com/deltick/MSDS692b/main/Data/USBPTS.csv"
TestV1<-read_csv(url(urlfile))
t1 <- dplyr::select(TestV1, xDate, "Southwest Border")
t1 <- dplyr::rename(t1, date=xDate)
t1 <- dplyr::rename(t1, value="Southwest Border")
data_prepared_tbl <- t1

# 2.0 MODELING ----

# * Train/Test ----
splits <- time_series_split(data_prepared_tbl, assess = "1 year", cumulative = TRUE)

splits %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(date, value) 

# * ARIMA ----
model_fit_arima <- arima_reg() %>%
    set_engine("auto_arima") %>%
    fit(
        value ~ date, 
        data = training(splits)
    )

# * LINEAR REGRESSION ----
model_fit_lm <- linear_reg() %>%
    set_engine("lm") %>%
    fit(
        value ~ as.numeric(date) + month(date, label = TRUE), 
        data = training(splits)
    )

# * LINEAR REGRESSION - NO TREND ----
model_fit_lm_no_trend <- linear_reg() %>%
    set_engine("lm") %>%
    fit(
        value ~ month(date, label = TRUE), 
        data = training(splits)
    )

# * PROPHET ----
model_fit_prophet <- prophet_reg() %>%
    set_engine("prophet") %>%
    fit(
        value ~ date, 
        data = training(splits)
    )

# * RANDOM FOREST ----
model_fit_rf <- rand_forest(mode = "regression") %>%
    set_engine("randomForest") %>%
    fit(
        value ~ as.numeric(date) + month(date, label = TRUE), 
        data = training(splits)
    )

# * XGBOOST ----
model_fit_xgboost <- boost_tree(mode = "regression") %>%
    set_engine("xgboost") %>%
    fit(
        value ~ as.numeric(date) + month(date, label = TRUE), 
        data = training(splits)
    )

# * SVM - Polynomial ----
model_fit_svm_poly <- svm_poly(mode = "regression") %>%
    set_engine("kernlab") %>%
    fit(
        value ~ as.numeric(date) + month(date, label = TRUE), 
        data = training(splits)
    )

# * SVM - RBF ----
model_fit_svm_rbf <- svm_rbf(mode = "regression") %>%
    set_engine("kernlab") %>%
    fit(
        value ~ as.numeric(date) + month(date, label = TRUE), 
        data = training(splits)
    )

# * PROPHET BOOST ----
model_fit_prophet_boost <- prophet_boost() %>%
    set_engine("prophet_xgboost") %>%
    fit(
        value ~ date + as.numeric(date) + month(date, label = TRUE), 
        data = training(splits)
    )

# * ARIMA BOOST ----
model_fit_arima_boost <- arima_boost() %>%
    set_engine("auto_arima_xgboost") %>%
    fit(
        value ~ date + as.numeric(date) + month(date, label = TRUE), 
        data = training(splits)
    )

model_fit_nn <- nnetar_reg() %>%
    set_engine(engine = "nnetar") %>%
    fit((value) ~ date, data = training(splits))

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

model_fit_ets <- exp_smoothing(
        seasonal_period  = 12,
        error            = "multiplicative",
        trend            = "none",
        season           = "multiplicative"
    ) %>%
    set_engine("ets") %>%
    fit((value) ~ date, data = training(splits))


# 3.0 MODELTIME FORECAST WORKFLOW ----

# * Modeltime Table ----
model_tbl <- modeltime_table(
    model_fit_arima,
    model_fit_lm,
    model_fit_lm_no_trend,
    model_fit_prophet,
    model_fit_rf,
    model_fit_xgboost,
    model_fit_svm_poly,
    model_fit_svm_rbf,
    model_fit_prophet_boost,
    model_fit_arima_boost,
    model_fit_nn,
    wflw_fit_mars,
    model_fit_ets
)

# * Calibrate ----
calibration_tbl <- model_tbl %>%
    modeltime_calibrate(testing(splits))

calibration_tbl %>%
    modeltime_accuracy() %>%
    table_modeltime_accuracy(resizable = TRUE, bordered = TRUE)

calibration_tbl %>%
    modeltime_forecast(
        new_data = testing(splits), 
        actual_data = data_prepared_tbl,
        conf_interval = 0.80
    ) %>%
    plot_modeltime_forecast(.legend_show = TRUE, 
                            .legend_max_width = 25)

# * Refit ----
refit_tbl <- calibration_tbl %>%
    modeltime_refit(data = data_prepared_tbl) 

forecast_tbl <- refit_tbl %>%
    modeltime_forecast(
        h = "1 year",
        actual_data = data_prepared_tbl,
        conf_interval = 0.80
    ) 

forecast_tbl %>%
    plot_modeltime_forecast(.interactive = TRUE)

# 4.0 MODEL AVERAGING ----

# * Mean Forecast ----
mean_forecast_tbl <- forecast_tbl %>%
    filter(.key != "actual") %>%
    group_by(.key, .index) %>%
    summarise(across(.value:.conf_hi, mean)) %>%
    mutate(
        .model_id   = 10,
        .model_desc = "AVERAGE OF MODELS"
    )

# * Visualize Mean Forecast ----
forecast_tbl %>%
    filter(.key == "actual") %>%
    bind_rows(mean_forecast_tbl) %>%
    plot_modeltime_forecast()

## Compare Forecasts to Actual 2020 results

allPred <- forecast_tbl %>%
    dplyr::filter(.key == 'prediction') %>%
    dplyr::select(.model_desc, .index, .value)
    #dplyr::select(.model_id, .model_desc,.index, .value) 


Y20Actuals <- read_csv("C:/Users/JJT/Desktop/Y20Actuals.csv")
allValues <- rbind(allPred, Y20Actuals)
allValues.cast <- dcast(allValues, .index ~ .model_desc, mean)
#allValues.cast <- dcast(allValues, .index ~ .model_id, mean)
#allValues.cast.tsb <- as_tsibble(allValues.cast)



actual.ts <- ts(allValues.cast$ACTUAL, start=c(2020, 1), frequency=12)
earth.ts <- ts(allValues.cast$EARTH, start=c(2020, 1), frequency=12)
etsmnm.ts <- ts(allValues.cast$`ETS(M,N,M)`, start=c(2020, 1), frequency=12)
kernlab.ts <- ts(allValues.cast$KERNLAB, start=c(2020, 1), frequency=12)
lm.ts <- ts(allValues.cast$LM, start=c(2020, 1), frequency=12)
nnetar.ts <- ts(allValues.cast$`NNAR(1,1,10)[12]`, start=c(2020, 1), frequency=12)
prophet.ts <- ts(allValues.cast$PROPHET, start=c(2020, 1), frequency=12)
prophetxg.ts <- ts(allValues.cast$`PROPHET W/ XGBOOST ERRORS`, start=c(2020, 1), frequency=12)
randomforest.ts <- ts(allValues.cast$RANDOMFOREST, start=c(2020, 1), frequency=12)
arima1.ts <- ts(allValues.cast$`UPDATE: ARIMA(3,1,2)(2,1,1)[12] W/ XGBOOST ERRORS`, start=c(2020, 1), frequency=12)
arima2.ts <- ts(allValues.cast$`UPDATE: ARIMA(4,1,0)(2,1,1)[12]`, start=c(2020, 1), frequency=12)
xgboost.ts <- ts(allValues.cast$XGBOOST, start=c(2020, 1), frequency=12)

all2020Preds <- cbind(actual.ts, earth.ts, etsmnm.ts, kernlab.ts, lm.ts, nnetar.ts, prophet.ts, prophetxg.ts, randomforest.ts, arima1.ts, arima2.ts, xgboost.ts)

autoplot(all2020Preds) +
    ggtitle('Plot of Predicions vs 2020 Actuals')


combined_metrics_set <- metric_set(mape, rmse, mase, mae)

get_combined_metrics <- function(est) {
    allMetrics <- combined_metrics_set(allValues.cast, ACTUAL, est )
    return(allMetrics)
}    
    
metrics_output <-  NULL
for (i in seq_along((allValues.cast))) {
    if (i > 1)
        metrics_output[[i]] <- get_combined_metrics(allValues.cast[[i]] )
}

metrics_output <- bind_rows(metrics_output)
metrics_output <- metrics_output %>% select(-.estimator)
all_mape <- filter(metrics_output, .metric=='mape')
all_rmse <- filter(metrics_output, .metric=='rmse')
all_mase <- filter(metrics_output, .metric=='mase')
all_mae  <- filter(metrics_output, .metric=='mae')


all_metrics <- cbind(all_mape, all_rmse, all_mase, all_mae)
names(all_metrics)[1] <- "mapen"
names(all_metrics)[2] <- "mape"
names(all_metrics)[3] <- "rmsen"
names(all_metrics)[4] <- "rmse"
names(all_metrics)[5] <- "masen"
names(all_metrics)[6] <- "mase"
names(all_metrics)[7] <- "maen"
names(all_metrics)[8] <- "mae"

all_metrics.tib <- all_metrics %>% 
    select(mape, rmse, mase, mae)

row.names(all_metrics.tib) <- c("Actual", "Earth", "ETS" , "Kernlab", "LM",
                                "NNetAR", "Prophet", "ProphetXG", "RandomForest", "Arima1",
                                "Arima2", "XGBoost")


