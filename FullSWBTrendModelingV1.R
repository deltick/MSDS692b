# LEARNING LAB 38: INTRO TO MODELTIME ----
# FORECASTING WITH GOOGLE TRENDS WITH MANY MODELS

# LIBRARIES ----
library(gtrendsR)
library(tidymodels)
library(modeltime)
library(tidyverse)
library(timetk)
library(lubridate)
library(dplyr)
library(readr)
library(fable)
library(tsibble)
library(tsibbledata)



TestV1 <- read_csv("C:/Users/JJT/Desktop/USBPTS.csv")
t1 <- select(TestV1, xDate, "Southwest Border")
t1 <- dplyr::rename(t1, date=xDate)
t1 <- dplyr::rename(t1, value="Southwest Border")
#t1TS <- ts(t1$value, start = c(2000, 1), frequency = 12)
#t1TS %>% as_tsibble()
#t1tb <- as_tsibble(t1TS)


# 1.0 GOOGLE TRENDS DATA ----

#search_term <- "data science"
#location    <- "US"
#time        <- "all"
# search_term <- "crossing"  #migrate"  #c("border", "crossing", "illegal")
# location    <- "GT" #  MX"  #c("MX","HN", "GT", "SV")
# time        <- "all"
# 
# # Google Trends
# gtrends_list <- gtrendsR::gtrends(
#     keyword = search_term, 
#     geo     = location, 
#     time    = time
# )

# Save for reproducibility
# gtrends_list %>% write_rds("data/gtrends_list.rds")
#gtrends_list <- read_rds("data/gtrends_list.rds")

# Visualization
# gtrends_list %>%
#     pluck("interest_over_time") %>%
#     plot_time_series(date, hits)
# 
# # Data Cleaning
# gtrends_interest_over_time_tbl <- gtrends_list %>%
#     pluck("interest_over_time") %>%
#     as_tibble() %>%
#     select(date, hits) %>%
#     mutate(hits = ifelse(hits == 0, NA, hits)) %>%
#     mutate(hits = ts_impute_vec(hits, period = 12))
# 
# gtrends_interest_over_time_tbl %>%
#     plot_time_series(date, hits)
# 
# data_prepared_tbl <- gtrends_interest_over_time_tbl %>%
#     rename(value = hits)

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
    model_fit_arima_boost
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

# BONUS - SHINY APP ----
# - Get your hands on this app
# - Accelerate your career

