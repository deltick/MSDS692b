# Chapter 12 Code

# -------- Code Chank 1 --------
library(TSstudio)
library(tidyverse)
library (readr)

urlfile="https://raw.githubusercontent.com/deltick/MSDS692b/main/Data/USBPTS.csv"

inputTS<-read_csv(url(urlfile))


inputSel2 <- inputTS %>% select(xDate, "Southwest Border")
inputSel2 <- dplyr::rename(inputSel2, value = "Southwest Border")
#inputMelt2 <- melt(as.data.frame(inputSel2), id="xDate")

#inputMelt2 <- inputMelt2 %>%
#  mutate(Month = yearmonth(xDate)) %>%
#  as_tsibble(index = Month)

#inputMelt2 <- select(inputMelt2, -xDate)

#inputTsb2 <- as_tsibble(inputMelt2, index=Month, key=variable)
#inputTsb2 %>% autoplot()

swbts <- ts(inputSel2$value, start=c(2000, 01), frequency=12)
#data(USVSales)

#USVSales <- x.tx

# -------- Code Chank 2 --------
ts_info(swbts)
# -------- Code Chank 3 --------
ts_plot(swbts,
        title = "US Total Monthly Vehicle Sales",
        Ytitle = "Thousands of Units",
        Xtitle = "Year")
# -------- Code Chank 4 --------
ts_decompose(swbts)
# -------- Code Chank 5 --------
swb_detrend <- swbts - decompose(swbts)$trend

ts_seasonal(swb_detrend, type = "box")
# -------- Code Chank 6 --------
ts_cor(swbts)
# -------- Code Chank 7 --------
ts_lags(swbts, lags = c(12, 24, 36))
# -------- Code Chank 8 --------
df <- ts_to_prophet(window(swbts, start = c(2000,1))) #%>% 

names(df) <- c("date", "y")

head(df)
# -------- Code Chank 9 --------
ts_plot(df,
        title = "US Total Monthly Vehicle Sales (Subset)",
        Ytitle = "Thousands of Units",
        Xtitle = "Year")
# -------- Code Chank 10 --------
library(dplyr) 
library(lubridate)

df <-
  df %>% mutate(month = factor(lubridate::month(date, label = TRUE), ordered = FALSE),
                lag12 = lag(y, n = 12))
df %>%
  filter(!is.na(lag12))


# -------- Code Chank 11 --------
df$trend <- 1:nrow(df)
df$trend_sqr <- df$trend ^ 2
# -------- Code Chank 12 --------
str(df)
# -------- Code Chank 13 --------
h <- 12
train_df <- df[1:(nrow(df) - h), ]
test_df <- df[(nrow(df) - h + 1):nrow(df), ]
# -------- Code Chank 14 --------
forecast_df <- data.frame(date = seq.Date(from = max(df$date) + lubridate::month(1), length.out = h, by = "month"),
                          trend = seq(from = max(df$trend) + 1, length.out = h, by = 1))
forecast_df$trend_sqr <- forecast_df$trend ^ 2
forecast_df$month <- factor(lubridate::month(forecast_df$date, label = TRUE), ordered = FALSE)
forecast_df$lag12 <- tail(df$y, 12)

# -------- Code Chank 15 --------
lr <- lm(y ~ month + lag12 + trend + trend_sqr, data = train_df)
# -------- Code Chank 16 --------
summary(lr)
# -------- Code Chank 17 --------
test_df$yhat <- predict(lr, newdata = test_df)

mape_lr <- mean(abs(test_df$y - test_df$yhat) / test_df$y)
mape_lr
# -------- Code Chank 18 --------
library(h2o)

h2o.init(max_mem_size = "16G")

# -------- Code Chank 19--------
train_h <- as.h2o(train_df)
test_h <- as.h2o(test_df)
forecast_h <- as.h2o(forecast_df)

# -------- Code Chank 20 --------
x <- c("month", "lag12", "trend", "trend_sqr")
y <- "y"
# -------- Code Chank 21 --------
rf_md <- h2o.randomForest(
  training_frame = train_h,
  nfolds = 5,
  x = x,
  y = y,
  ntrees = 500,
  stopping_rounds = 10,
  stopping_metric = "RMSE",
  score_each_iteration = TRUE,
  stopping_tolerance = 0.0001,
  seed = 1234
)
# -------- Code Chank 22 --------
h2o.varimp_plot(rf_md)
# -------- Code Chank 23 --------
rf_md@model$model_summary
# -------- Code Chank 24 --------
library(plotly)


tree_score <- rf_md@model$scoring_history$training_rmse
plot_ly(
  x = seq_along(tree_score),
  y = tree_score,
  type = "scatter",
  mode = "line"
) %>%
  layout(
    title = "Random Forest Model - Trained Score History",
    yaxis = list(title = "RMSE"),
    xaxis = list(title = "Num. of Trees")
  )
# -------- Code Chank 25 --------
test_h$pred_rf <- h2o.predict(rf_md, test_h)
# -------- Code Chank 26--------
test_1 <- as.data.frame(test_h)
# -------- Code Chank 27 --------
mape_rf <- mean(abs(test_1$y - test_1$pred_rf) / test_1$y)
mape_rf
# -------- Code Chank 28 --------
search_criteria_rf <- list(
  strategy = "RandomDiscrete",
  stopping_metric = "rmse",
  stopping_tolerance = 0.0001,
  stopping_rounds = 10,
  max_runtime_secs = 60 * 20
)

hyper_params_rf <- list(
  mtries = c(2, 3, 4),
  sample_rate = c(0.632, 0.8, 0.95),
  col_sample_rate_per_tree = c(0.5, 0.9, 1.0),
  max_depth = c(seq(1, 30, 3)),
  min_rows = c(1, 2, 5, 10)
)

search_criteria_rf <- list(
  strategy = "RandomDiscrete",
  stopping_metric = "rmse",
  stopping_tolerance = 0.0001,
  stopping_rounds = 10,
  max_runtime_secs = 60 * 20
)

rf2 <- h2o.grid(algorithm = "randomForest",
                search_criteria = search_criteria_rf,
                hyper_params = hyper_params_rf,
                x = x,
                y = y,
                training_frame = train_h,
                ntrees = 5000,
                nfolds = 5,
                grid_id = "rf_grid2",
                seed = 1234)

# -------- Code Chank 29 --------
rf2_grid_search <- h2o.getGrid(grid_id = "rf_grid2",
                               sort_by = "rmse",
                               decreasing = FALSE)

rf_grid_model <- h2o.getModel(rf2_grid_search@model_ids[[1]])


test_h$rf_grid  <- h2o.predict(rf_grid_model, test_h)
mape_rf2 <- mean(abs(test_1$y - test_1$rf_grid) / test_1$y)
mape_rf2
# -------- Code Chank 30 --------
plot_ly(data = test_1) %>%
  add_lines(x = ~ date, y = ~y, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ yhat, name = "Linear Regression", line = list(dash = "dot")) %>%
  add_lines(x = ~ date, y = ~ pred_rf, name = "Random Forest", line = list(dash = "dash")) %>%
  #add_lines(x = ~ date, y = ~ rf_grid2, name = "Random Forest (grid)", line = list(dash = "dash")) %>%
  layout(title = "Total Vehicle Sales - Actual vs. Prediction (Random Forest)",
         yaxis = list(title = "Thousands of Units"),
         xaxis = list(title = "Month"))
# -------- Code Chank 31 --------
gbm_md <- h2o.gbm(
  training_frame = train_h,
  nfolds = 5,
  x = x,
  y = y,
  max_depth = 20,
  distribution = "gaussian",
  ntrees = 500,
  learn_rate = 0.1,
  score_each_iteration = TRUE
)
# -------- Code Chank 32 --------

h2o.varimp_plot(gbm_md)

test_h$pred_gbm  <- h2o.predict(gbm_md, test_h)
test_1 <- as.data.frame(test_h)

mape_gbm <- mean(abs(test_1$y - test_1$pred_gbm) / test_1$y)
mape_gbm

# -------- Code Chank 33 --------
plot_ly(data = test_1) %>%
  add_lines(x = ~ date, y = ~y, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ yhat, name = "Linear Regression", line = list(dash = "dot")) %>%
  add_lines(x = ~ date, y = ~ pred_gbm, name = "Gradient Boosting Machine", line = list(dash = "dash")) %>%
  layout(title = "Total Vehicle Sales - Actual vs. Prediction (Gradient Boosting Machine)",
         yaxis = list(title = "Thousands of Units"),
         xaxis = list(title = "Month"))
# -------- Code Chank 34 --------
autoML1 <- h2o.automl(training_frame = train_h,
                      x = x,
                      y = y,
                      nfolds = 5,
                      max_runtime_secs = 60*20,
                      seed = 1234)
# -------- Code Chank 35 --------
autoML1@leaderboard

test_h$pred_autoML  <- h2o.predict(autoML1@leader, test_h)
test_1 <- as.data.frame(test_h)

mape_autoML <- mean(abs(test_1$y - test_1$pred_autoML) / test_1$y)
mape_autoML
# -------- Code Chank 36 --------
plot_ly(data = test_1) %>%
  add_lines(x = ~ date, y = ~y, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ yhat, name = "Linear Regression", line = list(dash = "dot")) %>%
  add_lines(x = ~ date, y = ~ pred_autoML, name = "autoML", line = list(dash = "dash")) %>%
  layout(title = "Total Vehicle Sales - Actual vs. Prediction (Auto ML Model)",
         yaxis = list(title = "Thousands of Units"),
         xaxis = list(title = "Month"))
# -------- Code Chank 37 --------
forecast_h$pred_gbm  <- h2o.predict(gbm_md, forecast_h)
forecast_h$pred_rf  <- h2o.predict(rf_grid_model, forecast_h)
forecast_h$pred_automl  <- h2o.predict(autoML1@leader, forecast_h)

final_forecast <- as.data.frame(forecast_h)

plot_ly(x = df$date, y = df$y,
        type = "scatter",
        mode = "line", 
        name = "Actual") %>% 
  add_lines(x = final_forecast$date, y = final_forecast$pred_rf, name = "Random Forest") %>%
  add_lines(x = final_forecast$date, y = final_forecast$pred_gbm, name = "GBM") %>%
  add_lines(x = final_forecast$date, y = final_forecast$pred_automl, name = "Auto ML")
  #layout(title = "Total Vehicle Sales - Final Forecast",
  #       yaxis = list(title = "Thousands of Units", range = c(1100, 1750)),
  #       xaxis = list(title = "Month", range = c(as.Date("2016-01-01"), as.Date("2020-01-01"))))

