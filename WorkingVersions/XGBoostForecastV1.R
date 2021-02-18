library(caret)
library(xgboost)
library(forecast)
library(readr)
library(timetk)
library(dplyr)
library(tidyverse)


urlfile="https://raw.githubusercontent.com/deltick/MSDS692b/main/Data/USBPTS.csv"

inputTS<-read_csv(url(urlfile))

inputSel2 <- inputTS %>% select(xDate, "Southwest Border")
inputSel2 <- dplyr::rename(inputSel2, Value = "Southwest Border")
df <- inputSel2

ts <- timetk::tk_ts(df$Value, 
                    start = c(2000, 1),
                    end = c(2019, 12), 
                    frequency = 12)

ts

data <- timetk::tk_tbl(ts)

extended_ts <- stats::ts(matrix(c(ts, rep(NA, 12))),
                         start = start(ts), 
                         frequency = 12)

extended_ts

extended_data <- timetk::tk_tbl(extended_ts)
names(extended_data) <- c("date", "value")

extended_data_mod <- extended_data %>%
  dplyr::mutate(., 
                index_date = as.Date(paste0(lubridate::year(date), "-", lubridate::month(date), "-01")),
                months = lubridate::month(index_date),
                years = lubridate::year(index_date))

data <- extended_data_mod[1:length(ts), ] # initial data

pred <- extended_data_mod[(length(ts) + 1):nrow(extended_data), ] # extended time index

x_train <- xgboost::xgb.DMatrix(as.matrix(data %>%
                                            dplyr::select(months, years)))
x_pred <- xgboost::xgb.DMatrix(as.matrix(pred %>% 
                                           dplyr::select(months, years)))

y_train <- data$value


xgb_trcontrol <- caret::trainControl(
  method = "cv", 
  number = 5,
  allowParallel = TRUE, 
  verboseIter = FALSE, 
  returnData = FALSE
)

xgb_grid <- base::expand.grid(
  list(
    nrounds = c(100, 200),
    max_depth = c(10, 15, 20), # maximum depth of a tree
    colsample_bytree = seq(0.5), # subsample ratio of columns when construction each tree
    eta = 0.1, # learning rate
    gamma = 0, # minimum loss reduction
    min_child_weight = 1,  # minimum sum of instance weight (hessian) needed ina child
    subsample = 1 # subsample ratio of the training instances
  ))


xgb_model <- caret::train(
  x_train, y_train,
  trControl = xgb_trcontrol,
  tuneGrid = xgb_grid,
  method = "xgbTree",
  nthread = 1
)

xgb_model$bestTune

xgb_pred <- xgb_model %>% stats::predict(x_pred)


# prediction on a train set
fitted <- xgb_model %>% 
  stats::predict(x_train) %>%
  stats::ts(start = min(data$date), end = max(data$date), frequency = 12)

# prediction in a form of ts object
xgb_forecast <- xgb_pred %>%
  stats::ts(
    start = min(pred$date),
    end = max(pred$date),
    frequency = 12
  )


# forecast object
forecast_list <- list(
  model = xgb_model$modelInfo,
  method = xgb_model$method,
  mean = xgb_forecast,
  x = ts, 
  fitted = fitted,
  residuals = as.numeric(ts) - as.numeric(fitted)
)
class(forecast_list) <- "forecast"

forecast::autoplot(forecast_list)

### add regressors ##################################################################

# x_train <- xgboost::xgb.DMatrix(cbind(as.matrix(data %>% dplyr::select(months, years)),
#                                       reg_train))
# x_pred <- xgboost::xgb.DMatrix(as.matrix(pred %>%
#                                            dplyr::select(months, years)),
#                                reg_pred)
# y_train <- data$value
#####################################################################################
