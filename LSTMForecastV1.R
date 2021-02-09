library(keras)
library(tensorflow)

library(ggplot2)


library (readr)
library(dplyr)


urlfile="https://raw.githubusercontent.com/deltick/MSDS692b/main/Data/USBPTS.csv"

inputTS<-read_csv(url(urlfile))

inputSel2 <- inputTS %>% select(xDate, "Southwest Border")
inputSel2 <- dplyr::rename(inputSel2, Value = "Southwest Border")
df <- inputSel2
#head(economics)




scale_factors <- c(mean(df$Value), sd(df$Value))

scaled_train <- df %>%
  dplyr::select(Value) %>%
  dplyr::mutate(Value = (Value - scale_factors[1]) / scale_factors[2])

prediction <- 12
lag <- prediction

scaled_train <- as.matrix(scaled_train)

# we lag the data 11 times and arrange that into columns
x_train_data <- t(sapply(
  1:(length(scaled_train) - lag - prediction + 1),
  function(x) scaled_train[x:(x + lag - 1), 1]
))

# now we transform it into 3D form
x_train_arr <- array(
  data = as.numeric(unlist(x_train_data)),
  dim = c(
    nrow(x_train_data),
    lag,
    1
  )
)

x_train_arr

y_train_data <- t(sapply(
  (1 + lag):(length(scaled_train) - prediction + 1),
  function(x) scaled_train[x:(x + prediction - 1)]
))

y_train_arr <- array(
  data = as.numeric(unlist(y_train_data)),
  dim = c(
    nrow(y_train_data),
    prediction,
    1
  )
)

x_test <- df$Value[(nrow(scaled_train) - prediction + 1):nrow(scaled_train)]

# scale the data with same scaling factors as for training
x_test_scaled <- (x_test - scale_factors[1]) / scale_factors[2]

# this time our array just has one sample, as we intend to perform one 12-months prediction
x_pred_arr <- array(
  data = x_test_scaled,
  dim = c(
    1,
    lag,
    1
  )
)


lstm_model <- keras_model_sequential()

lstm_model %>%
  layer_lstm(units = 50, # size of the layer
             batch_input_shape = c(1, 12, 1), # batch size, timesteps, features
             return_sequences = TRUE,
             stateful = TRUE) %>%
  # fraction of the units to drop for the linear transformation of the inputs
  layer_dropout(rate = 0.5) %>%
  layer_lstm(units = 50,
             return_sequences = TRUE,
             stateful = TRUE) %>%
  layer_dropout(rate = 0.5) %>%
  time_distributed(keras::layer_dense(units = 1))

lstm_model %>%
  compile(loss = 'mae', optimizer = 'adam', metrics = 'accuracy')

summary(lstm_model)

lstm_model %>% fit(
  x = x_train_arr,
  y = y_train_arr,
  batch_size = 1,
  epochs = 20,
  verbose = 0,
  shuffle = FALSE
)

lstm_forecast <- lstm_model %>%
  predict(x_pred_arr, batch_size = 1) %>%
  .[, , 1]

# we need to rescale the data to restore the original values
lstm_forecast <- lstm_forecast * scale_factors[2] + scale_factors[1]
lstm_forecast

fitted <- predict(lstm_model, x_train_arr, batch_size = 1) %>%
  .[, , 1]

if (dim(fitted)[2] > 1) {
  fit <- c(fitted[, 1], fitted[dim(fitted)[1], 2:dim(fitted)[2]])
} else {
  fit <- fitted[, 1]
}

# additionally we need to rescale the data
fitted <- fit * scale_factors[2] + scale_factors[1]
nrow(fitted) # 562

# I specify first forecast values as not available
fitted <- c(rep(NA, lag), fitted)

lstm_forecast <- timetk::tk_ts(lstm_forecast,
                               start = c(2016, 1),
                               end = c(2019, 12),
                               frequency = 12
)

input_ts <- timetk::tk_ts(df$Value, 
                          start = c(2000, 1), 
                          end = c(2019, 12), 
                          frequency = 12)


forecast_list <- list(
  model = NULL,
  method = "LSTM",
  mean = lstm_forecast,
  x = input_ts,
  fitted = fitted,
  residuals = as.numeric(input_ts) - as.numeric(fitted)
)

class(forecast_list) <- "forecast"


forecast::autoplot(forecast_list)

#### Using Regressors 

# reg <- 100 * runif(nrow(economics))
# 
# scale_factors_reg <- list(
#   mean = mean(reg),
#   sd = sd(reg)
# )
# 
# scaled_reg <- (reg - scale_factors_reg$mean)/scale_factors_reg$sd
# 
# # additionally 12 values for the forecast
# scaled_reg_prediction <- (reg[(length(reg) -12): length(reg)] - 
#                             scale_factors_reg$mean) /scale_factors_reg$sd
# 
# 
# 
# # combine training data with regressors
# x_train <- cbind(scaled_train, scaled_reg)
# x_train_data <- list()
# 
# # transform the data into lagged columns
# for (i in 1:ncol(x_train)) {
#   x_train_data[[i]] <- t(sapply(
#     1:(length(x_train[, i]) - lag - prediction + 1),
#     function(x) x_train[x:(x + lag - 1), i]
#   ))
# }
# 
# x_train_arr <- array(
#   data = as.numeric(unlist(x_train_data)),
#   dim = c(
#     nrow(x_train_data[[1]]),
#     lag,
#     2
#   )
# )
# 
# x_train_arr
# 
# 
# # combine the data with regressors
# x_test_data <- c(x_test_scaled, scaled_reg_prediction)
# 
# # transform to tensor
# x_pred_arr <- array(
#   data = x_test_data,
#   dim = c(
#     1,
#     lag,
#     2
#   )
# )
# 
# #Rest of the modeling stays the same.




