library (readr)
library(tsibble)
library(tsibbledata)
library(fabletools)
library(fable)
library(dplyr)
library(reshape)
library(feasts)
library(timetk)
library(tidyverse)
library(ggplot2)
library(anomalize)
library(aTSA)
library(tsbox)
library(tsoutliers)
library(tidyverse)
library(tidyquant)
library(anomalize)



urlfile="https://raw.githubusercontent.com/deltick/MSDS692b/main/Data/USBPTS.csv"

inputTS<-read_csv(url(urlfile))

inputSel <- inputTS %>% select(xDate, 'San Diego', 'El Paso', 'Laredo', 'Rio Grande Valley', 'Del Rio', 'El Centro', 'Tucson', 'Yuma')

inputSel <- dplyr::rename(inputSel, SDC = 'San Diego')
inputSel <- dplyr::rename(inputSel, ELP = 'El Paso')
inputSel <- dplyr::rename(inputSel, LRT = 'Laredo')
inputSel <- dplyr::rename(inputSel, RGV = 'Rio Grande Valley')
inputSel <- dplyr::rename(inputSel, DRT = 'Del Rio')
inputSel <- dplyr::rename(inputSel, ELC = 'El Centro')
inputSel <- dplyr::rename(inputSel, TUS = 'Tucson')
inputSel <- dplyr::rename(inputSel, YUM = 'Yuma')

##inputSel <- select(inputSel, -xDate)

inputMelt <- melt(as.data.frame(inputSel), id="xDate") #, variable_name = "count")

#inputTs2 <- ts(inputSel, start=c(2000, 1), frequency=12)

inputTsb <- as_tsibble(inputMelt, index=xDate, key=variable)

#inputTsb %>% autoplot(vars(SD, RGV, LRT, ELP))

inputTsb %>% autoplot()


inputSel2 <- inputTS %>% select(xDate, "Southwest Border")
inputSel2 <- dplyr::rename(inputSel2, SWB = "Southwest Border")
inputMelt2 <- melt(as.data.frame(inputSel2), id="xDate")

inputMelt2 <- inputMelt2 %>%
  mutate(Month = yearmonth(xDate)) %>%
  as_tsibble(index = Month)

inputMelt2 <- select(inputMelt2, -xDate)

inputTsb2 <- as_tsibble(inputMelt2, index=Month, key=variable)
inputTsb2 %>% autoplot()

### EDA

summary(inputSel2)
hist(inputSel2$SWB)



swb <- inputTsb2

dcmp <- swb %>%
  model(STL(value))
components(dcmp)

autoplot(swb, value, color="gray") +
  autolayer(components(dcmp), trend, color="red")

components(dcmp) %>% 
  autoplot()


### timetk anomaly detection

swbTib <- as_tibble(inputSel2)

swbTib %>%
  plot_anomaly_diagnostics(xDate, SWB, .facet_ncol = 2)

swbTib %>%
  tk_anomaly_diagnostics(xDate, SWB )

###################################################
# anomalize package

swbTib <- as_tibble(inputSel2)

swbTib %>% 
  time_decompose(SWB, method='stl') %>%
  anomalize(remainder, method='iqr') %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed=TRUE, ncol=3, alpha_dots=0.25)

swbTib %>%
  time_decompose(SWB) %>%
  anomalize(remainder) %>%
  plot_anomaly_decomposition() +
  labs(title = "Decomposition of Anomalized Lubridate Downloads")


library(tidyverse)
library(tidyquant)
library(anomalize)
library(timetk)
swbTib %>%
  ggplot(aes(xDate, SWB)) +
  geom_point(alpha = 0.5) +
  #facet_wrap(~ package, ncol = 3, scales = "free_y") +
  scale_color_viridis_d() +
  theme_tq() 

swb_anomalized <- swbTib %>%
  time_decompose(SWB) %>%
  anomalize(remainder) %>%
  
  # Function to clean & repair anomalous data
  clean_anomalies()

swb_anomalized %>%
  plot_time_series(xDate, swb_anomalized$observed_cleaned)

swb_anomalized %>%
  plot_time_series(xDate, swb_anomalized$observed)


swb_anomalized %>%
  forecast_mae(col_train = observed, col_test = observed, prop = 0.8)

swb_anomalized %>%
  forecast_mae(col_train = observed_cleaned, col_test = observed, prop = 0.8)

######################################################################
forecast_mae <- function(data, col_train, col_test, prop = 0.8) {
  
  predict_expr <- enquo(col_train)
  actual_expr <- enquo(col_test)
  
  idx_train <- 1:(floor(prop * nrow(data)))
  
  train_tbl <- data %>% filter(row_number() %in% idx_train)
  test_tbl  <- data %>% filter(!row_number() %in% idx_train)
  
  # Model using training data (training) 
  model_formula <- as.formula(paste0(quo_name(predict_expr), " ~ index.num + year + quarter + month.lbl + day + wday.lbl"))
  
  model_glm <- train_tbl %>%
    tk_augment_timeseries_signature() %>%
    glm(model_formula, data = .)
  
  # Make Prediction
  suppressWarnings({
    # Suppress rank-deficit warning
    prediction <- predict(model_glm, newdata = test_tbl %>% tk_augment_timeseries_signature()) 
    actual     <- test_tbl %>% pull(!! actual_expr)
  })
  
  # Calculate MAE
  mae <- mean(abs(prediction - actual))
  
  return(mae)
  
}

### EDA Tests using aTSA for ADF and tsbox to convert

x.ts <- ts(swb$value, start=c(2000, 01), frequency=12)
adf.test(x.ts)


## anomaly detection with tsoutliers

outliers_excess_ts <- tso(x.ts, types = c("TC", "AO", "LS", "IO", "SLS"))
outliers_excess_ts
plot(outliers_excess_ts)

# time index where the outliers have been detected
(outliers_idx <- outliers_excess_ts$outliers$ind)

#length of our time series
n <- length(x.ts)

# transient change outlier at the same time index as found for our time series
mo_tc <- outliers("TC", outliers_idx)

# transient change effect data is stored into a one-column matrix, tc
tc <- outliers.effects(mo_tc, n)

# converting to a number
coefhat <- as.numeric(outliers_excess_ts$outliers["coefhat"])

# obtaining the transient change data with same magnitude as determined by the tso() function
tc_effect <- coefhat*tc

# definining a time series for the transient change data
tc_effect_ts <- ts(tc_effect, frequency = frequency(x.ts), start = start(x.ts))

# subtracting the transient change intervention to the original time series, obtaining a time series without the transient change effect
excess_wo_ts <- x.ts - tc_effect_ts

# plot of the original, without intervention and transient change time series 
plot(cbind(x.ts, excess_wo_ts, tc_effect_ts))

plot(excess_ts, type ='b', ylab = "excess birth ratio")
lines(excess_wo_ts, col = 'red', lty = 3, type ='b')

### Neural Net from Bookdown book

require(tidyverse)
require(tidymodels)
require(data.table)
require(tidyposterior)
require(tsibble)  #tsibble for time series based on tidy principles
require(fable)  #for forecasting based on tidy principles
require(ggfortify)  #for plotting timeseries
require(forecast)  #for forecast function
require(tseries)
require(chron)
require(lubridate)
require(directlabels)
require(zoo)
require(lmtest)
require(TTR)  #for smoothing the time series
require(MTS)
require(vars)
require(fUnitRoots)
require(lattice)
require(grid)
library(sweep)   # Broom tidiers for forecast pkg
library(broom)


fit = nnetar(x.ts)
nnetforecast <- forecast(fit, h = 24, PI = T)  #Prediction intervals do not come by default in neural net forecasts, in constrast to ARIMA or exponential smoothing model
autoplot(nnetforecast) + theme(plot.title = element_text(hjust = 0.5))

sweep::sw_glance(fit) 

sweep::sw_augment(fit )%>%head()

forecast::forecast(fit,h=12,level = c(90, 95))%>%tk_tbl()



