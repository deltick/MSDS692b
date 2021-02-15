library(prophet)
library(readxl)


#df <- read_excel("C:/Users/JJT/Desktop/SWBAppsV2.xlsx")
df <- read_excel("C:/Users/JJT/Desktop/SWBv3.xlsx")
tail(df)

names(df) <- c('ds', 'y')    #, 'y2', 'y3') 


###################################################
add_regressor(m, gdp)
m <- prophet(df)

m <- prophet()
m <- add_regressor(m, "gdp")
m <- fit.prophet(m, df)


future <- make_future_dataframe(m, periods=24)

forecast <- predict(m, future)
tail(forecast)

plot(m, forecast)

prophet_plot_components(m, forecast)

df.cv <- cross_validation(m, initial=240, period=30, horizon=12, units='days')
tail(df.cv)

plot_cross_validation_metric(df.cv, metric = 'mape')