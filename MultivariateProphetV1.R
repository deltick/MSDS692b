library(prophet)
library(readxl)
library(TSstudio)
library(dplyr)
library(forecast)
library(readr)
#library(timeSeries)
library(vars)
library(dynlm)
library(ggplot2)
library(tseries)
require(MTS)
library(tidyverse)
library(prophet)

urlfile="https://raw.githubusercontent.com/deltick/MSDS692b/main/Data/USBPTS.csv"
inputTS<-read_csv(url(urlfile))
inputSel2 <- inputTS %>% dplyr::select("Southwest Border")
inputSel2 <- dplyr::rename(inputSel2, Value = "Southwest Border")
swb.ts <- ts(inputSel2, start=c(2000,1), frequency=12)
inputDates <- inputTS %>% dplyr::select(xDate)

urlfile2="https://raw.githubusercontent.com/deltick/MSDS692b/main/Data/USMonthlyEconData.csv"
inputEcon <- read_csv(url(urlfile2))

## Create separate vectors for econ variables

gdp <- inputEcon %>% select(USGDP)
housing <- inputEcon %>% select(USHousingStarts)
unemp <- inputEcon %>% select(USUnemployment)

## Create time series object and set matchilibraryng windows

gdp.ts <- ts(gdp, start=c(1960, 1), frequency=12)
gdp.ts <- window(gdp.ts, start=c(2000, 1), end=c(2019,12))
forecast::autoplot(gdp.ts)

housing.ts <- ts(housing, start=c(1960, 1), frequency=12)
housing.ts <- window(housing.ts, start=c(2000, 1), end=c(2019,12))
forecast::autoplot(housing.ts)

unemp.ts <- ts(unemp, start=c(1960, 1), frequency=12)
unemp.ts <- window(unemp.ts, start=c(2000, 1), end=c(2019,12))
forecast::autoplot(unemp.ts)

econ.ts <- cbind(gdp.ts, housing.ts, unemp.ts)
forecast::autoplot(econ.ts)

## Center and scale all variables

swb.scaled.ts <- scale(swb.ts)
gdp.scaled.ts <- scale(gdp.ts)
housing.scaled.ts <- scale(housing.ts)
unemp.scaled.ts <- scale(unemp.ts)

## Combine series and plot

combined.ts <- cbind(swb.scaled.ts, gdp.scaled.ts, housing.scaled.ts, unemp.scaled.ts )
autoplot(combined.ts) +
  ggtitle('Plot of Scaled Series')#df <- read_excel("C:/Users/JJT/Desktop/SWBAppsV2.xlsx")

swb.scaled.ts.df <- ts_reshape(swb.scaled.ts, type="long")
swb.scaled.ts.df <- swb.scaled.ts.df$value
gdp.scaled.ts.df <- ts_reshape(gdp.scaled.ts, type="long")
gdp.scaled.ts.df <- gdp.scaled.ts.df$value
housing.scaled.ts.df <- ts_reshape(housing.scaled.ts, type="long")
housing.scaled.ts.df <- housing.scaled.ts.df$value
unemp.scaled.ts.df <- ts_reshape(unemp.scaled.ts, type="long")
unemp.scaled.ts.df <- unemp.scaled.ts.df$value

df <- cbind(inputDates,swb.scaled.ts.df, housing.scaled.ts.df, unemp.scaled.ts.df )

#df <- read_excel("C:/Users/JJT/Desktop/SWBv3.xlsx")
#tail(df)

names(df) <- c('ds', 'y', 'y2', 'y3') 


###################################################
m <- prophet(df)

m <- prophet()
m <- fit.prophet(m, df)


future <- make_future_dataframe(m, periods=24)

forecast <- predict(m, future)
tail(forecast)

plot(m, forecast)

prophet_plot_components(m, forecast)

#df.cv <- cross_validation(m, initial=240, period=30, horizon=12, units='days')
#tail(df.cv)

#plot_cross_validation_metric(df.cv, metric = 'mape')



lax_passengers <- read.csv("https://raw.githubusercontent.com/mitchelloharawild/fable.prophet/master/data-raw/lax_passengers.csv")
library(dplyr)
library(lubridate)
lax_passengers <- lax_passengers %>%
  mutate(datetime = mdy_hms(ReportPeriod)) %>%
  dplyr::group_by(month = yearmonth(datetime), type = Domestic_International) %>%
  dplyr::summarise(passengers = sum(Passenger_Count)) %>%
  ungroup()

lax_passengers
library(fable.prophet)
library(tsibble)
lax_passengers <- as_tsibble(lax_passengers, index = month, key = type)
lax_passengers

lax_passengers %>% 
  autoplot(passengers)

prophet(passengers ~ growth("linear") + season("year", type = "multiplicative"))

fit <- lax_passengers %>% 
  model(
    mdl = prophet(passengers ~ growth("linear") + season("year", type = "multiplicative")),
  )
fit

components(fit)

components(fit) %>% 
  autoplot()
library(ggplot2)
components(fit) %>% 
  ggplot(aes(
    # Plot the month of the time index (month) on the x-axis
    x = month(month, label = TRUE),
    # Plot the annual seasonal term (year) on the y-axis
    y = year, 
    # Colour by the passenger type
    colour = type,
    # Draw separate lines for each type and year
    group = interaction(type, year(month))
  )) +  
  geom_line()

fc <- fit %>% 
  forecast(h = "3 years")
fc

fc %>% 
  autoplot(lax_passengers)

accuracy(fit)

################################################################################
dates1 <- inputDates
dates2 <- inputDates
datesAll <- rbind(dates1, dates2)
unemp.scaled.ts.df.melt <- melt(unemp.scaled.ts.df,value.name = "value")



