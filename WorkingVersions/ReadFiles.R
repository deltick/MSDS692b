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

#inputSel <- select(inputSel, -xDate)

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



