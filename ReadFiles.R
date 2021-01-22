library (readr)
library(tsibble)
library(tsibbledata)
library(fabletools)
library(fable)
library(dplyr)
library(reshape)

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


