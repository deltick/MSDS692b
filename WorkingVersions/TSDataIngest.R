
library(readxl)
library(tidyverse)
library(dplyr)
library(tidymodels)
library(modeltime)
library(lubridate)
library(timetk)
library(astsa)
library(forecast)
library(aTSA)

TestV4 <- read_excel("C:/Users/John/Desktop/TestV4.xlsx")
#View(TestV4)

summary(TestV4)

TestV4$Month[TestV4$Month == "JAN"] <- "01"
TestV4$Month[TestV4$Month == "FEB"] <- "02"
TestV4$Month[TestV4$Month == "MAR"] <- "03"
TestV4$Month[TestV4$Month == "APR"] <- "04"
TestV4$Month[TestV4$Month == "MAY"] <- "05"
TestV4$Month[TestV4$Month == "JUN"] <- "06"
TestV4$Month[TestV4$Month == "JUL"] <- "07"
TestV4$Month[TestV4$Month == "AUG"] <- "08"
TestV4$Month[TestV4$Month == "SEP"] <- "09"
TestV4$Month[TestV4$Month == "OCT"] <- "10"
TestV4$Month[TestV4$Month == "NOV"] <- "11"
TestV4$Month[TestV4$Month == "DEC"] <- "12"

TestV4$Month = as.integer(TestV4$Month)

#SumFYMonth <- TestV4 %>% 
#  group_by(FY, Month) %>% 
#  summarise(Count = sum(Count, na.rm=TRUE))



#SumFYMonthV2 <- SumFYMonth[order(SumFYMonth$FY, SumFYMonth$Month),]

SumTS <- ts(SumFYMonthV2$Count, start = c(2010, 1), frequency = 12)

plot(SumTS)


#time_split <- initial_split(SumFYMonthV2)
#train_data <- training(time_split)
#test_data <- testing(time_split)

interactive <- FALSE

xday = '01'

TestV4$date <- paste(TestV4$FY,TestV4$Month,xday, sep="-")

TestV4$xDate <- as.Date(TestV4$date)


SumFYMonthV3 <- TestV4 %>% 
  group_by(xDate) %>% 
  summarise(Count = sum(Count, na.rm=TRUE))



SumFYMonthV3 <- SumFYMonthV3[order(SumFYMonthV3$xDate),]
SumFYMonthV3 %>%
  plot_time_series(xDate, Count, .interactive = interactive)

time_split <- initial_split(SumFYMonthV3)
train_data <- training(time_split)
test_data <- testing(time_split)

model_fit_arima_no_boost <- arima_reg() %>%
  set_engine(engine = "auto_arima") %>%
  fit(Count ~ xDate, data = training(time_split))



################################################################################

eqts <- ts(SumFYMonthV3$Count , start = c(2010, 1), frequency = 12)

## From forecast package ###########
ggtsdisplay(eqts)
ggseasonplot(eqts)

ggAcf(eqts, lag.max = NULL, type = c("correlation", "covariance", "partial"),
      plot = TRUE, na.action = na.contiguous, demean = TRUE)

ggPacf(eqts, lag.max = NULL, plot = TRUE, na.action = na.contiguous,
       demean = TRUE)

#ggCcf(x, y, lag.max = NULL, type = c("correlation", "covariance"),
#      plot = TRUE, na.action = na.contiguous, ...)


eqts %>% diff(lag=1) %>% diff() %>% ggtsdisplay()

###############################################################################

sarima.for(log(eqts),12,0,1,1,0,1,1,12) 
#  with regressors:
nummy   = length(soi)
n.ahead = 24 
nureg   = time(soi)[nummy] + seq(1,n.ahead)/12
sarima.for(soi,n.ahead,2,0,0,2,0,0,12, xreg=time(soi), newxreg=nureg)

################################################################################

SumDemoMonthV1 <- TestV4 %>% 
  group_by(xDate, Citizenship) %>% 
  summarise(Count = sum(Count, na.rm=TRUE))

SumDemoMonthV2 <- filter(SumDemoMonthV1, Citizenship == "MEXICO" | Citizenship == "HONDURAS" | Citizenship == "EL SALVADOR" | Citizenship == "GUATEMALA")
#SumDemoMonthV2 <- select(SumDemoMonthV2, -xDate)
#SumDemoAllV2 <- as.data.frame(SumDemoAll)
#SumDemoAllV2 <- select(SumDemoAll, -xDate)


SumDemoMonthV3 <- filter(SumDemoMonthV1, Citizenship == "MEXICO")
SumDemoMonthV4 <- filter(SumDemoMonthV1, Citizenship == "HONDURAS")
SumDemoMonthV5 <- filter(SumDemoMonthV1, Citizenship == "EL SALVADOR")
SumDemoMonthV6 <- filter(SumDemoMonthV1, Citizenship == "GUATEMALA")

colnames(SumDemoMonthV3)[3] <- "MEXICO"
SumDemoMonthV3 <- select(SumDemoMonthV3, -Citizenship)

colnames(SumDemoMonthV4)[3] <- "HONDURAS"
SumDemoMonthV4 <- select(SumDemoMonthV4, -Citizenship)

colnames(SumDemoMonthV5)[3] <- "ELSALVADOR"
SumDemoMonthV5 <- select(SumDemoMonthV5, -Citizenship)

colnames(SumDemoMonthV6)[3] <- "GUATEMALA"
SumDemoMonthV6 <- select(SumDemoMonthV6, -Citizenship)



SumDemoAll <- left_join(SumDemoMonthV3, SumDemoMonthV4)  
SumDemoAll <- left_join(SumDemoAll, SumDemoMonthV5)
SumDemoAll <- left_join(SumDemoAll, SumDemoMonthV6)

SumDemoAllV2 <- as.data.frame(SumDemoAll)
SumDemoAllV2 <- select(SumDemoAllV2, -xDate)

DemoMonthTS = ts(SumDemoAllV2,
           frequency = 12,
           start = c(2010, 1))
plot(DemoMonthTS)
autoplot(DemoMonthTS)


apply(DemoMonthTS, 2, adf.test)

#stationary.test(eqts, method = "kpss")
##################################################################

timeseriescomponents <- decompose(eqts)
plot(timeseriescomponents)








#######################################################################
