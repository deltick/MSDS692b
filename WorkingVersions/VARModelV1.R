library(tsm)
library(vars)
library(mFilter)
library(readxl)
library(dplyr)
library(readr)

#library(devtools)
#devtools::install_github("KevinKotze/tsm")
TestV1 <- read_csv("C:/Users/John/Desktop/USBPTS.csv")
t1 <- select(TestV1, xDate, "Southwest Border")

t1 <- rename(t1, date=xDate)
t1 <- rename(t1, value="Southwest Border")

t1TS <- ts(t1$value, start = c(2000, 1), frequency = 12)




USHS2000 <- window(USHS, start=c(2000, 1), end=c(2020, 1))

plot(cbind (t1TS, USHS2000))

t1.acf <- ac(t1TS,  main ="output")
hs.acf <- ac(USHS2000,  main ="output")


adf.t1TS <- ur.df(t1TS, type ="trend", selectlags = "AIC" )
summary(adf.t1TS)

adf.hs <- ur.df(USHS2000, type ="trend", selectlags = "AIC" )
summary(adf.hs)

dat.bv <- cbind(t1TS, USHS2000) 
colnames(dat.bv) <- c("t1" , "hs" ) 

info.bv <-  VARselect(dat.bv, lag.max = 12,   type = "const" ) 
info.bv$selection



