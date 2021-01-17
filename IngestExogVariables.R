library(readxl)
USUncertainty <- read_excel("C:/Users/John/Desktop/USUncertainty.xlsx", 
                              +     skip = 10)



####### US World Uncertainty Index
USU <- ts(USUncertainty$WUIUSA, start = c(1952, 1), frequency = 4)
autoplot(USU)
USU2000 <- window(USU, start=c(2000, 1), end=c(2019,4))
autoplot(USU2000)



