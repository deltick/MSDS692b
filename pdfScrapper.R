# PDF Scrapper
#https://www.cbp.gov/sites/default/files/assets/documents/2020-Jan/U.S.%20Border%20Patrol%20Monthly%20Apprehensions%20%28FY%202000%20-%20FY%202019%29_1.pdf

## John Tamer

library(tabulizer)
library(dplyr)
library(tidymodels)
library(modeltime)
library(lubridate)
library(timetk)
library(astsa)
library(forecast)
library(aTSA)
library(data.table)
library(readr)


pdf_file <- "c:/users/john/desktop/USBP1.pdf"

#  FY2000
FY2000 <- extract_tables(pdf_file, pages = 1)
FY2000DF <- as.data.frame(FY2000)
FY2000DF <- FY2000DF[-2,]
fy2000_transpose <- (transpose(FY2000DF))
names(fy2000_transpose) <- fy2000_transpose[1,]
fy2000_transpose <-fy2000_transpose[-1,]
fy2000_transpose <-fy2000_transpose[-13,]
fy2000_transpose <-fy2000_transpose[,-26]
fy2000_transpose <- rename(fy2000_transpose, xMonth=SECTOR)
fy2000_transpose <- rename(fy2000_transpose, 'Big Bend' = "Big Bend\r(formerly Marfa)" )
fy2000_transpose <- rename(fy2000_transpose, 'Rio Grande Valley' = "Rio Grande Valley\r(formerly McAllen)")
fy2000_transpose$fiscalYear <-2000

## FY2001
FY2001 <- extract_tables(pdf_file, pages = 2)
FY2001DF <- as.data.frame(FY2001)
FY2001DF <- FY2001DF[-2,]
fy2001_transpose <- transpose(FY2001DF)
names(fy2001_transpose) <- fy2001_transpose[1,]
fy2001_transpose <-fy2001_transpose[-1,]
fy2001_transpose <-fy2001_transpose[-13,]
fy2001_transpose <-fy2001_transpose[,-26]
fy2001_transpose <- rename(fy2001_transpose, xMonth=SECTOR)
fy2001_transpose <- rename(fy2001_transpose, 'Big Bend' = "Big Bend\r(formerly Marfa)" )
fy2001_transpose <- rename(fy2001_transpose, 'Rio Grande Valley' = "Rio Grande Valley\r(formerly McAllen)")
fy2001_transpose$fiscalYear <-2001


## FY2002
FY2002 <- extract_tables(pdf_file, pages = 3)
FY2002DF <- as.data.frame(FY2002)
FY2002DF <- FY2002DF[-2,]
fy2002_transpose <- transpose(FY2002DF)
names(fy2002_transpose) <- fy2002_transpose[1,]
fy2002_transpose <-fy2002_transpose[-1,]
fy2002_transpose <-fy2002_transpose[-13,]
fy2002_transpose <-fy2002_transpose[,-26]
fy2002_transpose <- rename(fy2002_transpose, xMonth=SECTOR)
fy2002_transpose <- rename(fy2002_transpose, 'Big Bend' = "Big Bend\r(formerly Marfa)" )
fy2002_transpose <- rename(fy2002_transpose, 'Rio Grande Valley' = "Rio Grande Valley\r(formerly McAllen)")
fy2002_transpose$fiscalYear <-2002


## FY2003
FY2003 <- extract_tables(pdf_file, pages = 4)
FY2003DF <- as.data.frame(FY2003)
FY2003DF <- FY2003DF[-2,]
fy2003_transpose <- transpose(FY2003DF)
names(fy2003_transpose) <- fy2003_transpose[1,]
fy2003_transpose <-fy2003_transpose[-1,]
fy2003_transpose <-fy2003_transpose[-13,]
fy2003_transpose <-fy2003_transpose[,-26]
fy2003_transpose <- rename(fy2003_transpose, xMonth=SECTOR)
fy2003_transpose <- rename(fy2003_transpose, 'Big Bend' = "Big Bend\r(formerly Marfa)" )
fy2003_transpose <- rename(fy2003_transpose, 'Rio Grande Valley' = "Rio Grande Valley\r(formerly McAllen)")
fy2003_transpose$fiscalYear <-2003

## FY2004
FY2004 <- extract_tables(pdf_file, pages = 5)
FY2004DF <- as.data.frame(FY2004)
FY2004DF <- FY2004DF[-2,]
fy2004_transpose <- transpose(FY2004DF)
names(fy2004_transpose) <- fy2004_transpose[1,]
fy2004_transpose <-fy2004_transpose[-1,]
fy2004_transpose <-fy2004_transpose[-13,]
fy2004_transpose <-fy2004_transpose[,-26]
fy2004_transpose <- rename(fy2004_transpose, xMonth=SECTOR)
fy2004_transpose <- rename(fy2004_transpose, 'Big Bend' = "Big Bend\r(formerly Marfa)" )
fy2004_transpose <- rename(fy2004_transpose, 'Rio Grande Valley' = "Rio Grande Valley\r(formerly McAllen)")
fy2004_transpose$fiscalYear <-2004

## FY2005
FY2005 <- extract_tables(pdf_file, pages = 6)
FY2005DF <- as.data.frame(FY2005)
FY2005DF <- FY2005DF[-2,]
fy2005_transpose <- transpose(FY2005DF)
names(fy2005_transpose) <- fy2005_transpose[1,]
fy2005_transpose <-fy2005_transpose[-1,]
fy2005_transpose <-fy2005_transpose[-13,]
fy2005_transpose <-fy2005_transpose[,-26]
fy2005_transpose <- rename(fy2005_transpose, xMonth=SECTOR)
fy2005_transpose <- rename(fy2005_transpose, 'Big Bend' = "Big Bend\r(formerly Marfa)" )
fy2005_transpose <- rename(fy2005_transpose, 'Rio Grande Valley' = "Rio Grande Valley\r(formerly McAllen)")
fy2005_transpose$fiscalYear <-2005

## FY2006
FY2006 <- extract_tables(pdf_file, pages = 7)
FY2006DF <- as.data.frame(FY2006)
FY2006DF <- FY2006DF[-2,]
fy2006_transpose <- transpose(FY2006DF)
names(fy2006_transpose) <- fy2006_transpose[1,]
fy2006_transpose <-fy2006_transpose[-1,]
fy2006_transpose <-fy2006_transpose[-13,]
fy2006_transpose <-fy2006_transpose[,-26]
fy2006_transpose <- rename(fy2006_transpose, xMonth=SECTOR)
fy2006_transpose <- rename(fy2006_transpose, 'Big Bend' = "Big Bend\r(formerly Marfa)" )
fy2006_transpose <- rename(fy2006_transpose, 'Rio Grande Valley' = "Rio Grande Valley\r(formerly McAllen)")
fy2006_transpose$fiscalYear <-2006

## FY2007
FY2007 <- extract_tables(pdf_file, pages = 8)
FY2007DF <- as.data.frame(FY2007)
FY2007DF <- FY2007DF[-2,]
fy2007_transpose <- transpose(FY2007DF)
names(fy2007_transpose) <- fy2007_transpose[1,]
fy2007_transpose <-fy2007_transpose[-1,]
fy2007_transpose <-fy2007_transpose[-13,]
fy2007_transpose <-fy2007_transpose[,-26]
fy2007_transpose <- rename(fy2007_transpose, xMonth=SECTOR)
fy2007_transpose <- rename(fy2007_transpose, 'Big Bend' = "Big Bend\r(formerly Marfa)" )
fy2007_transpose <- rename(fy2007_transpose, 'Rio Grande Valley' = "Rio Grande Valley\r(formerly McAllen)")
fy2007_transpose$fiscalYear <-2007

## FY2008
FY2008 <- extract_tables(pdf_file, pages = 9)
FY2008DF <- as.data.frame(FY2008)
FY2008DF <- FY2008DF[-2,]
fy2008_transpose <- transpose(FY2008DF)
names(fy2008_transpose) <- fy2008_transpose[1,]
fy2008_transpose <-fy2008_transpose[-1,]
fy2008_transpose <-fy2008_transpose[-13,]
fy2008_transpose <-fy2008_transpose[,-26]
fy2008_transpose <- rename(fy2008_transpose, xMonth=SECTOR)
fy2008_transpose <- rename(fy2008_transpose, 'Big Bend' = "Big Bend\r(formerly Marfa)" )
fy2008_transpose <- rename(fy2008_transpose, 'Rio Grande Valley' = "Rio Grande Valley\r(formerly McAllen)")
fy2008_transpose$fiscalYear <-2008

## FY2009
FY2009 <- extract_tables(pdf_file, pages = 10)
FY2009DF <- as.data.frame(FY2009)
FY2009DF <- FY2009DF[-2,]
fy2009_transpose <- transpose(FY2009DF)
names(fy2009_transpose) <- fy2009_transpose[1,]
fy2009_transpose <-fy2009_transpose[-1,]
fy2009_transpose <-fy2009_transpose[-13,]
fy2009_transpose <-fy2009_transpose[,-26]
fy2009_transpose <- rename(fy2009_transpose, xMonth=SECTOR)
fy2009_transpose <- rename(fy2009_transpose, 'Big Bend' = "Big Bend\r(formerly Marfa)" )
fy2009_transpose <- rename(fy2009_transpose, 'Rio Grande Valley' = "Rio Grande Valley\r(formerly McAllen)")
fy2009_transpose$fiscalYear <-2009

## FY2010
FY2010 <- extract_tables(pdf_file, pages = 11)
FY2010DF <- as.data.frame(FY2010)
FY2010DF <- FY2010DF[-2,]
fy2010_transpose <- transpose(FY2010DF)
names(fy2010_transpose) <- fy2010_transpose[1,]
fy2010_transpose <-fy2010_transpose[-1,]
fy2010_transpose <-fy2010_transpose[-13,]
fy2010_transpose <-fy2010_transpose[,-26]
fy2010_transpose <- rename(fy2010_transpose, xMonth=SECTOR)
fy2010_transpose <- rename(fy2010_transpose, 'Big Bend' = "Big Bend\r(formerly Marfa)" )
fy2010_transpose <- rename(fy2010_transpose, 'Rio Grande Valley' = "Rio Grande Valley\r(formerly McAllen)")
fy2010_transpose$fiscalYear <-2010

## FY2011
FY2011 <- extract_tables(pdf_file, pages = 12)
FY2011DF <- as.data.frame(FY2011)
FY2011DF <- FY2011DF[-2,]
fy2011_transpose <- transpose(FY2011DF)
names(fy2011_transpose) <- fy2011_transpose[1,]
fy2011_transpose <-fy2011_transpose[-1,]
fy2011_transpose <-fy2011_transpose[-13,]
fy2011_transpose <-fy2011_transpose[,-26]
fy2011_transpose <- rename(fy2011_transpose, xMonth=SECTOR)
fy2011_transpose <- rename(fy2011_transpose, 'Big Bend' = "Big Bend\r(formerly Marfa)" )
fy2011_transpose <- rename(fy2011_transpose, 'Rio Grande Valley' = "Rio Grande Valley\r(formerly McAllen)")
fy2011_transpose$fiscalYear <-2011

## FY2012
FY2012 <- extract_tables(pdf_file, pages = 13)
FY2012DF <- as.data.frame(FY2012)
FY2012DF <- FY2012DF[-2,]
fy2012_transpose <- transpose(FY2012DF)
names(fy2012_transpose) <- fy2012_transpose[1,]
fy2012_transpose <-fy2012_transpose[-1,]
fy2012_transpose <-fy2012_transpose[-13,]
fy2012_transpose <-fy2012_transpose[,-26]
fy2012_transpose <- rename(fy2012_transpose, xMonth=SECTOR)
fy2012_transpose <- rename(fy2012_transpose, 'Big Bend' = "Big Bend\r(formerly Marfa)" )
fy2012_transpose <- rename(fy2012_transpose, 'Rio Grande Valley' = "Rio Grande Valley\r(formerly McAllen)")
fy2012_transpose$fiscalYear <-2012

## FY2013
FY2013 <- extract_tables(pdf_file, pages = 14)
FY2013DF <- as.data.frame(FY2013)
FY2013DF <- FY2013DF[-2,]
fy2013_transpose <- transpose(FY2013DF)
names(fy2013_transpose) <- fy2013_transpose[1,]
fy2013_transpose <-fy2013_transpose[-1,]
fy2013_transpose <-fy2013_transpose[-13,]
fy2013_transpose <-fy2013_transpose[,-26]
fy2013_transpose <- rename(fy2013_transpose, xMonth=SECTOR)
fy2013_transpose <- rename(fy2013_transpose, 'Big Bend' = "Big Bend\r(formerly Marfa)" )
fy2013_transpose <- rename(fy2013_transpose, 'Rio Grande Valley' = "Rio Grande Valley\r(formerly McAllen)")
fy2013_transpose$fiscalYear <-2013

## FY2014
FY2014 <- extract_tables(pdf_file, pages = 15)
FY2014DF <- as.data.frame(FY2014)
FY2014DF <- FY2014DF[-2,]
fy2014_transpose <- transpose(FY2014DF)
names(fy2014_transpose) <- fy2014_transpose[1,]
fy2014_transpose <-fy2014_transpose[-1,]
fy2014_transpose <-fy2014_transpose[-13,]
fy2014_transpose <-fy2014_transpose[,-26]
fy2014_transpose <- rename(fy2014_transpose, xMonth=SECTOR)
fy2014_transpose <- rename(fy2014_transpose, 'Big Bend' = "Big Bend\r(formerly Marfa)" )
fy2014_transpose <- rename(fy2014_transpose, 'Rio Grande Valley' = "Rio Grande Valley\r(formerly McAllen)")
fy2014_transpose$fiscalYear <-2014

## FY2015
FY2015 <- extract_tables(pdf_file, pages = 16)
FY2015DF <- as.data.frame(FY2015)
FY2015DF <- FY2015DF[-2,]
fy2015_transpose <- transpose(FY2015DF)
names(fy2015_transpose) <- fy2015_transpose[1,]
fy2015_transpose <-fy2015_transpose[-1,]
fy2015_transpose <-fy2015_transpose[-13,]
fy2015_transpose <-fy2015_transpose[,-26]
fy2015_transpose <- rename(fy2015_transpose, xMonth=SECTOR)
fy2015_transpose <- rename(fy2015_transpose, 'Big Bend' = "Big Bend\r(formerly Marfa)" )
fy2015_transpose <- rename(fy2015_transpose, 'Rio Grande Valley' = "Rio Grande Valley\r(formerly McAllen)")
fy2015_transpose$fiscalYear <-2015

## FY2016
FY2016 <- extract_tables(pdf_file, pages = 17)
FY2016DF <- as.data.frame(FY2016)
FY2016DF <- FY2016DF[-2,]
fy2016_transpose <- transpose(FY2016DF)
names(fy2016_transpose) <- fy2016_transpose[1,]
fy2016_transpose <-fy2016_transpose[-1,]
fy2016_transpose <-fy2016_transpose[-13,]
fy2016_transpose <-fy2016_transpose[,-26]
fy2016_transpose <- rename(fy2016_transpose, xMonth=SECTOR)
fy2016_transpose <- rename(fy2016_transpose, 'Big Bend' = "Big Bend\r(formerly Marfa)" )
fy2016_transpose <- rename(fy2016_transpose, 'Rio Grande Valley' = "Rio Grande Valley\r(formerly McAllen)")
fy2016_transpose$fiscalYear <-2016

## FY2017
FY2017 <- extract_tables(pdf_file, pages = 18)
FY2017DF <- as.data.frame(FY2017)
FY2017DF <- FY2017DF[-2,]
fy2017_transpose <- transpose(FY2017DF)
names(fy2017_transpose) <- fy2017_transpose[1,]
fy2017_transpose <-fy2017_transpose[-1,]
fy2017_transpose <-fy2017_transpose[-13,]
fy2017_transpose <-fy2017_transpose[,-26]
fy2017_transpose <- rename(fy2017_transpose, xMonth=SECTOR)
fy2017_transpose <- rename(fy2017_transpose, 'Big Bend' = "Big Bend\r(formerly Marfa)" )
fy2017_transpose <- rename(fy2017_transpose, 'Rio Grande Valley' = "Rio Grande Valley\r(formerly McAllen)")
fy2017_transpose$fiscalYear <-2017

## FY2018
FY2018 <- extract_tables(pdf_file, pages = 19)
FY2018DF <- as.data.frame(FY2018)
FY2018DF <- FY2018DF[-2,]
fy2018_transpose <- transpose(FY2018DF)
names(fy2018_transpose) <- fy2018_transpose[1,]
fy2018_transpose <-fy2018_transpose[-1,]
fy2018_transpose <-fy2018_transpose[-13,]
fy2018_transpose <-fy2018_transpose[,-26]
fy2018_transpose <- rename(fy2018_transpose, xMonth=SECTOR)
fy2018_transpose <- rename(fy2018_transpose, 'Big Bend' = "Big Bend\r(formerly Marfa)" )
fy2018_transpose <- rename(fy2018_transpose, 'Rio Grande Valley' = "Rio Grande Valley\r(formerly McAllen)")
fy2018_transpose$fiscalYear <-2018

## FY 2019
FY2019 <- extract_tables(pdf_file, pages = 20)
FY2019DF <- as.data.frame(FY2019)
fy2019_transpose <- transpose(FY2019DF)
names(fy2019_transpose) <- fy2019_transpose[1,]
fy2019_transpose <-fy2019_transpose[-1,]
fy2019_transpose <-fy2019_transpose[-13,]
fy2019_transpose <-fy2019_transpose[,-26]
fy2019_transpose <- rename(fy2019_transpose, xMonth=SECTOR)
fy2019_transpose$fiscalYear <-2019

######################################################################

combined <- rbind(fy2000_transpose,
                  fy2001_transpose, 
                  fy2002_transpose, 
                  fy2003_transpose,
                  fy2004_transpose,
                  fy2005_transpose,
                  fy2006_transpose,
                  fy2007_transpose,
                  fy2008_transpose,
                  fy2009_transpose,
                  fy2010_transpose,
                  fy2011_transpose,
                  fy2012_transpose,
                  fy2013_transpose,
                  fy2014_transpose,
                  fy2015_transpose,
                  fy2016_transpose,
                  fy2017_transpose,
                  fy2018_transpose,
                  fy2019_transpose)

##################################################################

combinedV2 <- combined

combinedV2$xMonthN[combinedV2$xMonth == "October"] <- "01"
combinedV2$xMonthN[combinedV2$xMonth == "November"] <- "02"
combinedV2$xMonthN[combinedV2$xMonth == "December"] <- "03"
combinedV2$xMonthN[combinedV2$xMonth == "January"] <- "04"
combinedV2$xMonthN[combinedV2$xMonth == "February"] <- "05"
combinedV2$xMonthN[combinedV2$xMonth == "March"] <- "06"
combinedV2$xMonthN[combinedV2$xMonth == "April"] <- "07"
combinedV2$xMonthN[combinedV2$xMonth == "May"] <- "08"
combinedV2$xMonthN[combinedV2$xMonth == "June"] <- "09"
combinedV2$xMonthN[combinedV2$xMonth == "July"] <- "10"
combinedV2$xMonthN[combinedV2$xMonth == "August"] <- "11"
combinedV2$xMonthN[combinedV2$xMonth == "September"] <- "12"

##################################################################

xday = '01'
combinedV2$xDate <- paste(combinedV2$fiscalYear,combinedV2$xMonthN,xday, sep="-")
combinedV2$xDate <- as.Date(combinedV2$xDate)


combinedV2$`Southwest Border`  <- as.numeric(gsub(",", "", combinedV2$`Southwest Border`))
combinedV2$Miami               <- as.numeric(gsub(",", "", combinedV2$Miami))
combinedV2$`New Orleans`       <- as.numeric(gsub(",", "", combinedV2$`New Orleans`))
combinedV2$Ramey               <- as.numeric(gsub(",", "", combinedV2$Ramey))
combinedV2$Blaine              <- as.numeric(gsub(",", "", combinedV2$Blaine))
combinedV2$Buffalo             <- as.numeric(gsub(",", "", combinedV2$Buffalo))
combinedV2$Detroit             <- as.numeric(gsub(",", "", combinedV2$Detroit))
combinedV2$`Grand Forks`       <- as.numeric(gsub(",", "", combinedV2$`Grand Forks`))
combinedV2$Havre               <- as.numeric(gsub(",", "", combinedV2$Havre))
combinedV2$Houlton             <- as.numeric(gsub(",", "", combinedV2$Houlton))
combinedV2$Spokane             <- as.numeric(gsub(",", "", combinedV2$Spokane))
combinedV2$Swanton             <- as.numeric(gsub(",", "", combinedV2$Swanton))
combinedV2$`Big Bend`          <- as.numeric(gsub(",", "", combinedV2$`Big Bend`))
combinedV2$`Del Rio`           <- as.numeric(gsub(",", "", combinedV2$`Del Rio`))
combinedV2$`El Centro`         <- as.numeric(gsub(",", "", combinedV2$`El Centro`))
combinedV2$`El Paso`           <- as.numeric(gsub(",", "", combinedV2$`El Paso`))
combinedV2$Laredo              <- as.numeric(gsub(",", "", combinedV2$Laredo))
combinedV2$`Rio Grande Valley` <- as.numeric(gsub(",", "", combinedV2$`Rio Grande Valley`))
combinedV2$`San Diego`         <- as.numeric(gsub(",", "", combinedV2$`San Diego`))
combinedV2$Tucson              <- as.numeric(gsub(",", "", combinedV2$Tucson))
combinedV2$Yuma                <- as.numeric(gsub(",", "", combinedV2$Yuma))
combinedV2$`Coastal Border`    <- as.numeric(gsub(",", "", combinedV2$`Coastal Border`))
combinedV2$`Northern Border`   <- as.numeric(gsub(",", "", combinedV2$`Northern Border`))
combinedV2$`Monthly Total`     <- as.numeric(gsub(",", "", combinedV2$`Monthly Total`))

#############################################################################################


write_csv(combinedV2, "c:/users/john/desktop/USBPTS.csv", append = FALSE, col_names = TRUE)


# t1 <- select(combined, fiscalYear, SECTOR, "Southwest Border")
# t1$SWB <- as.numeric(gsub(",", "", t1$`Southwest Border`))
# 
# t1TS <- ts(t1$SWB, start = c(2000, 1), frequency = 12)
# 
# autoplot(t1TS)
# 
# summary(t1)

