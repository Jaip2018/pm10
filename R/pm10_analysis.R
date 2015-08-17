## Reanalysis of the PM10 and metal compositions at S18 and USM
## 
## Author: Yusri Yusup, PhD
## Date: 2015-08-17

## Preliminaries
library(openair)


## Import data
s18 <- read.csv('data/S18R2.csv',sep=',')
usm <- read.csv('data/USMR2.csv',sep=',')
## Change to POSIX date-time format
s18R2$DATE <- as.POSIXlt(s18R2$DATE)
## Summary of data
summary(s18R2)
