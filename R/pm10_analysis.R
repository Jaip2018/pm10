## Reanalysis of the PM10 and metal compositions at S18 and USM
## 
## Author: Yusri Yusup, PhD
## Date created: 2015-08-17

## Preliminaries
library(openair)


#### 1. Import and process data ####
s18 <- read.csv('data/S18R2.csv',sep=',')
usm <- read.csv('data/USMR2.csv',sep=',')

## Change to POSIX date-time format
s18$DATE <- as.POSIXlt(s18$DATE,format='%d/%m/%Y')
usm$DATE <- as.POSIXlt(usm$DATE,format='%d/%m/%y')

## Rename some of the columns
names(s18)[2] <- 'date'
names(s18)[14] <- 'pm10'
names(s18)[15] <- 'ws'
names(s18)[16] <- 'wd'

names(usm)[2] <- 'date'
names(usm)[14] <- 'pm10'
names(usm)[15] <- 'ws'
names(usm)[16] <- 'wd'

# Cutting data into months
s18 <- cutData(s18,type='month')
usm <- cutData(usm,type='month')

# For s18 classify data based on months of the monsoon season
monsoon <- ''
for (i in 1:nrow(s18)){
  if (s18$month[i] == 'December' | s18$month[i] == 'January' |
      s18$month[i] == 'February' | s18$month[i] == 'March') {
    monsoon[i] <- 'NEM'
  } else if (s18$month[i] == 'April' | s18$month[i] == 'May') {
    monsoon[i] <- 'STM'
  } else if (s18$month[i] == 'June' | s18$month[i] == 'July' |
             s18$month[i] == 'August' | s18$month[i] == 'September'){
    monsoon[i] <- 'SWM'
  } else {
    monsoon[i] <- 'FTM'
  }
}
monsoon<-factor(monsoon,levels=c('NEM','STM','SWM','FTM'),ordered=TRUE)
s18 <- cbind(s18,monsoon)
rm(monsoon,i)

# For usm classify data based on months of the monsoon season
monsoon <- ''
for (i in 1:nrow(usm)){
  if (usm$month[i] == 'December' | usm$month[i] == 'January' |
      usm$month[i] == 'February' | usm$month[i] == 'March') {
    monsoon[i] <- 'NEM'
  } else if (usm$month[i] == 'April' | usm$month[i] == 'May') {
    monsoon[i] <- 'STM'
  } else if (usm$month[i] == 'June' | usm$month[i] == 'July' |
             usm$month[i] == 'August' | usm$month[i] == 'September'){
    monsoon[i] <- 'SWM'
  } else {
    monsoon[i] <- 'FTM'
  }
}
monsoon<-factor(monsoon,levels=c('NEM','STM','SWM','FTM'),ordered=TRUE)
usm <- cbind(usm,monsoon)
rm(monsoon,i)

# For s18 classify data based on 'weekday' or 'weekend'
day <- ''
for (i in 1:nrow(s18)){
  if (s18$D[i] == 'Sat' | s18$D[i] == 'Sun') {
    day[i] <- 'weekend'
  } else {
    day[i] <- 'weekday'
  }
}
day<-factor(day,levels=c('weekday','weekend'),ordered=TRUE)
s18 <- cbind(s18,day)
rm(day,i)

# For usm classify data based on 'weekday' or 'weekend'
day <- ''
for (i in 1:nrow(usm)){
  if (usm$D[i] == 'Sat' | usm$D[i] == 'Sun') {
    day[i] <- 'weekend'
  } else {
    day[i] <- 'weekday'
  }
}
day<-factor(day,levels=c('weekday','weekend'),ordered=TRUE)
usm <- cbind(usm,day)
rm(day,i)

#### 2. Exploratory analyses ####
# pm10
polarPlot(s18,pollutant='pm10',k=50,statistic='mean',main='s18')
polarPlot(usm,pollutant='pm10',k=50,statistic='mean',main='usm')
# Pb
polarPlot(s18,pollutant='Pb',k=50,statistic='mean',main='s18')
polarPlot(usm,pollutant='Pb',k=50,statistic='mean',main='usm')
# Cd
polarPlot(s18,pollutant='Cd',k=50,statistic='mean',main='s18')
polarPlot(usm,pollutant='Cd',k=50,statistic='mean',main='usm')

# Fe
polarPlot(s18,pollutant='Fe',k=50,statistic='mean',main='s18')
polarPlot(usm,pollutant='Fe',k=50,statistic='mean',main='usm')

# Mn
polarPlot(s18,pollutant='Mn',k=50,statistic='mean',main='s18')
polarPlot(usm,pollutant='Mn',k=50,statistic='mean',main='usm')

# Mg
polarPlot(s18,pollutant='Mg',k=50,statistic='mean',main='s18')
polarPlot(usm,pollutant='Mg',k=50,statistic='mean',main='usm')

# Ni
polarPlot(s18,pollutant='Ni',k=50,statistic='mean',main='s18')
polarPlot(usm,pollutant='Ni',k=50,statistic='mean',main='usm')

# Zn
polarPlot(s18,pollutant='Zn',k=50,statistic='mean',main='s18')
polarPlot(usm,pollutant='Zn',k=50,statistic='mean',main='usm')

# Be
polarPlot(s18,pollutant='Be',k=50,statistic='mean',main='s18')
polarPlot(usm,pollutant='Be',k=50,statistic='mean',main='usm')

# Ca
polarPlot(s18,pollutant='Ca',k=50,statistic='mean',main='s18')
polarPlot(usm,pollutant='Ca',k=50,statistic='mean',main='usm')

# Cu
polarPlot(s18,pollutant='Cu',k=50,statistic='mean',main='s18')
polarPlot(usm,pollutant='Cu',k=50,statistic='mean',main='usm')
