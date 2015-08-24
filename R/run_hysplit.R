## This script runs Hysplit after downloading met data and looks for the data
## in certain folders specified within the function
## 
## Author: Yusri Yusup
## Date modified: 2015-08-05

# Load packages
library(openair)
library(mapdata) # To use hi-res maps
library(mapproj)

source('R/procTraj.R')
source('R/read.files.R')
source('R/getMet.R')
source('R/add.met.R')

# Download the data, commented out so that the data is not downloaded again
#getMet(year=1998:2012,month=1:12)

# Run Hysplit, this function changes the working directory.
# Need to change lat, long, name, hours, and height
for(i in 2013){
  procTraj(lat=5.356249,lon=100.307905,year=i,
           name='penang',hours=8,height=300,
           met='/Users/Yusri/Documents/Work/Data analysis/ozone_paper/TrajData/',
           out='/Users/Yusri/Documents/Work/Data analysis/ozone_paper/TrajProc/')
}
# Reset working directory
setwd('/Users/Yusri/Documents/Work/Data analysis/ozone_paper/')
# Import data
trajPenang<-importTraj(site='penang',year=2013,local='./TrajProc/')


# For S18 for the year 2012 and 96 hours
procTraj(lat=5.401581,lon=100.334703,year=2012,
         name='s18_2012_96h',hours=96,height=10,
         met='/Users/Yusri/Documents/Work/Data analysis/ozone_paper/TrajData/',
         out='/Users/Yusri/Documents/Work/Data analysis/pm10/TrajProc/')
# Reset working directory
setwd('/Users/Yusri/Documents/Work/Data analysis/pm10/')
# Import data
trajS18_2012_96h<-importTraj(site='s18_2012_96h',year=2012,local='./TrajProc/')




# For usm for the year 2012 and 96 hours
procTraj(lat=5.357728,lon=100.301739,year=2012,
         name='usm_2012_96h',hours=96,height=10,
         met='/Users/Yusri/Documents/Work/Data analysis/ozone_paper/TrajData/',
         out='/Users/Yusri/Documents/Work/Data analysis/pm10/TrajProc/')
# Reset working directory
setwd('/Users/Yusri/Documents/Work/Data analysis/pm10/')
# Import data
trajUSM_2012_96h<-importTraj(site='usm_2012_96h',year=2012,local='./TrajProc/')




# Trajectory plots
trajPlot(selectByDate(subset(trajS18,lat > 0 & lat < 10 & lon > 95 & lon < 105),
                      start='20/6/2012',end='20/6/2012'),
         orientation=c(90,90,0),projection='mercator',
         parameters=NULL,map.res = 'hires')

trajPlot(subset(usmt),orientation = c(90,90,0),
         plot.type='l',projection = 'mercator',
         parameters=NULL,pollutant = 'pm10',map.res='hires',type='monsoon')

trajLevel(s18t,orientation = c(90,90,0),projection = 'mercator',
          parameters=NULL,pollutant = 'Cd',map.res='hires')

trajLevel(trajPenang,statistic='frequency',col='increment',projection='mercator',
          parameters=NULL)

trajLevel(subset(trajPenang,lat > 0 & lat < 10 & lon > 95 & lon < 105),
          col='increment',orientation=c(90,90,0),method='hexbin',statistic='difference',
          projection='mercator',parameters=NULL,map.res='hires',grid.col='black')

trajLevel(s18t,pollutant='pm10',statistic='frequency',orientation = c(90,90,0))

trajLevel(s18t,pollutant='pm10',statistic='difference',orientation=c(90,90,0),
          map.res='hires')

# For date 2012-06-20

d1 <- selectByDate(trajS18_2012_96h,start='20/6/2012',end='20/6/2012')
d2 <- selectByDate(trajS18_2012_96h,start='11/8/2012',end='11/8/2012')
d <- rbind(d1,d2)
d$day <- as.Date(d$date)

trajPlot(d,
         map.res = 'hires', projection = 'mercator',parameters=NULL,
         grid.col = 'black',lwd=2,type='day',layout=c(2,1))
