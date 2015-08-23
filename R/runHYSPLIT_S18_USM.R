# Run Hysplit, this function changes the working directory.
## USM
for(i in 2011:2012){
  procTraj(lat=5.357728,lon=100.301739,year=i,
           name='usm',hours=8,height=10,
           met='/Users/Yusri/Documents/Work/Data analysis/ozone_paper/TrajData/',
           out='/Users/Yusri/Documents/Work/Data analysis/pm10/TrajProc/')
}

# Reset working directory
setwd('/Users/Yusri/Documents/Work/Data analysis/pm10/')

# Import data
trajUSM<-importTraj(site='usm',year=2011:2012,local='./TrajProc/')

## S18
# Run Hysplit, this function changes the working directory.
for(i in 2011:2012){
  procTraj(lat=5.401581,lon=100.334703,year=i,
           name='s18',hours=8,height=10,
           met='/Users/Yusri/Documents/Work/Data analysis/ozone_paper/TrajData/',
           out='/Users/Yusri/Documents/Work/Data analysis/pm10/TrajProc/')
}

# Reset working directory
setwd('/Users/Yusri/Documents/Work/Data analysis/pm10/')

# Import data
trajS18<-importTraj(site='s18',year=2011:2012,local='./TrajProc/')

## Merge with trajectory data
s18t <- merge(s18,trajS18,by='date')
usmt <- merge(usm,trajUSM,by='date')
