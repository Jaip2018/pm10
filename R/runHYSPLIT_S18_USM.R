# Run Hysplit, this function changes the working directory.
## USM
for(i in 2011:2012){
  procTraj(lat=siteDetails$latitude[2],lon=siteDetails$longitude[2],year=i,
           name='usm',hours=24,height=10,
           met='/Users/Yusri/Documents/Work/Data_analysis/ozone_paper/TrajData/',
           out='/Users/Yusri/Documents/Work/Data_analysis/pm10/TrajProc/')
}

# Reset working directory
setwd('/Users/Yusri/Documents/Work/Data_analysis/pm10/')

# Import data
trajUSM<-importTraj(site='usm',year=2011:2012,local='./TrajProc/')

## S18
# Run Hysplit, this function changes the working directory.
for(i in 2011:2012){
  procTraj(lat=siteDetails$latitude[1],lon=siteDetails$longitude[1],year=i,
           name='s18',hours=24,height=10,
           met='/Users/Yusri/Documents/Work/Data_analysis/ozone_paper/TrajData/',
           out='/Users/Yusri/Documents/Work/Data_analysis/pm10/TrajProc/')
}

# Reset working directory
setwd('/Users/Yusri/Documents/Work/Data_analysis/pm10/')

# Import data
trajS18<-importTraj(site='s18',year=2011:2012,local='./TrajProc/')

## Merge with trajectory data
s18t <- merge(s18,trajS18,by='date')
usmt <- merge(usm,trajUSM,by='date')

# Trajectory plot with factors
#### S18 ####
# Factor 1: Mn-Fe-Cd
trajPlot(s18t,pollutant = 'f_metal1',
         map.res = 'hires', projection = 'mercator',parameters=NULL,
         grid.col = 'black',lwd=2)
# Factor 2: Pb-Ca
trajPlot(s18t,pollutant = 'f_metal2',
         map.res = 'hires', projection = 'mercator',parameters=NULL,
         grid.col = 'black',lwd=2)
# Factor 3: Ni-Zn
trajPlot(s18t,pollutant = 'f_metal3',
         map.res = 'hires', projection = 'mercator',parameters=NULL,
         grid.col = 'black',lwd=2)
# Factor 4: Mg-Cu
trajPlot(s18t,pollutant = 'f_metal4',
         map.res = 'hires', projection = 'mercator',parameters=NULL,
         grid.col = 'black',lwd=2)

#### USM ####
# Factor 1: Mn-Fe-Cd
trajPlot(usmt,pollutant = 'f_metal1',
         map.res = 'hires', projection = 'mercator',parameters=NULL,
         grid.col = 'black',lwd=2)
# Factor 2: Pb-Ca
trajPlot(usmt,pollutant = 'f_metal2',
         map.res = 'hires', projection = 'mercator',parameters=NULL,
         grid.col = 'black',lwd=2)
# Factor 3: Ni-Zn
trajPlot(usmt,pollutant = 'f_metal3',
         map.res = 'hires', projection = 'mercator',parameters=NULL,
         grid.col = 'black',lwd=2)
# Factor 4: Mg-Cu
trajPlot(usmt,pollutant = 'f_metal4',
         map.res = 'hires', projection = 'mercator',parameters=NULL,
         grid.col = 'black',lwd=2)

# trajLevel plots
trajLevel(subset(usmt,lat > 2 & lat < 8 & lon > 90 & lon < 120),
          pollutant='pm10',statistic='difference',projection='mercator',
          parameters = NULL,border=NA,map.res = 'hires',type='monsoon',col='jet')
trajLevel(subset(usmt,lat > 2 & lat < 8 & lon > 90 & lon < 120),
          method='hexbin',projection='mercator',parameters=NULL,
          map.res = 'hires',type='monsoon',col='jet')