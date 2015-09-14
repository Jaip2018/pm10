
#getMet(year=2014,month=12)

procTraj(lat=siteDetails$latitude[2],lon=siteDetails$longitude[2],year=2015,
         name='usm',hours=96,height=10,
         met='/Users/Yusri/Documents/Work/Data_analysis/pm10/TrajData/',
         out='/Users/Yusri/Documents/Work/Data_analysis/pm10/TrajProc/')

traj2015<-importTraj(site='usm',year=2015,local='/Users/Yusri/Documents/Work/Data_analysis/pm10/TrajProc/')

trajLevel(traj2015,
          xlim=c(94,106),ylim=c(-1,11),
          statistic='difference',projection='mercator',
          parameters = NULL,border=NA,col='increment',
          map.alpha=0.6,grid.col = 'transparent',
          origin=TRUE, fontsize = 16)
trajPlot(selectByDate(traj2015,start='1/7/2015',end='31/7/2015',year=2015),
         projection='mercator',parameters = NULL,origin=TRUE)
trajPlot(selectByDate(traj2015,year=2015,month=7),
         projection='mercator',parameters = NULL,origin=TRUE)
