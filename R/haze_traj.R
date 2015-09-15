# To import met data
#getMet(year=2013,month=12)

# To run HYSPLIT
procTraj(lat=siteDetails$latitude[2],lon=siteDetails$longitude[2],year=2014,
         name='usm',hours=96,height=10,
         met='/Users/Yusri/Documents/Work/Data_analysis/pm10/TrajData/',
         out='/Users/Yusri/Documents/Work/Data_analysis/pm10/TrajProc/')
# To import processed trajectory data into R
traj2015<-importTraj(site='usm',year=2015,local='/Users/Yusri/Documents/Work/Data_analysis/pm10/TrajProc/')

# To plot trajectory levels
trajLevel(traj2015,
          xlim=c(94,106),ylim=c(-1,11),
          statistic='difference',projection='mercator',
          parameters = NULL,border=NA,col='increment',
          map.alpha=0.6,grid.col = 'transparent',
          origin=TRUE, fontsize = 16)

# Use selectByDate function by month iteratively to combine later to draw animation
#pb <- txtProgressBar(min = 0, max = 217, style = 3)
for (i in 1:12){
  for (j in 1:31){
    # creating a name for each plot file with leading zeros
    if (j < 10) {name = paste('0',j,sep='')}
    else {name = j}
    
    if (i < 10) {name_i = paste('0',i,sep='')}
    else {name_i = i}
    
    # Conditions for months
    if (i == 2 & j > 28) # February
      break
    if (i == 4 & j > 30) # April
      break
    if (i == 6 & j > 28) # June (note: only 28 days for June)
      break
    if (i == 9 & j > 30) # June (note: only 28 days for June)
      break
    if (i == 11 & j > 30) # June (note: only 28 days for June)
      break
    
    
    png(filename = paste('figs/haze/2014_', name_i, '_', name, '.png',sep=''), 
        height = 400, width = 400)
    trajPlots <- trajPlot(selectByDate(traj2014, year = 2014,month = i,day = j),
                          xlim=c(90,106),ylim=c(-5,11),
                          projection='mercator',parameters = NULL,origin=TRUE,
                          main = paste('4-day back trajectory, Month ', i, ' 2014'))
    print(trajPlots)
    dev.off()
    
  }
  #setTxtProgressBar(pb, i)
}
