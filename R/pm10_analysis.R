## Reanalysis of the PM10 and metal compositions at S18 and USM
## 
## Author: Yusri Yusup, PhD
## Date created: 2015-08-17

## Preliminaries
library(openair)
library(Hmisc)


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

## Add 17 hours to date to coincide with end of sampling time
s18$date <- s18$date + (17*60*60)
usm$date <- usm$date + (17*60*60)


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

#### Fig. 2 Time series of PM10, WS, WD, T, RH ####
png(filename='figs/fig2.png',height=16,width=16,res=360,units='cm')

par(mfrow=c(5,1),tcl=-0.5,omi=c(0.5,0,0.1,0))

# a) PM10 time series
par(mai=c(0.1,0.7,0.2,0.1))
plot(usm$date,usm$pm10,type='l',lwd=1,ylim=c(0,150), xlab= '',ylab='', 
     xaxt = 'n')
lines(s18$date,s18$pm10,lty=4,lwd=2)
mtext(side=2,expression(paste('PM10 (',mu,'g m'^'-3',')')),line=2)
lines(x=c(as.POSIXlt('2011-12-01'),as.POSIXlt('2011-12-01')),
      y=c(-10,200),lty=5,lwd=1)
lines(x=c(as.POSIXlt('2012-04-01'),as.POSIXlt('2012-04-01')),
      y=c(-10,200),lty=5,lwd=1)
lines(x=c(as.POSIXlt('2012-06-01'),as.POSIXlt('2012-06-01')),
      y=c(-10,200),lty=5,lwd=1)
lines(x=c(as.POSIXlt('2012-10-01'),as.POSIXlt('2012-10-01')),
      y=c(-10,200),lty=5,lwd=1)
lines(x=c(as.POSIXlt('2012-12-01'),as.POSIXlt('2012-12-01')),
      y=c(-10,200),lty=5,lwd=1)
mtext(side=3,'FTM',line=0.5, adj = 0.005)
mtext(side=3,'NEM',line=0.5,adj = 0.2)
mtext(side=3,'STM',line=0.5, adj=0.47)
mtext(side=3,'SWM',line=0.5,adj = 0.7)
mtext(side=3,'FTM',line=0.5, adj = 0.95)
legend('topright', legend = c('USM','S18'),
       lty = c(1, 4), lwd = c(1, 2))
text(x=as.POSIXlt('2011-11-10'),y=135,'a)',cex=1.5)
#minor.tick(nx=2, ny=2)

# b) ws time series
par(mai=c(0.1,0.7,0.1,0.1))
plot(usm$date,usm$ws,type='l',lwd=1,ylim=c(0,5), xlab= '',ylab='', 
     xaxt = 'n')
lines(s18$date,s18$ws,lty=4,lwd=2)
mtext(side=2,expression(paste('WS ','(m s'^'-1',')')),line=2)
lines(x=c(as.POSIXlt('2011-12-01'),as.POSIXlt('2011-12-01')),
      y=c(-10,200),lty=5,lwd=1)
lines(x=c(as.POSIXlt('2012-04-01'),as.POSIXlt('2012-04-01')),
      y=c(-10,200),lty=5,lwd=1)
lines(x=c(as.POSIXlt('2012-06-01'),as.POSIXlt('2012-06-01')),
      y=c(-10,200),lty=5,lwd=1)
lines(x=c(as.POSIXlt('2012-10-01'),as.POSIXlt('2012-10-01')),
      y=c(-10,200),lty=5,lwd=1)
lines(x=c(as.POSIXlt('2012-12-01'),as.POSIXlt('2012-12-01')),
      y=c(-10,200),lty=5,lwd=1)
text(x=as.POSIXlt('2011-11-10'),y=4.5,'b)',cex=1.5)
#minor.tick(nx=2, ny=2)

# c) wd time series
par(mai=c(0.1,0.7,0.1,0.1))
plot(usm$date,usm$wd,type='l',lwd=1,ylim=c(0,360), xlab= '',ylab='', 
     xaxt = 'n')
lines(s18$date,s18$wd,lty=4,lwd=2)
mtext(side=2,expression(paste('WD ','(',degree,')')),line=2)
lines(x=c(as.POSIXlt('2011-12-01'),as.POSIXlt('2011-12-01')),
      y=c(-10,400),lty=5,lwd=1)
lines(x=c(as.POSIXlt('2012-04-01'),as.POSIXlt('2012-04-01')),
      y=c(-10,400),lty=5,lwd=1)
lines(x=c(as.POSIXlt('2012-06-01'),as.POSIXlt('2012-06-01')),
      y=c(-10,400),lty=5,lwd=1)
lines(x=c(as.POSIXlt('2012-10-01'),as.POSIXlt('2012-10-01')),
      y=c(-10,400),lty=5,lwd=1)
lines(x=c(as.POSIXlt('2012-12-01'),as.POSIXlt('2012-12-01')),
      y=c(-10,400),lty=5,lwd=1)
text(x=as.POSIXlt('2011-11-10'),y=335,'c)',cex=1.5)
#minor.tick(nx=2, ny=2)

# d) T time series
par(mai=c(0.1,0.7,0.1,0.1))
plot(usm$date,usm$T,type='l',lwd=1,ylim=c(24,34), xlab= '',ylab='', 
     xaxt = 'n')
lines(s18$date,s18$T,lty=4,lwd=2)
mtext(side=2,expression(paste('T ','(',degree,'C)')),line=2)
lines(x=c(as.POSIXlt('2011-12-01'),as.POSIXlt('2011-12-01')),
      y=c(-10,400),lty=5,lwd=1)
lines(x=c(as.POSIXlt('2012-04-01'),as.POSIXlt('2012-04-01')),
      y=c(-10,400),lty=5,lwd=1)
lines(x=c(as.POSIXlt('2012-06-01'),as.POSIXlt('2012-06-01')),
      y=c(-10,400),lty=5,lwd=1)
lines(x=c(as.POSIXlt('2012-10-01'),as.POSIXlt('2012-10-01')),
      y=c(-10,400),lty=5,lwd=1)
lines(x=c(as.POSIXlt('2012-12-01'),as.POSIXlt('2012-12-01')),
      y=c(-10,400),lty=5,lwd=1)
text(x=as.POSIXlt('2011-11-10'),y=33.2,'d)',cex=1.5)
#minor.tick(nx=2, ny=2)

# e) RH time series
par(mai=c(0.1,0.7,0.1,0.1))
plot(usm$date,usm$RH,type='l',lwd=1,ylim=c(50,100), xlab= '',ylab='')
lines(s18$date,s18$RH,lty=4,lwd=2)
mtext(side=2,expression(paste('RH ','(%)')),line=2){Waked, 2014 #172}
lines(x=c(as.POSIXlt('2011-12-01'),as.POSIXlt('2011-12-01')),
      y=c(-10,400),lty=5,lwd=1)
lines(x=c(as.POSIXlt('2012-04-01'),as.POSIXlt('2012-04-01')),
      y=c(-10,400),lty=5,lwd=1)
lines(x=c(as.POSIXlt('2012-06-01'),as.POSIXlt('2012-06-01')),
      y=c(-10,400),lty=5,lwd=1)
lines(x=c(as.POSIXlt('2012-10-01'),as.POSIXlt('2012-10-01')),
      y=c(-10,400),lty=5,lwd=1)
lines(x=c(as.POSIXlt('2012-12-01'),as.POSIXlt('2012-12-01')),
      y=c(-10,400),lty=5,lwd=1)
text(x=as.POSIXlt('2011-11-10'),y=96,'e)',cex=1.5)

mtext("Date", side=1, outer=T, at=0.55,line = 2)
dev.off()

#### Fig. 3 Boxplot of PM10 in different seasons ####

png(filename='figs/fig3.png',height=16,width=8,res=180,units='cm')


par(mfrow=c(2,1),tcl=-0.5,omi=c(0.2,0,0,0))

par(mai=c(0.2,0.7,0.2,0.1))
boxplot(usm$pm10[which(s18$monsoon=='NEM')],usm$pm10[which(s18$monsoon=='STM')],
        usm$pm10[which(s18$monsoon=='SWM')],usm$pm10[which(s18$monsoon=='FTM')],
        ylim=c(0,150),names=c('','','',''),xlab='Monsoon',
        ylab='')
text('a) USM',x=1,y=148)
minor.tick(nx=0,ny=2)

par(mai=c(0.4,0.7,0,0.1))
boxplot(s18$pm10[which(s18$monsoon=='NEM')],s18$pm10[which(s18$monsoon=='STM')],
        s18$pm10[which(s18$monsoon=='SWM')],s18$pm10[which(s18$monsoon=='FTM')],
        ylim=c(0,150),names = c('NEM','STM','SWM','FTM'),ylab='')
text('b) S18',x=1,y=148)
minor.tick(nx=0,ny=2)

mtext("Monsoon", side=1, outer=T, at=0.6)
mtext(expression(paste('PM10 (',mu,'g m'^'-3',')')), side=2, outer=T, at=0.5,
      line=-1.3)

dev.off()

#### Fig. 4 Factor 1 barplot ####
fa <- read.csv('data/factor_analy.csv',sep=',',header = TRUE)
# Reclass the date variable
fa$date <- as.POSIXlt(fa$date,format="%m/%d/%y")

png(filename='figs/fig4.png',height=8,width=16,res=360,units='cm')
par(mai=c(0.8,0.6,0.4,0.2))
barplot(fa$F1,names.arg = fa$date,col='white',ylim=c(-3,1.5))
lines(x=c(89,89),
      y=c(-10,10),lty = 2, col ='black',lwd=2)
mtext(side=2,'Factor scores',line = 2)
mtext(side=1,'Date',line = 2.5)
mtext(side=3,'S18',line=0.5, at = 45)
mtext(side=3,'USM',line=0.5,at = 135)

box()
minor.tick(ny=2,nx=10)
dev.off()

#### Fig. 5 TrajPlot for S18 on 2012-06-20 ####
d1 <- selectByDate(trajS18_2012_96h,start='20/6/2012',end='20/6/2012')
d2 <- selectByDate(trajS18_2012_96h,start='11/8/2012',end='11/8/2012')
d3 <- selectByDate(trajUSM_2012_96h,start='29/2/2012',end='29/2/2012')
d4 <- selectByDate(trajUSM_2012_96h,start='25/4/2012',end='25/4/2012')
d <- rbind(d1,d2,d3,d4)
d$day <- as.Date(d$date)

png(filename='figs/fig5.png',height=16,width=16,res=400,units='cm')
trajPlot(d,
         xlim=c(89,106),ylim=c(-4,13),
         map.res = 'hires', projection = 'mercator',parameters=NULL,
         grid.col = 'transparent',lwd=2,type='day',layout=c(2,2),fontsize=12,
         map.alpha = 0.6,origin=TRUE)
dev.off()

rm(d1,d2,d3,d4,d)

#### Fig. 6 Factor 1: Mn-Fe-Cd ####
png(filename='figs/factor1.png',height=17,width=8,res=360,units='cm')

par(mfrow=c(3,1),tcl=-0.5,omi=c(0.6,0,0.1,0))

# Mn
par(mai=c(0.1,0.7,0.2,0.1))
plot(usm$date,usm$Mn,type='l',lwd=1, xlab= '',ylab='',cex.axis=1, 
     xaxt = 'n')
lines(s18$date,s18$Mn,lty=4,lwd=2)
mtext(side=2,expression(paste('Mn (g m'^'-3',')')),line=2)
lines(x=c(as.POSIXlt('2011-12-01'),as.POSIXlt('2011-12-01')),
      y=c(-10,200),lty=5,lwd=2)
lines(x=c(as.POSIXlt('2012-04-01'),as.POSIXlt('2012-04-01')),
      y=c(-10,200),lty=5,lwd=2)
lines(x=c(as.POSIXlt('2012-06-01'),as.POSIXlt('2012-06-01')),
      y=c(-10,200),lty=5,lwd=2)
lines(x=c(as.POSIXlt('2012-10-01'),as.POSIXlt('2012-10-01')),
      y=c(-10,200),lty=5,lwd=2)
lines(x=c(as.POSIXlt('2012-12-01'),as.POSIXlt('2012-12-01')),
      y=c(-10,200),lty=5,lwd=2)
mtext(side=3,'FTM',line=0.5, adj = 0.005,cex=0.8)
mtext(side=3,'NEM',line=0.5,adj = 0.2,cex=0.8)
mtext(side=3,'STM',line=0.5, adj=0.47,cex=0.8)
mtext(side=3,'SWM',line=0.5,adj = 0.7,cex=0.8)
mtext(side=3,'FTM',line=0.5, adj = 0.95,cex=0.8)
legend('topright', legend = c('USM','S18'),
       lty = c(1, 4), lwd = c(1, 2),cex=0.8)
text(x=as.POSIXlt('2011-11-18'),y=0.00175,'a)',cex=1.4)

# Fe
par(mai=c(0.15,0.7,0.15,0.1))
plot(usm$date,usm$Fe,type='l',lwd=1, xlab= '',ylab='',cex.axis=1,
     xaxt='n')
lines(s18$date,s18$Fe,lty=4,lwd=2)
mtext(side=2,expression(paste('Fe (g m'^'-3',')')),line=2)
lines(x=c(as.POSIXlt('2011-12-01'),as.POSIXlt('2011-12-01')),
      y=c(-10,200),lty=5,lwd=2)
lines(x=c(as.POSIXlt('2012-04-01'),as.POSIXlt('2012-04-01')),
      y=c(-10,200),lty=5,lwd=2)
lines(x=c(as.POSIXlt('2012-06-01'),as.POSIXlt('2012-06-01')),
      y=c(-10,200),lty=5,lwd=2)
lines(x=c(as.POSIXlt('2012-10-01'),as.POSIXlt('2012-10-01')),
      y=c(-10,200),lty=5,lwd=2)
lines(x=c(as.POSIXlt('2012-12-01'),as.POSIXlt('2012-12-01')),
      y=c(-10,200),lty=5,lwd=2)
text(x=as.POSIXlt('2011-11-18'),y=0.024,'b)',cex=1.4)
# Cd
par(mai=c(0.1,0.7,0.18,0.1))
plot(usm$date,usm$Cd,type='l',lwd=1, xlab= '',ylab='',cex.axis=1)
lines(s18$date,s18$Cd,lty=4,lwd=2)
mtext(side=2,expression(paste('Cd (g m'^'-3',')')),line=2)
lines(x=c(as.POSIXlt('2011-12-01'),as.POSIXlt('2011-12-01')),
      y=c(-10,200),lty=5,lwd=2)
lines(x=c(as.POSIXlt('2012-04-01'),as.POSIXlt('2012-04-01')),
      y=c(-10,200),lty=5,lwd=2)
lines(x=c(as.POSIXlt('2012-06-01'),as.POSIXlt('2012-06-01')),
      y=c(-10,200),lty=5,lwd=2)
lines(x=c(as.POSIXlt('2012-10-01'),as.POSIXlt('2012-10-01')),
      y=c(-10,200),lty=5,lwd=2)
lines(x=c(as.POSIXlt('2012-12-01'),as.POSIXlt('2012-12-01')),
      y=c(-10,200),lty=5,lwd=2)

text(x=as.POSIXlt('2011-11-18'),y=0.000285,'c)',cex=1.4)

mtext("Date", side=1, outer=T, at=0.55,line = 2)
dev.off()

#### Fig. 7 Factor 2: Ca-Pb ####
png(filename='figs/factor2.png',height=11,width=8,res=360,units='cm')

par(mfrow=c(2,1),tcl=-0.5,omi=c(0.6,0,0.1,0))

# Ca
par(mai=c(0.1,0.7,0.2,0.1))
plot(usm$date,usm$Ca,type='l',lwd=1, xlab= '',ylab='',cex.axis=0.7, 
     xaxt = 'n')
lines(s18$date,s18$Ca,lty=4,lwd=2)
mtext(side=2,expression(paste('Ca (g m'^'-3',')')),line=2)
lines(x=c(as.POSIXlt('2011-12-01'),as.POSIXlt('2011-12-01')),
      y=c(-10,200),lty=5,lwd=2)
lines(x=c(as.POSIXlt('2012-04-01'),as.POSIXlt('2012-04-01')),
      y=c(-10,200),lty=5,lwd=2)
lines(x=c(as.POSIXlt('2012-06-01'),as.POSIXlt('2012-06-01')),
      y=c(-10,200),lty=5,lwd=2)
lines(x=c(as.POSIXlt('2012-10-01'),as.POSIXlt('2012-10-01')),
      y=c(-10,200),lty=5,lwd=2)
lines(x=c(as.POSIXlt('2012-12-01'),as.POSIXlt('2012-12-01')),
      y=c(-10,200),lty=5,lwd=2)
mtext(side=3,'FTM',line=0.5, adj = 0.005,cex=0.7)
mtext(side=3,'NEM',line=0.5,adj = 0.2,cex=0.7)
mtext(side=3,'STM',line=0.5, adj=0.47,cex=0.7)
mtext(side=3,'SWM',line=0.5,adj = 0.7,cex=0.7)
mtext(side=3,'FTM',line=0.5, adj = 0.95,cex=0.7)
legend('topright', legend = c('USM','S18'),
       lty = c(1, 4), lwd = c(1, 2),cex=0.7)
text(x=as.POSIXlt('2011-11-18'),y=0.0445,'a)',cex=0.8)

# Pb
par(mai=c(0.1,0.7,0.15,0.1))
plot(usm$date,usm$Pb,type='l',lwd=1, xlab= '',ylab='',cex.axis=0.7)
lines(s18$date,s18$Pb,lty=4,lwd=2)
mtext(side=2,expression(paste('Pb (g m'^'-3',')')),line=2)
lines(x=c(as.POSIXlt('2011-12-01'),as.POSIXlt('2011-12-01')),
      y=c(-10,200),lty=5,lwd=2)
lines(x=c(as.POSIXlt('2012-04-01'),as.POSIXlt('2012-04-01')),
      y=c(-10,200),lty=5,lwd=2)
lines(x=c(as.POSIXlt('2012-06-01'),as.POSIXlt('2012-06-01')),
      y=c(-10,200),lty=5,lwd=2)
lines(x=c(as.POSIXlt('2012-10-01'),as.POSIXlt('2012-10-01')),
      y=c(-10,200),lty=5,lwd=2)
lines(x=c(as.POSIXlt('2012-12-01'),as.POSIXlt('2012-12-01')),
      y=c(-10,200),lty=5,lwd=2)

text(x=as.POSIXlt('2011-11-18'),y=0.0031,'b)',cex=0.8)

mtext("Date", side=1, outer=T, at=0.55,line = 2)
dev.off()

#### Fig. 8 Factor 3: Ni-Zn ####
png(filename='figs/factor3.png',height=11,width=8,res=360,units='cm')

par(mfrow=c(2,1),tcl=-0.5,omi=c(0.6,0,0.1,0))

# Ni
par(mai=c(0.1,0.7,0.2,0.1))
plot(usm$date,usm$Ni,type='l',lwd=1, xlab= '',ylab='',cex.axis=0.7, 
     xaxt = 'n')
lines(s18$date,s18$Ni,lty=4,lwd=2)
mtext(side=2,expression(paste('Ni (g m'^'-3',')')),line=2)
lines(x=c(as.POSIXlt('2011-12-01'),as.POSIXlt('2011-12-01')),
      y=c(-10,200),lty=5,lwd=2)
lines(x=c(as.POSIXlt('2012-04-01'),as.POSIXlt('2012-04-01')),
      y=c(-10,200),lty=5,lwd=2)
lines(x=c(as.POSIXlt('2012-06-01'),as.POSIXlt('2012-06-01')),
      y=c(-10,200),lty=5,lwd=2)
lines(x=c(as.POSIXlt('2012-10-01'),as.POSIXlt('2012-10-01')),
      y=c(-10,200),lty=5,lwd=2)
lines(x=c(as.POSIXlt('2012-12-01'),as.POSIXlt('2012-12-01')),
      y=c(-10,200),lty=5,lwd=2)
mtext(side=3,'FTM',line=0.5, adj = 0.005,cex=0.7)
mtext(side=3,'NEM',line=0.5,adj = 0.2,cex=0.7)
mtext(side=3,'STM',line=0.5, adj=0.47,cex=0.7)
mtext(side=3,'SWM',line=0.5,adj = 0.7,cex=0.7)
mtext(side=3,'FTM',line=0.5, adj = 0.95,cex=0.7)
legend('topright', legend = c('USM','S18'),
       lty = c(1, 4), lwd = c(1, 2),cex=0.7)
text(x=as.POSIXlt('2011-11-18'),y=0.00049,'a)',cex=0.8)

# Zn
par(mai=c(0.1,0.7,0.15,0.1))
plot(usm$date,usm$Zn,type='l',lwd=1, xlab= '',ylab='',cex.axis=0.7)
lines(s18$date,s18$Zn,lty=4,lwd=2)
mtext(side=2,expression(paste('Zn (g m'^'-3',')')),line=2)
lines(x=c(as.POSIXlt('2011-12-01'),as.POSIXlt('2011-12-01')),
      y=c(-10,200),lty=5,lwd=2)
lines(x=c(as.POSIXlt('2012-04-01'),as.POSIXlt('2012-04-01')),
      y=c(-10,200),lty=5,lwd=2)
lines(x=c(as.POSIXlt('2012-06-01'),as.POSIXlt('2012-06-01')),
      y=c(-10,200),lty=5,lwd=2)
lines(x=c(as.POSIXlt('2012-10-01'),as.POSIXlt('2012-10-01')),
      y=c(-10,200),lty=5,lwd=2)
lines(x=c(as.POSIXlt('2012-12-01'),as.POSIXlt('2012-12-01')),
      y=c(-10,200),lty=5,lwd=2)

text(x=as.POSIXlt('2011-11-18'),y=0.0084,'b)',cex=0.8)

mtext("Date", side=1, outer=T, at=0.55,line = 2)
dev.off()

#### Fig. 9 Factor 4: Cu-Mg ####
png(filename='figs/factor4.png',height=11,width=8,res=360,units='cm')

par(mfrow=c(2,1),tcl=-0.5,omi=c(0.6,0,0.1,0))

# Cu
par(mai=c(0.1,0.7,0.2,0.1))
plot(usm$date,usm$Cu,type='l',lwd=1, xlab= '',ylab='',cex.axis=0.7, 
     xaxt = 'n')
lines(s18$date,s18$Cu,lty=4,lwd=2)
mtext(side=2,expression(paste('Cu (g m'^'-3',')')),line=2)
lines(x=c(as.POSIXlt('2011-12-01'),as.POSIXlt('2011-12-01')),
      y=c(-10,200),lty=5,lwd=2)
lines(x=c(as.POSIXlt('2012-04-01'),as.POSIXlt('2012-04-01')),
      y=c(-10,200),lty=5,lwd=2)
lines(x=c(as.POSIXlt('2012-06-01'),as.POSIXlt('2012-06-01')),
      y=c(-10,200),lty=5,lwd=2)
lines(x=c(as.POSIXlt('2012-10-01'),as.POSIXlt('2012-10-01')),
      y=c(-10,200),lty=5,lwd=2)
lines(x=c(as.POSIXlt('2012-12-01'),as.POSIXlt('2012-12-01')),
      y=c(-10,200),lty=5,lwd=2)
mtext(side=3,'FTM',line=0.5, adj = 0.005,cex=0.7)
mtext(side=3,'NEM',line=0.5,adj = 0.2,cex=0.7)
mtext(side=3,'STM',line=0.5, adj=0.47,cex=0.7)
mtext(side=3,'SWM',line=0.5,adj = 0.7,cex=0.7)
mtext(side=3,'FTM',line=0.5, adj = 0.95,cex=0.7)
legend('topright', legend = c('USM','S18'),
       lty = c(1, 4), lwd = c(1, 2),cex=0.7)
text(x=as.POSIXlt('2011-11-18'),y=0.0043,'a)',cex=0.8)

# Mg
par(mai=c(0.1,0.7,0.15,0.1))
plot(usm$date,usm$Mg,type='l',lwd=1, xlab= '',ylab='',cex.axis=0.7)
lines(s18$date,s18$Mg,lty=4,lwd=2)
mtext(side=2,expression(paste('Mg (g m'^'-3',')')),line=2)
lines(x=c(as.POSIXlt('2011-12-01'),as.POSIXlt('2011-12-01')),
      y=c(-10,200),lty=5,lwd=2)
lines(x=c(as.POSIXlt('2012-04-01'),as.POSIXlt('2012-04-01')),
      y=c(-10,200),lty=5,lwd=2)
lines(x=c(as.POSIXlt('2012-06-01'),as.POSIXlt('2012-06-01')),
      y=c(-10,200),lty=5,lwd=2)
lines(x=c(as.POSIXlt('2012-10-01'),as.POSIXlt('2012-10-01')),
      y=c(-10,200),lty=5,lwd=2)
lines(x=c(as.POSIXlt('2012-12-01'),as.POSIXlt('2012-12-01')),
      y=c(-10,200),lty=5,lwd=2)

text(x=as.POSIXlt('2011-11-18'),y=0.085,'b)',cex=0.8)

mtext("Date", side=1, outer=T, at=0.55,line = 2)
dev.off()

#### Fig. 10 Factors (1-4) for metals barplot ####

png(filename='figs/fig10.png',height=16,width=16,res=360,units='cm')
par(mai=c(0.8,0.6,0.4,0.2),mfrow=c(4,1))

# a) Factor 1 Mn-Fe-Cd
par(mai=c(0.1,0.5,0.32,0.1))
barplot(fa$F1.1,names.arg = fa$date,col='white',
        ylim=c(min(fa$F1.1)-0.5,max(fa$F1.1))+0.5, xaxt='n')
lines(x=c(89,89),
      y=c(-10,10),lty = 2, col ='black',lwd=2)
mtext(side=3,'S18',line=0.5, at = 45)
mtext(side=3,'USM',line=0.5,at = 135)
text(x=0,y=max(fa$F1.1) - (0.1*max(fa$F1.1)),'a)',
     cex=2)
box()
minor.tick(ny=2,nx=10)

# b) Factor 2 Pb-Ca
par(mai=c(0.2,0.5,0.2,0.1))
barplot(fa$F2,names.arg = fa$date,col='white',
        ylim=c(min(fa$F2)-0.5,max(fa$F2))+0.5, xaxt='n')
lines(x=c(89,89),
      y=c(-10,10),lty = 2, col ='black',lwd=2)
mtext(side=2,'Factor scores',line = 2, at = -2)
text(x=0,y=max(fa$F2) - (0.1*max(fa$F2)),'b)',
     cex=2)
axis(side=2,at=0)
box()
minor.tick(ny=2,nx=10)


# c) Factor 3 Zn-Ni
par(mai=c(0.3,0.5,0.1,0.1))
barplot(fa$F3,names.arg = fa$date,col='white',
        ylim=c(min(fa$F3)-0.5,max(fa$F3))+0.5, xaxt = 'n')
lines(x=c(89,89),
      y=c(-10,10),lty = 2, col ='black',lwd=2)
text(x=0,y=max(fa$F3) - (0.1*max(fa$F3)),'c)',
     cex=2)
box()
minor.tick(ny=2,nx=10)

# d) Factor 4 Mg-Cu
par(mai=c(0.5,0.5,0,0.1))
barplot(fa$F4,names.arg = fa$date,col='white',
        ylim=c(min(fa$F4)-0.5,max(fa$F4))+0.5)
lines(x=c(89,89),
      y=c(-10,10),lty = 2, col ='black',lwd=2)
mtext(side=1,'Date',line = 2.5)
text(x=0,y=max(fa$F4) - (0.1*max(fa$F4)),'d)',
     cex=2)
box()
minor.tick(ny=2,nx=10)

dev.off()

#### Importing only factors into S18 and USM dataframes ####

# USM 
fa_usmf1 <- fa$F1[79:159]
fa_usmf11 <- fa$F1.1[79:159]
fa_usmf2 <- fa$F2[79:159]
fa_usmf3 <- fa$F3[79:159]
fa_usmf4 <- fa$F4[79:159]
fa_usm <- data.frame(f_met=fa_usmf1,f_metal1=fa_usmf11,f_metal2=fa_usmf2,
                     f_metal3=fa_usmf3,f_metal4=fa_usmf4)
usm <- cbind(usm,fa_usm)
rm(fa_usmf1,fa_usmf11,fa_usmf2,fa_usmf3,fa_usmf4,fa_usm)

# S18
fa_s18f1 <- fa$F1[1:78]
fa_s18f11 <- fa$F1.1[1:78]
fa_s18f2 <- fa$F2[1:78]
fa_s18f3 <- fa$F3[1:78]
fa_s18f4 <- fa$F4[1:78]
fa_s18 <- data.frame(f_met=fa_s18f1,f_metal1=fa_s18f11,f_metal2=fa_s18f2,
                     f_metal3=fa_s18f3,f_metal4=fa_s18f4)
s18 <- cbind(s18,fa_s18)
rm(fa_s18f1,fa_s18f11,fa_s18f2,fa_s18f3,fa_s18f4,fa_s18)

#### Gridded trajectory plot (24 h) for USM and PM10 ####
png(filename='figs/trajUSMpm10.png',height=16,width=16,res=400,units='cm')
trajLevel(usmt,
          #xlim=c(94,106),ylim=c(-1,11),
          pollutant='pm10',statistic='difference',projection='mercator',
          parameters = NULL,border=NA,map.res = 'hires',type='monsoon',col='increment',
          layout=c(2,2),key.header='USM: PM10',map.alpha=0.6,grid.col = 'transparent',
          origin=TRUE, fontsize = 16)
dev.off()

#### Gridded trajectory plot (24 h) for S18 and PM10 ####
png(filename='figs/trajS18pm10.png',height=16,width=16,res=400,units='cm')
trajLevel(s18t,
          xlim=c(94,106),ylim=c(-1,11),
          pollutant='pm10',statistic='difference',projection='mercator',
          parameters = NULL,border=NA,map.res = 'hires',type='monsoon',col='increment',
          layout=c(2,2),key.header='S18: PM10',map.alpha=0.6,grid.col = 'transparent',
          origin=TRUE, fontsize = 16)
dev.off()

#### Gridded trajectory plot (24 h) for USM and Mn-Fe-Cd ####
png(filename='figs/trajUSMF1.png',height=16,width=16,res=400,units='cm')
trajLevel(usmt,
          xlim=c(94,106),ylim=c(-1,11),
          pollutant='f_metal1',statistic='difference',projection='mercator',
          parameters = NULL,border=NA,map.res = 'hires',type='monsoon',col='increment',
          layout=c(2,2),key.header='USM: Mn-Fe-Cd',map.alpha=0.6,grid.col = 'transparent',
          origin=TRUE, fontsize = 16)
dev.off()

#### Gridded trajectory plot (24 h) for S18 and Mn-Fe-Cd ####
png(filename='figs/trajS18F1.png',height=16,width=16,res=400,units='cm')
trajLevel(s18t,
          xlim=c(94,106),ylim=c(-1,11),
          pollutant='f_metal1',statistic='difference',projection='mercator',
          parameters = NULL,border=NA,map.res = 'hires',type='monsoon',col='increment',
          layout=c(2,2),key.header='S18: Mn-Fe-Cd',map.alpha=0.6,grid.col = 'transparent',
          origin=TRUE, fontsize = 16)
dev.off()

#### Gridded trajectory plot (24 h) for USM and Pb-Ca ####
png(filename='figs/trajUSMF2.png',height=16,width=16,res=400,units='cm')
trajLevel(usmt,
          xlim=c(94,106),ylim=c(-1,11),
          pollutant='f_metal2',statistic='difference',projection='mercator',
          parameters = NULL,border=NA,map.res = 'hires',type='monsoon',col='increment',
          layout=c(2,2),key.header='USM: Pb-Ca',map.alpha=0.6,grid.col = 'transparent',
          origin=TRUE, fontsize = 16)
dev.off()

#### Gridded trajectory plot (24 h) for S18 and Pb-Ca ####
png(filename='figs/trajS18F2.png',height=16,width=16,res=400,units='cm')
trajLevel(s18t,
          xlim=c(94,106),ylim=c(-1,11),
          pollutant='f_metal2',statistic='difference',projection='mercator',
          parameters = NULL,border=NA,map.res = 'hires',type='monsoon',col='increment',
          layout=c(2,2),key.header='S18: Pb-Ca',map.alpha=0.6,grid.col = 'transparent',
          origin = TRUE, fontsize = 16)
dev.off()

#### Gridded trajectory plot (24 h) for USM and Ni-Zn ####
png(filename='figs/trajUSMF3.png',height=16,width=16,res=400,units='cm')
trajLevel(usmt,
          xlim=c(94,106),ylim=c(-1,11),
          pollutant='f_metal3',statistic='difference',projection='mercator',
          parameters = NULL,border=NA,map.res = 'hires',type='monsoon',col='increment',
          layout=c(2,2),key.header='USM: Ni-Zn',map.alpha=0.6,grid.col = 'transparent',
          origin = TRUE, fontsize = 16)
dev.off()

#### Gridded trajectory plot (24 h) for S18 and Ni-Zn ####
png(filename='figs/trajS18F3.png',height=16,width=16,res=400,units='cm')
trajLevel(s18t,
          xlim=c(94,106),ylim=c(-1,11),
          pollutant='f_metal3',statistic='difference',projection='mercator',
          parameters = NULL,border=NA,map.res = 'hires',type='monsoon',col='increment',
          layout=c(2,2),key.header='S18: Ni-Zn',map.alpha=0.6,grid.col = 'transparent',
          origin = TRUE, fontsize = 16)
dev.off()

#### Gridded trajectory plot (24 h) for USM and Cu-Mg ####
png(filename='figs/trajUSMF4.png',height=16,width=16,res=400,units='cm')
trajLevel(usmt,
          xlim=c(94,106),ylim=c(-1,11),
          pollutant='f_metal4',statistic='difference',projection='mercator',
          parameters = NULL,border=NA,map.res = 'hires',type='monsoon',col='increment',
          layout=c(2,2),key.header='USM: Cu-Mg',map.alpha=0.6,grid.col = 'transparent',
          origin = TRUE, fontsize = 16)
dev.off()

#### Gridded trajectory plot (24 h) for S18 and Cu-Mg ####
png(filename='figs/trajS18F4.png',height=16,width=16,res=400,units='cm')
trajLevel(s18t,
          xlim=c(94,106),ylim=c(-1,11),
          pollutant='f_metal4',statistic='difference',projection='mercator',
          parameters = NULL,border=NA,map.res = 'hires',type='monsoon',col='increment',
          layout=c(2,2),key.header='S18: Cu-Mg',map.alpha=0.6,grid.col = 'transparent',
          origin = TRUE, fontsize = 16)
dev.off()

#### To determine the conc. of metals during haze ####

s18[which(s18$date == as.POSIXlt('2012-06-20 17:00:00')),19:28]
s18[which(s18$date == as.POSIXlt('2012-06-14 17:00:00')),19:28]
s18[which(s18$date == as.POSIXlt('2012-06-16 17:00:00')),19:28]

plot(s18$date[30:60],s18$Fe[30:60],type='l')

plot(s18$date[30:60],s18$Mg[30:60],type='l')
plot(s18$date[30:60],s18$Cu[30:60],type='l')
plot(s18$date[30:60],s18$f_metal4[30:60],type='l')

usm[which(s18$date == as.POSIXlt('2012-08-11 17:00:00')),19:28]

plot(usm$date[50:70],usm$Cu[50:70],type='l')
plot(usm$date[50:70],usm$Mg[50:70],type='l')
plot(usm$date[50:70],usm$Cu[50:70],type='l')
plot(usm$date[50:70],usm$f_metal4[50:70],type='l')
