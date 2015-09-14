## Download met data from NOAA (National Oceanic and Atmospheric Administration)
## for Hysplit runs. Run this function first before running procTraj().
## Note: Depending on the duration, the downloaded files will be large (1 month
## is 120 MB)
## Inputs: year, month, and path_met (the target folder of the downloaded files)
## E.g., year = 2013, month = 1, path_met = './TrajData/'
## year = 2012:2014, month=1:12, path_met='./TrajData/'
## Adapted from Carslaw, D. (2015) The openair manual, appendix D
##
## Author: Yusri Yusup, PhD
## Date created: 2015-08-04

getMet <- function(year = 2013,month = 1, 
                   path_met = '/Users/Yusri/Documents/Work/Data_analysis/pm10/TrajData/') {
  for (i in seq_along(year)) {
    for (j in seq_along(month)) {
      download.file(url=paste0('ftp://arlftp.arlhq.noaa.gov/pub/archives/reanalysis/RP',
                               year[i],sprintf('%02d',month[j]),'.gbl'),
                    destfile=paste0(path_met,'RP',year[i],sprintf('%02d',month[j]),'.gbl'),
                    mode='wb',method = 'curl')
    }
  }
}