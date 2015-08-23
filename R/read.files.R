## Function to read and combine files, used within the procTraj() function
## Adapted from Carslaw, D. (2015) The openair manual, appendix D
##
## Input: hours
## E.g. hours = 96 (4 days)
##
## Author: Yusri Yusup, PhD
## Date created: 2015-08-04

read.files <- function(hours = 96) {
  ## find tdump files
  files <- Sys.glob("tdump*")
  output <- file('Rcombined.txt', 'w')
  
  ## read through them all, ignoring 1st 7 lines
  for (i in files){
    input <- readLines(i)
    input <- input[-c(1:7)]  # delete header
    writeLines(input, output)
  }
  close(output)
  
  ## read the combined txt file
  traj <- read.table("/Users/Yusri/Documents/Hysplit4_dist/working/Rcombined.txt", 
                     header = FALSE)
  traj <- subset(traj, select = -c(V2, V7, V8))
  
  traj <- rename(traj, c(V1 = "receptor", V3 = "year", V4 = "month", V5 = "day",
                         V6 = "hour", V9 = "hour.inc", V10 = "lat", V11 = "lon",
                         V12 = "height", V13 = "pressure"))
  
  ## hysplit uses 2-digit years ...
  year <- traj$year[1]
  if (year < 50) traj$year <- traj$year + 2000 else traj$year <- traj$year + 1900
  
  traj$date2 <- with(traj, ISOdatetime(year, month, day, hour,  min = 0, sec = 0,
                                       tz = "GMT"))
  ## arrival time
  traj$date <- traj$date2 - 3600 * traj$hour.inc
  traj
}
