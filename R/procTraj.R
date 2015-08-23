## Function to read and combine files, run getMet() first to download met data
## Adapted from Carslaw, D. (2015) The openair manual, appendix D
## Note: you need to specify the location of the installed Hysplit software
## and other relevant folders needed commented below
## Input: lat, lon, year, name, met, out, hours, height
## E.g. hours = 96 (4 days)
##
## Author: Yusri Yusup, PhD
## Date created: 2015-08-04

procTraj <- function(lat = 51.5, lon = -0.1, year = 2010, name = "london",
                     met = "/TrajData", out = "/TrajProc", hours = 96, height = 10) {
  ## hours is the back trajectory time e.g. 96 = 4-day back trajectory
  ## height is start height (m)
  library(openair)
  library(plyr)
  library(reshape2)
  ## function to run 12 months of trajectories
  ## assumes 96 hour back trajectories, 1 receptor
  setwd("/Users/Yusri/Documents/Hysplit4_dist/working/") ## change here depending
  ## on the installation folder of Hysplit in your PC
  
  ## remove existing "tdump" files
  path.files <- "/Users/Yusri/Documents/Hysplit4_dis/working/" ## change here depending
  ## on the installation folder of Hysplit in your PC
  bat.file <- "/Users/Yusri/Documents/Hysplit4_dist/working/test.sh" ## name of BAT file to add to/run
  ## change here depending
  ## on the installation folder of Hysplit in your PC
  
  files <- list.files(path = path.files, pattern = "tdump")
  lapply(files, function(x) file.remove(x))
  
  start <- paste(year, "-01-01", sep = "")
  end <- paste(year, "-12-31 18:00", sep = "")
  dates <- seq(as.POSIXct(start, "GMT"), as.POSIXct(end, "GMT"), by = "3 hour")
  
  for (i in 1:length(dates)) {
    
    year <- format(dates[i], "%y")
    Year <- format(dates[i], "%Y") # long format
    month <- format(dates[i], "%m")
    day <- format(dates[i], "%d")
    hour <- format(dates[i], "%H")
    
    x <- paste("echo", year, month, day, hour, "        >CONTROL")
    write.table(x, bat.file, col.names = FALSE,
                row.names = FALSE, quote = FALSE)
    
    x <-  "echo 1                 >>CONTROL"
    write.table(x, bat.file, col.names = FALSE,
                row.names = FALSE, quote = FALSE, append = TRUE)
    x <- paste("echo", lat, lon, height, "     >>CONTROL")
    
    write.table(x, bat.file, col.names = FALSE,
                row.names = FALSE, quote = FALSE, append = TRUE)
    
    x <- paste("echo ", "-", hours, "               >>CONTROL", sep = "")
    write.table(x, bat.file, col.names = FALSE,
                row.names = FALSE, quote = FALSE, append = TRUE)
    
    x <- "echo 0                 >>CONTROL
          echo 10000.0           >>CONTROL
          echo 3                 >>CONTROL"
    
    write.table(x, bat.file, col.names = FALSE,
                row.names = FALSE, quote = FALSE, append = TRUE)
    
    add.met <- function(month, Year) {
      
      ## if month is one, need previous year and month = 12
      if (month == 0) {
        month <- 12
        Year <- as.numeric(Year) - 1
      }
      
      if (month < 10) month <- paste("0", month, sep = "")
      ## add first line
      
      write.table(paste("echo", met, "      >>CONTROL"),
                  bat.file, col.names = FALSE,
                  row.names = FALSE, quote = FALSE, append = TRUE)
      
      x <- paste("echo RP", Year, month, ".gbl       >>CONTROL", sep = "")
      write.table(x, bat.file, col.names = FALSE,
                  row.names = FALSE, quote = FALSE, append = TRUE)
    }
    
    ## processing always assumes 3 months of met for consistent tdump files
    months <- as.numeric(unique(format(dates[i], "%m")))
    months <- c(months, months + 1:2)
    months <- months - 1 ## to make sure we get the start of the previous year
    months <- months[months <= 12]
    if (length(months) == 2) months <- c(min(months) - 1, months)
    
    for (i in 1:3)
      add.met(months[i], Year)
    
    x <-  "echo ./                >>CONTROL"
    
    write.table(x, bat.file, col.names = FALSE,
                row.names = FALSE, quote = FALSE, append = TRUE)
    
    x <- paste("echo tdump", year, month, day, hour, "          >>CONTROL", sep = "")
    
    write.table(x, bat.file, col.names = FALSE,
                row.names = FALSE, quote = FALSE, append = TRUE)
    
    system('sh test.sh')
    write.table(x, bat.file, col.names = FALSE,
                row.names = FALSE, quote = FALSE, append = TRUE)
    
    ## run the file
    system('../exec/hyts_std')
    
    
  }
  
  ## combine files and make data frame
  
  traj <- read.files(hours)
  
  ## write R object to file
  file.name <- paste(out, name, Year, ".RData", sep = "")
  save(traj, file = file.name)
}
