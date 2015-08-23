# Plot map script

# Loading packages
library(openair)
library(lattice)



# Plot the map

## Input coordinates of stations: S18 and USM
siteDetails <- data.frame(site=c('S18','USM'),latitude=c(5.401581,5.357728),
                          longitude=c(100.334703,100.301739))

## Plot the map

## Change the margins of the lattice plots
#lw <- list(left.padding = list(x = 0, units = "inches")) 
#lw$right.padding <- list(x = 0, units = "inches") 
#lh <- list(bottom.padding = list(x = 0, units = "inches")) 
#lh$top.padding <- list(x = 0, units = "inches")
#lattice.options(layout.widths = lw, layout.heights = lh)

### Zoomed in map showing the stations

# (a)

png(filename = "figs/maps/penang1.jpeg",height=8,width=8,
    bg = "white",units='cm', res = 360, family = "")
GoogleMapsPlot(siteDetails,lat='latitude',long='longitude',maptype='roadmap',
               size=c(640,640),labels='site',col='black')
dev.off()

### Zoomed out map showing the region
# (b)
png(filename = "figs/maps/penang2.jpeg",height=8,width=8,
    bg = "white",units='cm', res = 360, family = "")
GoogleMapsPlot(siteDetails,lat='latitude',long='longitude',maptype='roadmap',
               size=c(640,640),col='black',cex=1,
               xlim=c(100.1,100.5), ylim=c(5.2,5.6))
dev.off()