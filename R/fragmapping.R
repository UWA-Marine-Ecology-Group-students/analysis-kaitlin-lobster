###
# Project: Seagrass as habitat for juvenile lobsters
# Data:    Seagrass map
# Task:    Map fragmentation across the area
# author:  Kingsley Griffin
# date:    Aug 2022
#

library(rgeos)
library(raster)
library(landscapemetrics)

#load in raster image
#have merges 7 and 1 into 1 - "vegetation", and 3 and 5 as 3 - "no vegetation". 2 remains as "partial vegetation"
sgrass <- raster("data/seagrass_reclass.tif")
plot(sgrass)

# fix classes into just seagrass or not
sgrass[sgrass == 3] <- NA
sgrass[sgrass > 0] <- 1
plot(sgrass)

# aggregate raster to increase speed of fragstats calculation
# you may want to increase this further, and maybe increase the buffer size in the loop below, too.
sgrass <- aggregate(sgrass, 40, fun = sum)
res(sgrass)  # check the dimensions of the new cells
plot(sgrass/1600)  # plot the proportion of the cells filled with seagrass (1600 being full coverage at this resolution)

