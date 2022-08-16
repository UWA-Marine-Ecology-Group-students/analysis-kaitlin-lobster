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

########################
#         2019         #
########################

#load in raster image
#have merges 7 and 1 into 1 - "vegetation", and 3 and 5 as 3 - "no vegetation". 2 remains as "partial vegetation"
sgrass <- raster("data/seagrass_reclass2019.tif")
plot(sgrass)[]

# fix classes into just seagrass or not
sgrass[sgrass == 3] <- NA
sgrass[sgrass > 0] <- 1
plot(sgrass)

# aggregate raster to increase speed of fragstats calculation
# you may want to increase this further, and maybe increase the buffer size in the loop below, too.
sgrass <- aggregate(sgrass, 40, fun = sum)
res(sgrass)  # check the dimensions of the new cells
heatmap <- (sgrass/1600)

#Specify colour ramp
library(RColorBrewer)
my.palette <- brewer.pal(n = 9, name = "OrRd")
plot(heatmap, col = rev(my.palette)) # plot the proportion of the cells filled with seagrass (1600 being full coverage at this resolution)


########################
#         2016         #
########################

#load in raster image
#have merged 7 and 1 into 1 - "vegetation", and 3 and 5 as 3 - "no vegetation". 2 remains as "partial vegetation"
sgrass2016 <- raster("data/seagrass_reclass2016.tif")
plot(sgrass2016)[]

# fix classes into just seagrass or not
sgrass2016[sgrass2016 == 3] <- NA
sgrass2016[sgrass2016 > 0] <- 1
plot(sgrass2016)

# aggregate raster to increase speed of fragstats calculation
# you may want to increase this further, and maybe increase the buffer size in the loop below, too.
sgrass2016 <- aggregate(sgrass2016, 40, fun = sum)
res(sgrass2016)  # check the dimensions of the new cells
heatmap2016 <- (sgrass2016/1600)

#Specify colour ramp
library(RColorBrewer)
my.palette2016 <- brewer.pal(n = 9, name = "OrRd")
plot(heatmap2016, col = rev(my.palette2016)) # plot the proportion of the cells filled with seagrass (1600 being full coverage at this resolution)


########################
#         2010         #
########################

#load in raster image
#have merged 7 and 1 into 1 - "vegetation", and 3 and 5 as 3 - "no vegetation". 2 remains as "partial vegetation"
sgrass2010 <- raster("data/seagrass_reclass2010.tif")
plot(sgrass2010)[]

# fix classes into just seagrass or not
sgrass2010[sgrass2010 == 3] <- NA
sgrass2010[sgrass2010 > 0] <- 1
plot(sgrass2010)

# aggregate raster to increase speed of fragstats calculation
# you may want to increase this further, and maybe increase the buffer size in the loop below, too.
sgrass2010 <- aggregate(sgrass2010, 40, fun = sum)
res(sgrass2010)  # check the dimensions of the new cells
heatmap2010 <- (sgrass2010/1600)

#Specify colour ramp
library(RColorBrewer)
my.palette2010 <- brewer.pal(n = 9, name = "OrRd")
plot(heatmap2010, col = rev(my.palette2010)) # plot the proportion of the cells filled with seagrass (1600 being full coverage at this resolution)

