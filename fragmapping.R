###
# Project: Seagrass as habitat for juvenile lobsters
# Data:    Seagrass map
# Task:    Map fragmentation across the area
# author:  Kingsley Griffin
# date:    Aug 2022
#

install.packages("stars")

library(rgeos)
library(raster)
library(landscapemetrics)
library(terra)
library(rgdal)
library(stars)

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







#######################################################
#             removing single cell patches            #
#######################################################

r <- raster("data/seagrass_reclass2019.tif")



# fix classes into just seagrass or not
r[r == 3] <- 0
r[r > 0] <- 1




x <- focal(r, w=matrix(1,3,3), fun=modal) 

res(x)

x_factor <- as.factor(x) 
levels(x_factor)

x_seagrass_patches <- clump(x_factor, directions=8)
plot(x_seagrass_patches)
cellStats(x_seagrass_patches, max)

patchstats <- SDMTools::PatchStat(as.matrix(x_seagrass_patches), cellsize=0.499991)

hist(log(patchstats$area))

raster::writeRaster(x, "bigpatch_2019.tif", format="GTiff")







r_2010 <- raster("data/seagrass_reclass2010.tif")



# fix classes into just seagrass or not
r_2010[r_2010 == 3] <- 0
r_2010[r_2010 > 0] <- 1




x_2010 <- focal(r_2010, w=matrix(1,3,3), fun=modal) 

res(x_2010)

x_factor_2010 <- as.factor(x_2010) 
levels(x_factor_2010)

x_seagrass_patches_2010 <- clump(x_factor_2010, directions=8)
plot(x_seagrass_patches_2010)
cellStats(x_seagrass_patches_2010, max)


patchstats_2010 <- SDMTools::PatchStat(as.matrix(x_seagrass_patches_2010), cellsize=0.499991)

hist(log(patchstats_2010$area))

raster::writeRaster(x_2010, "bigpatch_2010.tif", format="GTiff")






r_2016 <- raster("data/seagrass_reclass2016.tif")



# fix classes into just seagrass or not
r_2016[r_2016 == 3] <- 0
r_2016[r_2016 > 0] <- 1




x_2016 <- focal(r_2016, w=matrix(1,3,3), fun=modal) 

res(x_2016)

x_factor_2016 <- as.factor(x_2016) 
levels(x_factor_2016)

x_seagrass_patches_2016 <- clump(x_factor_2016, directions=8)
plot(x_seagrass_patches_2016)
cellStats(x_seagrass_patches_2016, max)

patchstats_2016 <- SDMTools::PatchStat(as.matrix(x_seagrass_patches_2016), cellsize=0.499991)

hist(log(patchstats_2016$area))


raster::writeRaster(x_2016, "bigpatch_2016.tif", format="GTiff")

