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

##############################
#         2019 - 10m         #
##############################

#load in raster image
#have merges 7 and 1 into 1 - "vegetation", and 3 and 5 as 3 - "no vegetation". 2 remains as "partial vegetation"
sgrass_19 <- raster("bigpatch_2019.tif")
plot(sgrass_19)[]

# fix classes into just seagrass or not
sgrass_19[sgrass_19 == 0] <- NA
sgrass_19[sgrass_19 > 0] <- 1
plot(sgrass_19)
res(sgrass_19)

# aggregate raster to increase speed of fragstats calculation
# you may want to increase this further, and maybe increase the buffer size in the loop below, too.
sgrass_19_10 <- aggregate(sgrass_19, 20, fun = sum)
res(sgrass_19_10)  # check the dimensions of the new cells
heatmap_19_10 <- (sgrass_19_10/400)

#Specify colour ramp
library(RColorBrewer)
my.palette <- brewer.pal(n = 9, name = "OrRd")
plot(heatmap_19_10, col = rev(my.palette)) # plot the proportion of the cells filled with seagrass (1600 being full coverage at this resolution)


##############################
#         2019 - 20m         #
##############################

# aggregate raster to increase speed of fragstats calculation
# you may want to increase this further, and maybe increase the buffer size in the loop below, too.
sgrass_19_20 <- aggregate(sgrass_19, 40, fun = sum)
res(sgrass_19_20)  # check the dimensions of the new cells
heatmap_19_20 <- (sgrass_19_20/1600)

#Specify colour ramp
library(RColorBrewer)
my.palette <- brewer.pal(n = 9, name = "OrRd")
plot(heatmap_19_20, col = rev(my.palette)) # plot the proportion of the cells filled with seagrass (1600 being full coverage at this resolution)


##############################
#         2019 - 50m         #
##############################

# aggregate raster to increase speed of fragstats calculation
# you may want to increase this further, and maybe increase the buffer size in the loop below, too.
sgrass_19_50 <- aggregate(sgrass_19, 100, fun = sum)
res(sgrass_19_50)  # check the dimensions of the new cells
heatmap_19_50 <- (sgrass_19_50/10000)

#Specify colour ramp
library(RColorBrewer)
my.palette <- brewer.pal(n = 9, name = "OrRd")
plot(heatmap_19_50, col = rev(my.palette)) # plot the proportion of the cells filled with seagrass (1600 being full coverage at this resolution)


##############################
#         2019 - 100m         #
##############################

# aggregate raster to increase speed of fragstats calculation
# you may want to increase this further, and maybe increase the buffer size in the loop below, too.
sgrass_19_100 <- aggregate(sgrass_19, 200, fun = sum)
res(sgrass_19_100)  # check the dimensions of the new cells
heatmap_19_100 <- (sgrass_19_100/40000)

#Specify colour ramp
library(RColorBrewer)
my.palette <- brewer.pal(n = 9, name = "OrRd")
plot(heatmap_19_100, col = rev(my.palette)) # plot the proportion of the cells filled with seagrass (1600 being full coverage at this resolution)


##################################
#         2019 - 263.77m         #
##################################

# aggregate raster to increase speed of fragstats calculation
# you may want to increase this further, and maybe increase the buffer size in the loop below, too.
sgrass_19_264 <- aggregate(sgrass_19, 527.55, fun = sum)
res(sgrass_19_264)  # check the dimensions of the new cells
heatmap_19_264 <- (sgrass_19_264/278309.0025)

#Specify colour ramp
library(RColorBrewer)
my.palette <- brewer.pal(n = 9, name = "OrRd")
plot(heatmap_19_264, col = rev(my.palette)) # plot the proportion of the cells filled with seagrass (1600 being full coverage at this resolution)






##############################
#         2016 - 10m         #
##############################

#load in raster image
#have merges 7 and 1 into 1 - "vegetation", and 3 and 5 as 3 - "no vegetation". 2 remains as "partial vegetation"
sgrass_16 <- raster("bigpatch_2016.tif")
plot(sgrass_16)[]


# fix classes into just seagrass or not
sgrass_16[sgrass_16 == 0] <- NA
sgrass_16[sgrass_16 > 0] <- 1
plot(sgrass_16)
res(sgrass_16)


# aggregate raster to increase speed of fragstats calculation
# you may want to increase this further, and maybe increase the buffer size in the loop below, too.
sgrass_16_10 <- aggregate(sgrass_16, 20, fun = sum)
res(sgrass_16_10)  # check the dimensions of the new cells
heatmap_16_10 <- (sgrass_16_10/400)

#Specify colour ramp
library(RColorBrewer)
my.palette <- brewer.pal(n = 9, name = "OrRd")
plot(heatmap_16_10, col = rev(my.palette)) # plot the proportion of the cells filled with seagrass (1600 being full coverage at this resolution)


##############################
#         2016 - 20m         #
##############################

# aggregate raster to increase speed of fragstats calculation
# you may want to increase this further, and maybe increase the buffer size in the loop below, too.
sgrass_16_20 <- aggregate(sgrass_16, 40, fun = sum)
res(sgrass_16_20)  # check the dimensions of the new cells
heatmap_16_20 <- (sgrass_16_20/1600)

#Specify colour ramp
library(RColorBrewer)
my.palette <- brewer.pal(n = 9, name = "OrRd")
plot(heatmap_16_20, col = rev(my.palette)) # plot the proportion of the cells filled with seagrass (1600 being full coverage at this resolution)


##############################
#         2016 - 50m         #
##############################

# aggregate raster to increase speed of fragstats calculation
# you may want to increase this further, and maybe increase the buffer size in the loop below, too.
sgrass_16_50 <- aggregate(sgrass_16, 100, fun = sum)
res(sgrass_16_50)  # check the dimensions of the new cells
heatmap_16_50 <- (sgrass_16_50/10000)

#Specify colour ramp
library(RColorBrewer)
my.palette <- brewer.pal(n = 9, name = "OrRd")
plot(heatmap_16_50, col = rev(my.palette)) # plot the proportion of the cells filled with seagrass (1600 being full coverage at this resolution)


###############################
#         2016 - 100m         #
###############################

# aggregate raster to increase speed of fragstats calculation
# you may want to increase this further, and maybe increase the buffer size in the loop below, too.
sgrass_16_100 <- aggregate(sgrass_16, 200, fun = sum)
res(sgrass_16_100)  # check the dimensions of the new cells
heatmap_16_100 <- (sgrass_16_100/40000)

#Specify colour ramp
library(RColorBrewer)
my.palette <- brewer.pal(n = 9, name = "OrRd")
plot(heatmap_16_100, col = rev(my.palette)) # plot the proportion of the cells filled with seagrass (1600 being full coverage at this resolution)


##################################
#         2016 - 263.77m         #
##################################

# aggregate raster to increase speed of fragstats calculation
# you may want to increase this further, and maybe increase the buffer size in the loop below, too.
sgrass_16_264 <- aggregate(sgrass_16, 527.55, fun = sum)
res(sgrass_16_264)  # check the dimensions of the new cells
heatmap_16_264 <- (sgrass_16_264/278309.0025)

#Specify colour ramp
library(RColorBrewer)
my.palette <- brewer.pal(n = 9, name = "OrRd")
plot(heatmap_16_264, col = rev(my.palette)) # plot the proportion of the cells filled with seagrass (1600 being full coverage at this resolution)








##############################
#         2010 - 10m         #
##############################

#load in raster image
#have merges 7 and 1 into 1 - "vegetation", and 3 and 5 as 3 - "no vegetation". 2 remains as "partial vegetation"
sgrass_10<-raster("bigpatch_2010.tif")
plot(sgrass_10)

# fix classes into just seagrass or not
sgrass_10[sgrass_10 == 0] <- NA
sgrass_10[sgrass_10 > 0] <- 1
plot(sgrass_10)
res(sgrass_10)

# aggregate raster to increase speed of fragstats calculation
# you may want to increase this further, and maybe increase the buffer size in the loop below, too.
sgrass_10_10 <- aggregate(sgrass_10, 20, fun = sum)
res(sgrass_10_10)  # check the dimensions of the new cells
heatmap_10_10 <- (sgrass_10_10/400)

#Specify colour ramp
library(RColorBrewer)
my.palette <- brewer.pal(n = 9, name = "OrRd")
plot(heatmap_10_10, col = rev(my.palette)) # plot the proportion of the cells filled with seagrass (1600 being full coverage at this resolution)


##############################
#         2010 - 20m         #
##############################

# aggregate raster to increase speed of fragstats calculation
# you may want to increase this further, and maybe increase the buffer size in the loop below, too.
sgrass_10_20 <- aggregate(sgrass_10, 40, fun = sum)
res(sgrass_10_20)  # check the dimensions of the new cells
heatmap_10_20 <- (sgrass_10_20/1600)

#Specify colour ramp
library(RColorBrewer)
my.palette <- brewer.pal(n = 9, name = "OrRd")
plot(heatmap_10_20, col = rev(my.palette)) # plot the proportion of the cells filled with seagrass (1600 being full coverage at this resolution)


##############################
#         2010 - 50m         #
##############################

# aggregate raster to increase speed of fragstats calculation
# you may want to increase this further, and maybe increase the buffer size in the loop below, too.
sgrass_10_50 <- aggregate(sgrass_10, 100, fun = sum)
res(sgrass_10_50)  # check the dimensions of the new cells
heatmap_10_50 <- (sgrass_10_50/10000)

#Specify colour ramp
library(RColorBrewer)
my.palette <- brewer.pal(n = 9, name = "OrRd")
plot(heatmap_10_50, col = rev(my.palette)) # plot the proportion of the cells filled with seagrass (1600 being full coverage at this resolution)


###############################
#         2010 - 100m         #
###############################

# aggregate raster to increase speed of fragstats calculation
# you may want to increase this further, and maybe increase the buffer size in the loop below, too.
sgrass_10_100 <- aggregate(sgrass_10, 200, fun = sum)
res(sgrass_10_100)  # check the dimensions of the new cells
heatmap_10_100 <- (sgrass_10_100/40000)

#Specify colour ramp
library(RColorBrewer)
my.palette <- brewer.pal(n = 9, name = "OrRd")
plot(heatmap_10_100, col = rev(my.palette)) # plot the proportion of the cells filled with seagrass (1600 being full coverage at this resolution)


##################################
#         2010 - 263.77m         #
##################################

# aggregate raster to increase speed of fragstats calculation
# you may want to increase this further, and maybe increase the buffer size in the loop below, too.
sgrass_10_264 <- aggregate(sgrass_10, 527.55, fun = sum)
res(sgrass_10_264)  # check the dimensions of the new cells
heatmap_10_264 <- (sgrass_10_264/278309.0025)

#Specify colour ramp
library(RColorBrewer)
my.palette <- brewer.pal(n = 9, name = "OrRd")
plot(heatmap_10_264, col = rev(my.palette)) # plot the proportion of the cells filled with seagrass (1600 being full coverage at this resolution)





