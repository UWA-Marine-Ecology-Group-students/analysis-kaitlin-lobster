#LANDSCAPE METRICS
#install.packages("landscapemetrics")
#install.packages("landscapetools")
#install.packages("raster")
#install.packages("rgdal")
#install.packages("lattice")
#install.packages("rasterVis")
#install.packages("devtools")
#install_version("SDMTools", version = "1.1-221.2", repos = "https://cran.r-project.org")


library(landscapemetrics)
library(landscapetools)
library(raster)
library(sp)
library(rgdal)
library(ggplot2)
library(lattice)
library(rasterVis)
library(SSDM)
library(devtools)



wd <- ("C:/Users/kaitl/Documents/Kaitlin's Stuff/Dissertation/analysis-kaitlin-lobster/data")
setwd(wd)


########################
#         2019        #
########################

#load in raster image
#have merges 7 and 1 into 1 - "vegetation", and 3 and 5 as 3 - "no vegetation". 2 remains as "partial vegetation"
clas_image <- raster(paste0("C:/Users/kaitl/Documents/Kaitlin's Stuff/Dissertation/analysis-kaitlin-lobster/data/seagrass_reclass.tif"))
clas_image
plot(clas_image)

#reclassify for vegetation and non vegetation
m <- c(0, 2, 1,  2.1, 3, 2)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
class_image_two <- reclassify(clas_image, rclmat)
plot(class_image_two)

#Make non-vegetation NA
class_image_two[class_image_two == 2] <- NA

#check raster is suitable for analysis
check_landscape(class_image_two, verbose = TRUE)

# crop raster for testing
testarea  <- extent(304400, 304600, 6732300, 6732500)
testpatch <- crop(class_image_two, testarea)
plot(testpatch)

#check raster resolution
res(testpatch)

# convert to categorical raster
testpatch_factor <- as.factor(testpatch) 
levels(testpatch_factor)

#displays patches within the landscape
landscapemetrics:::show_patches(testpatch_factor, class = 1)

#assign names to each level
seagrass_cover_testpatch <- levels(testpatch_factor)[[1]]
seagrass_cover_testpatch[,"benthic_cover"] <- c("vegetation")
levels(testpatch_factor) <- seagrass_cover_testpatch

#plot seagrass cover model with colours and labels
land_col <- c("dark green", "light grey")
levelplot(testpatch_factor, col.regions=land_col, xlab="", ylab="")




############################################
#patch-level analysis - test patch
############################################

#8-neighbour rule to identify patches
seagrass_patches_testpatch <- clump(testpatch_factor, directions=8)
plot(seagrass_patches_testpatch)#plot the result
cellStats(seagrass_patches_testpatch, max)

#mean perimeter area ratio of each patch = 5.61
lsm_l_para_mn(testpatch_factor, directions = 8)

#statistics by patch
testpatchstats <- SDMTools::PatchStat(as.matrix(seagrass_patches_testpatch), cellsize=0.499991)
testpatchstats

#histogram of log area of patches 
hist(log(testpatchstats$area))

#histogram of perimeter area ratio 
hist(testpatchstats$perim.area.ratio)

############################################
#patch-level analysis - full area
############################################
# convert to categorical raster
class_image_two_factor <- as.factor(class_image_two) 
levels(class_image_two_factor)

#displays patches within the landscape
landscapemetrics:::show_patches(class_image_two_factor, class = 1)

#8-neighbour rule to identify patches
seagrass_patches <- clump(class_image_two_factor, directions=8)
plot(seagrass_patches)#plot the result
cellStats(seagrass_patches, max)

#mean perimeter area ratio of each patch = 5.68
lsm_l_para_mn(class_image_two_factor, directions = 8)

#statistics by patch
patchstats <- SDMTools::PatchStat(as.matrix(seagrass_patches), cellsize=0.499991)
patchstatsmean <- colMeans(patchstats[,2:ncol(patchstats)])
patchstatsmean

#histogram of log area of patches 
hist(log(patchstats$area))

#histogram of perimeter area ratio 
hist(patchstats$perim.area.ratio)



########################
#         2016        #
########################

#load in raster image
#have merged 7 and 1 into 1 - "vegetation", and 3 and 5 as 3 - "no vegetation". 2 remains as "partial vegetation"
sgrass2016 <- raster("data/seagrass_reclass2016.tif")
plot(sgrass2016)[]

# fix classes into just seagrass or not
sgrass2016[sgrass2016 == 3] <- NA
sgrass2016[sgrass2016 > 0] <- 1
plot(sgrass2016)

# convert to categorical raster
sgrass2016_factor <- as.factor(sgrass2016) 
levels(sgrass2016_factor)

#displays patches within the landscape
landscapemetrics:::show_patches(sgrass2016_factor, class = 1)

#8-neighbour rule to identify patches
seagrass_patches2016 <- clump(sgrass2016_factor, directions=8)
plot(seagrass_patches2016)#plot the result
cellStats(seagrass_patches2016, max)

#mean perimeter area ratio of each patch = 5.83
lsm_l_para_mn(sgrass2016_factor, directions = 8)

#statistics by patch
patchstats2016 <- SDMTools::PatchStat(as.matrix(seagrass_patches2016), cellsize=0.499991)
patchstatsmean2016 <- colMeans(patchstats2016[,2:ncol(patchstats2016)])
patchstatsmean2016

#histogram of log area of patches 
hist(log(patchstats2016$area))

#histogram of perimeter area ratio 
hist(patchstats2016$perim.area.ratio)


########################
#         2010        #
########################

#load in raster image
#have merged 7 and 1 into 1 - "vegetation", and 3 and 5 as 3 - "no vegetation". 2 remains as "partial vegetation"
sgrass2010 <- raster("data/seagrass_reclass2010.tif")
plot(sgrass2010)[]

# fix classes into just seagrass or not
sgrass2010[sgrass2010 == 3] <- NA
sgrass2010[sgrass2010 > 0] <- 1
plot(sgrass2010)

# convert to categorical raster
sgrass2010_factor <- as.factor(sgrass2010) 
levels(sgrass2010_factor)

#displays patches within the landscape
landscapemetrics:::show_patches(sgrass2010_factor, class = 1)

#8-neighbour rule to identify patches
seagrass_patches2010 <- clump(sgrass2010_factor, directions=8)
plot(seagrass_patches2010)#plot the result
cellStats(seagrass_patches2010, max)

#mean perimeter area ratio of each patch = 5.47
lsm_l_para_mn(sgrass2010_factor, directions = 8)

#statistics by patch
patchstats2010 <- SDMTools::PatchStat(as.matrix(seagrass_patches2010), cellsize=0.499991)
patchstatsmean2010 <- colMeans(patchstats2010[,2:ncol(patchstats2010)])
patchstatsmean2010

#histogram of log area of patches 
hist(log(patchstats2010$area))

#histogram of perimeter area ratio 
hist(patchstats2010$perim.area.ratio)
