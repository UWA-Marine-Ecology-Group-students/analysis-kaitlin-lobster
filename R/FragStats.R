#LANDSCAPE METRICS
#install.packages("landscapemetrics")
#install.packages("landscapetools")
#install.packages("raster")
#install.packages("rgdal")
#install.packages("lattice")
#install.packages("rasterVis")
install.packages("SSDM")

library(landscapemetrics)
library(landscapetools)
library(raster)
library(sp)
library(rgdal)
library(ggplot2)
library(lattice)
library(rasterVis)
library(SSDM)



wd <- ("C:/Users/kaitl/Documents/Kaitlin's Stuff/Dissertation/analysis-kaitlin-lobster/data")
setwd(wd)

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

#Make sand NA
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
#patch-level analysis
############################################

#8-neighbour rule to identify patches
seagrass_patches_testpatch <- clump(testpatch_factor, directions=8)
plot(seagrass_patches_testpatch)#plot the result

#Not sure why there is only one patch
#Is it because there is more than 2 factors?
cellStats(seagrass_patches_testpatch, max)

#mean perimeter area ratio of each patch = 5.61
lsm_l_para_mn(testpatch_factor, directions = 8)

testpatchstats <- PatchStat(as.matrix(testpatch_factor), cellsize=0.499991)
SSDM:::
