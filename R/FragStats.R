#LANDSCAPE METRICS
#install.packages("landscapemetrics")
#install.packages("landscapetools")
#install.packages("raster")
#install.packages("rgdal")
#install.packages("lattice")
#install.packages("rasterVis")

library(landscapemetrics)
library(landscapetools)
library(raster)
library(sp)
library(rgdal)
library(ggplot2)
library(lattice)
library(rasterVis)


wd <- ("C:/Users/kaitl/Documents/Kaitlin's Stuff/Dissertation/analysis-kaitlin-lobster/data")
setwd(wd)

#load in raster image
#have merges 7 and 1 into 1 - "vegetation", and 3 and 5 as 3 - "no vegetation". 2 remains as "partial vegetation"
clas_image <- raster(paste0("C:/Users/kaitl/Documents/Kaitlin's Stuff/Dissertation/analysis-kaitlin-lobster/data/seagrass_reclass.tif"))
clas_image

#check raster is suitable for analysis
check_landscape(clas_image, verbose = TRUE)

#check raster resolution
res(clas_image)

#plot raster
plot(clas_image)
#l = lsm_c_contig_cv(clas_image, directions = 8)
#l

# convert to categorical raster
clas_image_factor <- as.factor(clas_image) 
levels(clas_image_factor)

#displays patches within the landscape
landscapemetrics:::show_patches(clas_image)

#assign names to each level
seagrass_cover <- levels(clas_image_factor)[[1]]
seagrass_cover[,"benthic_cover"] <- c("vegetation","partial vegetation", "no vegetation")
levels(clas_image_factor) <- seagrass_cover

#plot seagrass cover model with colours and labels
land_col <- c("dark green","green","light grey")
levelplot(clas_image_factor, col.regions=land_col, xlab="", ylab="")




############################################
#patch-level analysis
############################################

#8-neighbour rule to identify patches
seagrass_patches <- clump(clas_image_factor, directions=8)
plot(seagrass_patches)#plot the result

#Not sure why there is only one patch
#Is it because there is more than 2 factors?
cellStats(seagrass_patches, max)
