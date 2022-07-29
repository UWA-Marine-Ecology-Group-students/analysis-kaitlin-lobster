#LANDSCAPE METRICS
#install.packages("landscapemetrics")
#install.packages("landscapetools")
#install.packages("raster")
#install.packages("rgdal")

library(landscapemetrics)
library(landscapetools)
library(raster)
library(sp)
library(rgdal)
library(ggplot2)


wd <- ("C:/Users/kaitl/Documents/Kaitlin's Stuff/Dissertation/analysis-kaitlin-lobster/data")
setwd(wd)

#load in raster image
clas_image <- raster(paste0("C:/Users/kaitl/Documents/Kaitlin's Stuff/Dissertation/analysis-kaitlin-lobster/data/seagrass_classification_raster.tif"))
clas_image

#check raster is suitable for analysis
check_landscape(clas_image, verbose = TRUE)


landscapemetrics:::show_landscape(clas_image, discrete = TRUE)
plot(clas_image)
l = lsm_c_contig_cv(clas_image, directions = 8)
l
