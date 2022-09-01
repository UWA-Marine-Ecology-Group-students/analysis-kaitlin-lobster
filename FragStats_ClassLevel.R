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
library(dplyr)

working.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

data_dir <- paste(working.dir, "Data", sep="/")
raster_dir <- paste(working.dir, "Raster_Class", sep="/")

setwd(working.dir)


prep_function <- function(seagrass_patch){
  seagrass_patch[seagrass_patch == 0] <- NA
  seagrass_patch[seagrass_patch == 3] <- NA
  
  patch_factor <- as.factor(seagrass_patch) 
  
  patch_factor_clump <- clump(patch_factor, directions=8)
  no_patches <- cellStats(patch_factor_clump, max)
  
  prep_output <- list()
  prep_output[[1]] <- patch_factor
  prep_output[[2]] <- no_patches
  
  return(prep_output)
  
}

setwd(data_dir)
points <- read.csv("spatially_balanced_points.csv") %>% 
  mutate(CID = CID-1)

########################
#        LOOP          #
########################
setwd(raster_dir)

year <- c(10,16,19) # loop values for year
window <- c(10,16,19) # loop values for window
n_loop <- 1

results <- as.data.frame(array(0, dim=c(540,7))) %>% 
  rename(Year = "V1",
         Window = "V2",
         ID = "V3",
         No_Patches = "V4",
         Mean_Area = "V5",
         Mean_PAR = "V6",
         Mean_Div = "V7")

for(YEAR in 1:3){
  for(WIN in 1:3){
    for(P in 1:60){
      
      file_name <- paste0(year[YEAR], sep="_", window[WIN],sep="_", P, sep=".", "tif") #creates a new file name with the numbers going through the loop for year and window
      
      seagrass <- raster(file_name) #loads the raster using the file name we created above
      
      prep_data <- prep_function(seagrass) #data preparation function we made earlier
      
      #mean area of patches
      mean_area <- lsm_c_area_mn(prep_data[[1]], directions = 8) #because the function gives us a table, we assign it to something new
      results[n_loop,5] <- mean_area %>% 
        dplyr::select(value) #this chooses the value from the table we want to go into our results table by selecting the column name
      
      #mean perimeter area ratio of patches
      mean_par <- lsm_c_para_mn(prep_data[[1]], directions = 8)
      results[n_loop,6] <- mean_par %>% 
        dplyr::select(value)
      
      #mean landscape division of patches
      mean_div <- lsm_c_division(prep_data[[1]], directions = 8)
      results[n_loop,7] <- mean_div %>% 
        dplyr::select(value)
      
      ## Putting sampling year in results table
      if(YEAR == 1){
        results[n_loop, 1] <- 2010
      } else if (YEAR == 2){
        results[n_loop, 1] <- 2016
      } else {
        results[n_loop, 1] <- 2019
      }
      
      ## Putting window year into results table
      if(WIN == 1){
        results[n_loop, 2] <- 2010
      } else if (WIN == 2){
        results[n_loop, 2] <- 2016
      } else {
        results[n_loop, 2] <- 2019
      }
      
      ## Putting point number into results
      results[n_loop,3] <- P
      
      ## Putting number of patches from our function into results
      results[n_loop, 4] <- prep_data[[2]]
      
      ## Because our inner loop is 1-60 we can't use this to define the rows of the results table
      ## so we create a new variable that will just go up by one every time the loop runs to make sure the 
      ## data is going into the right row of the results table
      n_loop <- n_loop+1
      
    }
  }
}

results <- results %>% 
  mutate(CID = ifelse(ID>=1 & ID<=20, 1, 
                      ifelse(ID>20 & ID<=40, 2,
                             ifelse(ID>40,3,NA))))

 
write.csv(results,"C:/Users/kaitl/Documents/Kaitlin's Stuff/Dissertation/analysis-kaitlin-lobster/output\\fragstats_class_results.csv")

##### OLD CODE ####
# #load in raster image
# buff1919 <- raster(paste0("C:/Users/kaitl/Documents/ArcGIS/Projects/Seagrass_class_level/BUFF2019AREA/2019_window/19_19_1.tif"))
# plot(buff1919)
# 
# #Make non-vegetation NA
# buff1919[buff1919 == 0] <- NA
# buff1919[buff1919 == 3] <- NA
# 
# # convert to categorical raster
# buff1919_factor <- as.factor(buff1919) 
# levels(buff1919_factor)
# 
# 
# #8-neighbour rule to identify patches
# buff1919_factor_patches <- clump(buff1919_factor, directions=8)
# # plot(buff1919_factor_patches)#plot the result
# cellStats(buff1919_factor_patches, max)
# #^^^^^^^^THIS GETS NUMBER OF PATCHES^^^^^^^^^^
  



#Need to make loop and save output of four stages above
#Need to be able to group outputs by type of output, year, year window, and number in file name into 3 groups (1, 2, and 3) for statistical analysis


  