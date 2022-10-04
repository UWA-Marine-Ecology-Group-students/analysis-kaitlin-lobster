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
raster_dir <- paste(working.dir, "bp_Raster_Class", sep="/")

setwd(working.dir)


bp_prep_function <- function(bp_seagrass_patch){
  bp_seagrass_patch[bp_seagrass_patch == 0] <- NA
  bp_seagrass_patch[bp_seagrass_patch == 3] <- NA
  
  bp_patch_factor <- as.factor(bp_seagrass_patch) 
  
  bp_patch_factor_clump <- clump(bp_patch_factor, directions=8)
  bp_no_patches <- cellStats(bp_patch_factor_clump, max)
  
  bp_prep_output <- list()
  bp_prep_output[[1]] <- bp_patch_factor
  bp_prep_output[[2]] <- bp_no_patches
  
  return(bp_prep_output)
  
}

setwd(data_dir)
points <- read.csv("spatially_balanced_points.csv") %>% 
  mutate(CID = CID-1)

########################
#        LOOP          #
########################
setwd(raster_dir)

year <- c(10,16,19) # loop values for year
n_loop <- 1

bp_results <- as.data.frame(array(0, dim=c(180,6))) %>% 
  rename(Year = "V1",
         ID = "V2",
         No_Patches = "V3",
         Mean_Area = "V4",
         Mean_PAR = "V5",
         Mean_Div = "V6")

bp_file_name <- paste0("bp", year[YEAR], sep="_", P, sep=".", "tif") #creates a new file name with the numbers going through the loop for year and window

for(YEAR in 1:3){
    for(P in 1:60){
      
      bp_file_name <- paste0("bp", year[YEAR], sep="_", P, sep=".", "tif") #creates a new file name with the numbers going through the loop for year and window
      
      bp_seagrass <- raster(bp_file_name) #loads the raster using the file name we created above
      
      bp_prep_data <- bp_prep_function(bp_seagrass) #data preparation function we made earlier
      
      #mean area of patches
      bp_mean_area <- lsm_c_area_mn(bp_prep_data[[1]], directions = 8) #because the function gives us a table, we assign it to something new
      bp_results[n_loop,4] <- bp_mean_area %>% 
        dplyr::select(value) #this chooses the value from the table we want to go into our results table by selecting the column name
      
      #mean perimeter area ratio of patches
      bp_mean_par <- lsm_c_para_mn(bp_prep_data[[1]], directions = 8)
      bp_results[n_loop,5] <- bp_mean_par %>% 
        dplyr::select(value)
      
      #mean landscape division of patches
      bp_mean_div <- lsm_c_division(bp_prep_data[[1]], directions = 8)
      bp_results[n_loop,6] <- bp_mean_div %>% 
        dplyr::select(value)
      
      ## Putting sampling year in results table
      if(YEAR == 1){
        bp_results[n_loop, 1] <- 2010
      } else if (YEAR == 2){
        bp_results[n_loop, 1] <- 2016
      } else {
        bp_results[n_loop, 1] <- 2019
      }

      
      ## Putting point number into results
      bp_results[n_loop,2] <- P
      
      ## Putting number of patches from our function into results
      bp_results[n_loop, 3] <- bp_prep_data[[2]]
      
      ## Because our inner loop is 1-60 we can't use this to define the rows of the results table
      ## so we create a new variable that will just go up by one every time the loop runs to make sure the 
      ## data is going into the right row of the results table
      n_loop <- n_loop+1
      
    }
  }

bp_results <- bp_results %>% 
  mutate(CID = ifelse(ID>=1 & ID<=20, 1, 
                      ifelse(ID>20 & ID<=40, 2,
                             ifelse(ID>40,3,NA))))


write.csv(bp_results,"C:/Users/kaitl/Documents/Kaitlin's Stuff/Dissertation/analysis-kaitlin-lobster/output\\bigpatch_results.csv")

##### OLD CODE ###
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


