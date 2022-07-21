
#Structure density RUM

library(dplyr)
library(tidyr)

# read in data
structure_density <- read.csv("data/Structure_density_raw_19.07.22.csv")

# tidy up the data
struct_dens_clean <- filter(structure_density, Useable.data == 1)
head(struct_dens_clean)




#FIRST CHOICE
data_firstchoice_pivoted <- struct_dens_clean %>% 
  dplyr::select(round, trial, lobster, density.A, density.B, first.choice)

#structure data for analysis
data_firstchoice_rum <- data_firstchoice_pivoted %>% 
  pivot_longer(cols = c("density.A", "density.B"),
               names_to = "side", values_to = "density" ) %>% 
  mutate(first.choice = ifelse(first.choice==density, 1, 0))

#save file
write.csv(data_firstchoice_rum,"C:/Users/kaitl/Documents/Kaitlin's Stuff/Dissertation/analysis-kaitlin-lobster/data/Modified data\\structure_density_firstchoice_RUM.csv", row.names = FALSE)





#FINAL CHOICE
data_finalchoice_pivoted <- struct_dens_clean %>% 
  dplyr::select(round, trial, lobster, density.A, density.B, final.choice)

#structure data for analysis
data_finalchoice_rum <- data_finalchoice_pivoted %>% 
  pivot_longer(cols = c("density.A", "density.B"),
               names_to = "side", values_to = "density" ) %>% 
  mutate(final.choice = ifelse(final.choice==density, 1, 0))

#remove NAs
data_finalchoice_rum_clean <- data_finalchoice_rum[complete.cases(data_finalchoice_rum),]

#check for no NAs
sum(is.na(data_finalchoice_rum_clean))

#save file
write.csv(data_finalchoice_rum_clean,"C:/Users/kaitl/Documents/Kaitlin's Stuff/Dissertation/analysis-kaitlin-lobster/data/Modified data\\structure_density_finalchoice_RUM.csv", row.names = FALSE)




