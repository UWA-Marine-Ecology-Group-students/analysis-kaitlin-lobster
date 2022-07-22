
#Structure vs scent RUM

library(dplyr)
library(tidyr)

# read in data
structure_vs_scent <- read.csv("data/Structure_vs_scent_20.07.2022.csv")

# tidy up the data
struct_vs_scent_clean <- filter(structure_vs_scent, Useable.data == 1)
head(struct_vs_scent_clean)




#FIRST CHOICE
structvsscent_firstchoice_pivoted <- struct_vs_scent_clean %>% 
  dplyr::select(round, trial, lobster, side.A, side.B, first.choice)

#structure data for analysis
#Matt - should I make the choice made = 1 and the choice not made = 0? 
#Or attribute numbers in another way?
structvsscent_firstchoice_rum <- structvsscent_firstchoice_pivoted %>% 
  pivot_longer(cols = c("side.A", "side.B"),
               names_to = "side", values_to = "treatment" ) %>% 
#  mutate(first.choice = ifelse(first.choice==treatment, 1, 0))

#save file
write.csv(structvsscent_firstchoice_rum,"C:/Users/kaitl/Documents/Kaitlin's Stuff/Dissertation/analysis-kaitlin-lobster/data/Modified data\\structurevsscent_firstchoice_RUM.csv", row.names = FALSE)




#FINAL CHOICE
structvsscent_finalchoice_pivoted <- struct_vs_scent_clean %>% 
  dplyr::select(round, trial, lobster, side.A, side.B, final.choice)

#structure data for analysis
structvsscent_finalchoice_rum <- structvsscent_finalchoice_pivoted %>% 
  pivot_longer(cols = c("side.A", "side.B"),
               names_to = "side", values_to = "treatment" ) %>% 
  #  mutate(first.choice = ifelse(first.choice==treatment, 1, 0))

#remove NAs
structvsscent_finalchoice_rum_clean <- structvsscent_finalchoice_rum[complete.cases(structvsscent_finalchoice_rum),]

#check for no NAs
sum(is.na(structvsscent_finalchoice_rum_clean))

#save file
write.csv(structvsscent_finalchoice_rum_clean,"C:/Users/kaitl/Documents/Kaitlin's Stuff/Dissertation/analysis-kaitlin-lobster/data/Modified data\\structurevsscent_finalchoice_RUM.csv", row.names = FALSE)






