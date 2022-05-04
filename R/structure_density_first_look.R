library(tidyverse)

structure_density <- read.csv("data/structure_density_copy.csv")

#view data
head(structure_density)
View(structure_density)
str(structure_density)
summary(structure_density)

# tidy up the data
struct_dens_clean <- filter(structure_density, Useable.data == 1)
head(struct_dens_clean)

# ideal data setup:
# response = first choice level, covariates = (other choice - first choice), round, trial, ymaze, lobster

firstchoice <- data.frame("first_choice" = struct_dens_clean$first.choice,
                          "alternative" = c(struct_dens_clean$density.A + 
                                             struct_dens_clean$density.B) - 
                            struct_dens_clean$first.choice)
firstchoice$difference <- firstchoice$first_choice - firstchoice$alternative


# then do the same for different responses: final choice


