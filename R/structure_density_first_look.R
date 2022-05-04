install.packages("tidyverse")
install.packages(c("ggplot2", "ggpubr", "broom", "AICcmodavg"))
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(broom)
library(AICcmodavg)

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
firstchoice$round <- struct_dens_clean$round
firstchoice$trial <- struct_dens_clean$trial
firstchoice$ymaze <- struct_dens_clean$ymaze
firstchoice$lobster <- struct_dens_clean$lobster

head(firstchoice)


# then do the same for different responses: final choice

finalchoice <- data.frame("final_choice" = struct_dens_clean$final.choice,
                          "alternative" = c(struct_dens_clean$density.A + 
                                              struct_dens_clean$density.B) - 
                            struct_dens_clean$final.choice)

finalchoice$difference <- finalchoice$final_choice - finalchoice$alternative
finalchoice$round <- struct_dens_clean$round
finalchoice$trial <- struct_dens_clean$trial
finalchoice$ymaze <- struct_dens_clean$ymaze
finalchoice$lobster <- struct_dens_clean$lobster

head(finalchoice)


#attempting ANOVA with residency time (non-independence is an issue)

residency<- read.csv("data/structure_density_residencetime_anova.csv")

summary(aov(residency$Time~residency$Density))

boxplot(residency$Time~residency$Density)
