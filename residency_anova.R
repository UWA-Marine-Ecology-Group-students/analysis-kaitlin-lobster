###
# Project: Seagrass as habitat for juvenile lobsters
# Data:    Structure density trials
# Task:    Testing residency time
# author:  Kaitlin McCloghry, Kingsley Griffin
# date:    April-July 2022
##

library(tidyverse)
library(ggplot2)
library(ggpubr)
library(broom)
library(AICcmodavg)


#attempting ANOVA with residency time (non-independence is an issue)

residency<- read.csv("data/structure_density_residencetime_anova.csv")

summary(aov(residency$Time~residency$Density))

boxplot(residency$Time~residency$Density)