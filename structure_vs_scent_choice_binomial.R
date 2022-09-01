###
# Project: Seagrass as habitat for juvenile lobsters
# Data:    Structure Vs Scent Trials - Binomial model
# Task:    Data wrangling and modelling for first/final choice data
# author:  Kaitlin McCloghry, Kingsley Griffin
# date:    April-July 2022
#

library(tidyverse)
library(reshape2)
library(ggplot2)
library(lme4)
library(dplyr)
library(tidyr)
library(stringr)

# read in data
structure_vs_scent <- read.csv("data/Structure_vs_scent_20.07.2022.csv")

#view data
head(structure_vs_scent)
# View(structure_vs_scent)
str(structure_vs_scent)
summary(structure_vs_scent)

# tidy up the data
struct_vs_scent_clean <- filter(structure_vs_scent, Useable.data == 1)
head(struct_vs_scent_clean)

#struct_vs_scent_clean$lower     <- pmin(struct_vs_scent_clean$density.A, struct_vs_scent_clean$density.B)
#struct_vs_scent_clean$higher    <- pmax(struct_dens_clean$density.A, struct_dens_clean$density.B)




#FIRST CHOICE

firstchoice_bylevel_structscent <- summarise(group_by(struct_vs_scent_clean, first.choice),
                                 nfirstchoice_structscent = n())
head(firstchoice_bylevel_structscent)


#getting ntrials
trials_structure <- table(struct_vs_scent_clean$side.A, struct_vs_scent_clean$side.B)
trials_structure <- as.data.frame.matrix(trials_structure)
trials_structure$ntrials <- trials_structure$blank + trials_structure$scent + trials_structure$`st/sc` + trials_structure$structure
trials_structure

side.A <- struct_vs_scent_clean %>% 
  group_by(side.A) %>% 
  summarise(countA=n())
side.B <- struct_vs_scent_clean %>% 
  group_by(side.B) %>% 
  summarise(countB=n())
trials_struct_scent <- cbind(side.A, side.B) %>%  
  mutate(ntrials=countA+countB)

#creating dataframe to run model with
firstchoice_factors_structscent <- firstchoice_bylevel_structscent %>% 
  dplyr::select(c(first.choice, nfirstchoice_structscent)) %>% 
  mutate(ntrials = trials_struct_scent$ntrials) %>% 
  rename(factor = "first.choice",
         nfirstchoice = "nfirstchoice_structscent")
firstchoice_factors_structscent

# plot the main effect we're interested in
ggplot(firstchoice_factors_structscent, aes(factor, nfirstchoice/ntrials)) + 
  geom_point() +
  theme_bw()

#cannot create line of best fit as non-linear

# model this to check its robustness
m1_structscent_first <- glm(cbind(nfirstchoice, ntrials - nfirstchoice) ~ factor, 
          data = firstchoice_factors_structscent, family = "binomial")
summary(m1_structscent_first)
plot(m1_structscent_first)

#cannot get SE curves as non-linear

ggsave("plots/structure_vs_scent_firstchoice_binomial.png")





#LAST CHOICE
finalchoice_bylevel_structscent <- summarise(group_by(struct_vs_scent_clean, final.choice),
                                             nlastchoice_structscent = n())
head(finalchoice_bylevel_structscent)

finalchoice_bylevel_structscent_clean <- finalchoice_bylevel_structscent[complete.cases(finalchoice_bylevel_structscent),]

#getting ntrials
side.A_final <- struct_vs_scent_clean %>% 
  group_by(side.A) %>% 
  summarise(countA=n())
side.B_final <- struct_vs_scent_clean %>% 
  group_by(side.B) %>% 
  summarise(countB=n())
trials_struct_scent_final <- cbind(side.A_final, side.B_final) %>%  
  mutate(ntrials=countA+countB)

#creating dataframe to run model with
finalchoice_factors_structscent <- finalchoice_bylevel_structscent_clean %>% 
  dplyr::select(c(final.choice, nlastchoice_structscent)) %>% 
  mutate(ntrials = trials_struct_scent_final$ntrials) %>% 
  rename(factor = "final.choice",
         nfinalchoice = "nlastchoice_structscent")
finalchoice_factors_structscent

# plot the main effect we're interested in
ggplot(finalchoice_factors_structscent, aes(factor, nfinalchoice/ntrials)) + 
  geom_point() +
  theme_bw()

#cannot create line of best fit as non-linear

# model this to check its robustness
m1_structscent_final <- glm(cbind(nfinalchoice, ntrials - nfinalchoice) ~ factor, 
          data = finalchoice_factors_structscent, family = "binomial")
summary(m1_structscent_final)
plot(m1_structscent_final)

#cannot get SE curves as non-linear

ggsave("plots/structure_vs_scent_finalchoice_binomial.png")
