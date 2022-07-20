###
# Project: Seagrass as habitat for juvenile lobsters
# Data:    Structure density trials
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
# library(ggpubr)
# library(broom)
# library(AICcmodavg)

# read in data
structure_density <- read.csv("data/Structure_density_raw_19.07.22.csv")

#view data
head(structure_density)
# View(structure_density)
str(structure_density)
summary(structure_density)

# tidy up the data
struct_dens_clean <- filter(structure_density, Useable.data == 1)
head(struct_dens_clean)

#Kingsley - did we use this alternative clean data at all?
struct_dens_clean$alternative <- c(struct_dens_clean$density.A + struct_dens_clean$density.B) - 
  struct_dens_clean$first.choice

struct_dens_clean$lower     <- pmin(struct_dens_clean$density.A, struct_dens_clean$density.B)
struct_dens_clean$higher    <- pmax(struct_dens_clean$density.A, struct_dens_clean$density.B)
# struct_dens_clean$trial_lvl <- as.factor(paste(struct_dens_clean$lower, struct_dens_clean$higher, sep = "_"))
# struct_dens_clean$firsthigh <- ifelse(struct_dens_clean$first.choice > struct_dens_clean$alternative, 1, 0)
# struct_dens_clean$lasthigh  <- ifelse(struct_dens_clean$final.choice > struct_dens_clean$alternative, 1, 0)
# struct_dens_clean$lob_num   <- gsub("[A-Z]", "", struct_dens_clean$lobster)

# struct_dens_clean$perc_dif  <- (struct_dens_clean$higher - struct_dens_clean$lower) / 
#   struct_dens_clean$higher

# trying summary by level







#FIRST CHOICE

firstchoice_bylevel <- summarise(group_by(struct_dens_clean, first.choice),
                                 nfirstchoice = n())
head(firstchoice_bylevel)

lower_n  <- summarise(group_by(struct_dens_clean, lower), ntest = n())
higher_n <- summarise(group_by(struct_dens_clean, higher), ntest = n())
names(lower_n)[1]  <- "level"
names(higher_n)[1] <- "level"

trials <- merge(lower_n, higher_n, by = "level", all = TRUE) %>%
  replace_na(list(ntest.y=0, ntest.x=0))
# trials$ntest.y[1] <- 0
# trials$ntest.x[6] <- 0
trials$ntest <- trials$ntest.x + trials$ntest.y
head(trials)

firstchoice_bylevel$ntrials  <- trials$ntest
firstchoice_bylevel$nsuccess <- firstchoice_bylevel$nfirstchoice
firstchoice_bylevel$density  <- firstchoice_bylevel$first.choice

head(firstchoice_bylevel)

# plot the main effect we're interested in
ggplot(firstchoice_bylevel, aes(density, nfirstchoice/ntrials)) + 
  geom_point() +
  theme_bw()

# this looks like a logarithmic curve to me - try adding a log line of best fit
ggplot(firstchoice_bylevel, aes(density, nfirstchoice/ntrials)) + 
  geom_point() + 
  geom_smooth(method = glm, formula = y ~ log(x + 1), se = FALSE) +
  theme_bw()

# model this to check its robustness
m1 <- glm(cbind(nsuccess, ntrials - nsuccess) ~ log(density + 1), 
          data = firstchoice_bylevel, family = "binomial")
summary(m1)
plot(m1)

# now we need to make predictions from this model back onto our data, so we can make a final plot

pred_df_stage1     <- data.frame("density" = seq(from = 0, to = 32, by = 1 ))
predictions <- as.data.frame(predict(m1, pred_df_stage1, type = "response", se = TRUE))
pred_df_stage2     <- cbind(pred_df_stage1, predictions[, 1:2])
head(pred_df_stage2)

ggplot() + 
  geom_point(data = firstchoice_bylevel, aes(density, nsuccess/ntrials), alpha = 0.5) +
  geom_ribbon(data = pred_df_stage2, aes(density, 
                                  ymin = fit - se.fit, 
                                  ymax = fit + se.fit),
              alpha = 0.2) +
  geom_line(data = pred_df_stage2, aes(density, fit)) +
  labs(y = "nchoice/ntrials") +
  theme_bw()

ggsave("plots/structure_density_firstchoice.png")





#LAST CHOICE

lastchoice_bylevel <- summarise(group_by(struct_dens_clean, final.choice),
                                 nlastchoice = n()) %>%
  filter(!is.na(final.choice))
head(lastchoice_bylevel)

lower_n_last  <- summarise(group_by(struct_dens_clean, lower), ntest_last = n())
higher_n_last <- summarise(group_by(struct_dens_clean, higher), ntest_last = n())
names(lower_n_last)[1]  <- "level"
names(higher_n_last)[1] <- "level"

trials_last <- merge(lower_n_last, higher_n_last, by = "level", all = TRUE) %>%
  replace_na(list(ntest_last.y=0, ntest_last.x=0))
#trials$ntest.y[1] <- 0
#trials$ntest.x[6] <- 0
trials_last$ntest_last <- trials_last$ntest_last.x + trials_last$ntest_last.y
head(trials_last)

lastchoice_bylevel$ntrials_last  <- trials_last$ntest_last
lastchoice_bylevel$nsuccess_last <- lastchoice_bylevel$nlastchoice
lastchoice_bylevel$density_last  <- lastchoice_bylevel$final.choice

head(lastchoice_bylevel)

# plot the main effect we're interested in
ggplot(lastchoice_bylevel, aes(density_last, nlastchoice/ntrials_last)) + 
  geom_point() +
  theme_bw()

# this looks like a logarithmic curve to me - try adding a log line of best fit
ggplot(lastchoice_bylevel, aes(density_last, nlastchoice/ntrials_last)) + 
  geom_point() + 
  geom_smooth(method = glm, formula = y ~ log(x + 1), se = FALSE) +
  theme_bw()

# model this to check its robustness
m1last <- glm(cbind(nsuccess_last, ntrials_last - nsuccess_last) ~ log(density_last + 1), 
          data = lastchoice_bylevel, family = "binomial")
summary(m1last)
plot(m1last)

# now we need to make predictions from this model back onto our data, so we can make a final plot

pred_df_last_stage1     <- data.frame("density_last" = seq(from = 0, to = 32, by = 1 ))
predictions_last <- as.data.frame(predict(m1last, pred_df_last_stage1, type = "response", se = TRUE))
pred_df_last_stage2     <- cbind(pred_df_last_stage1, predictions_last[, 1:2])
head(pred_df_last_stage2)

ggplot() + 
  geom_point(data = lastchoice_bylevel, aes(density_last, nsuccess_last/ntrials_last), alpha = 0.5) +
  geom_ribbon(data = pred_df_last_stage2, aes(density_last, 
                                  ymin = fit - se.fit, 
                                  ymax = fit + se.fit),
              alpha = 0.2) +
  geom_line(data = pred_df_last_stage2, aes(density_last, fit)) +
  labs(y = "nchoice/ntrials") +
  theme_bw()

ggsave("plots/structure_density_lastchoice.png")
