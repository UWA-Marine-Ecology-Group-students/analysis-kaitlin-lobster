###
# Project: Seagrass as habitat for juvenile lobsters
# Data:    Structure density trials
# Task:    Data wrangling and modelling for first/final choice data
# author:  Kaitlin McCloghry, Kingsley Griffin
# date:    April-July 2022
##

library(tidyverse)
library(reshape2)
library(ggplot2)
library(lme4)
# library(ggpubr)
# library(broom)
# library(AICcmodavg)

# read in data
structure_density <- read.csv("data/structure_density_copy.csv")

#view data
head(structure_density)
# View(structure_density)
str(structure_density)
summary(structure_density)

# tidy up the data
struct_dens_clean <- filter(structure_density, Useable.data == 1)
head(struct_dens_clean)

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

firstchoice_bylevel <- summarise(group_by(struct_dens_clean, first.choice),
                                 nfirstchoice = n())
head(firstchoice_bylevel)

lower_n  <- summarise(group_by(struct_dens_clean, lower), ntest = n())
higher_n <- summarise(group_by(struct_dens_clean, higher), ntest = n())
names(lower_n)[1]  <- "level"
names(higher_n)[1] <- "level"

trials <- merge(lower_n, higher_n, by = "level", all = TRUE)
trials$ntest.y[1] <- 0
trials$ntest.x[6] <- 0
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

ggsave("plots/p_firstchoice.png")

# model this to check its robustness
m1 <- glm(cbind(nsuccess, ntrials - nsuccess) ~ log(density + 1), 
          data = firstchoice_bylevel, family = "binomial")
summary(m1)
plot(m1)

# now we need to make predictions from this model back onto our data, so we can make a final plot

pred_df     <- data.frame("density" = seq(from = 0, to = 32, by = 1 ))
predictions <- as.data.frame(predict(m1, pred_df, type = "response", se = TRUE))
pred_df     <- cbind(pred_df, predictions[, 1:2])
head(pred_df)

ggplot() + 
  geom_point(data = firstchoice_bylevel, aes(density, nsuccess/ntrials), alpha = 0.5) +
  geom_ribbon(data = pred_df, aes(density, 
                                  ymin = fit - se.fit, 
                                  ymax = fit + se.fit),
              alpha = 0.2) +
  geom_line(data = pred_df, aes(density, fit)) +
  labs(y = "nchoice/ntrials") +
  theme_bw()

