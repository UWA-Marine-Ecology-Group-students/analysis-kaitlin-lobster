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
struct_dens_clean$trial_lvl <- as.factor(paste(struct_dens_clean$lower, struct_dens_clean$higher, sep = "_"))
struct_dens_clean$firsthigh <- ifelse(struct_dens_clean$first.choice > struct_dens_clean$alternative, 1, 0)
struct_dens_clean$lasthigh  <- ifelse(struct_dens_clean$final.choice > struct_dens_clean$alternative, 1, 0)
struct_dens_clean$lob_num   <- gsub("[A-Z]", "", struct_dens_clean$lobster)

struct_dens_clean$perc_dif  <- (struct_dens_clean$higher - struct_dens_clean$lower) / 
  struct_dens_clean$higher

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

firstchoice_bylevel$ntrials <- trials$ntest

head(firstchoice_bylevel)

# plot the main effect we're interested in
ggplot(firstchoice_bylevel, aes(first.choice, nfirstchoice/ntrials)) + 
  geom_point() +
  theme_bw()

# this looks like a logarithmic curve to me - try adding a log line of best fit
ggplot(firstchoice_bylevel, aes(first.choice, nfirstchoice/ntrials)) + 
  geom_point() + 
  geom_smooth(method = glm, formula = y ~ log(x + 1), se = FALSE) +
  theme_bw()

ggsave("plots/p_firstchoice.png")

# model this to check its robustness
m1 <- glm(cbind(nsuccess, ntrials - nsuccess) ~ log(nfirstchoice + 1), 
          data = firstchoice_bylevel, family = "binomial")
summary(m1)
plot(m1)

# now we need to make predictions from this model back onto our data, so we can make a final plot

#I'm leaving this for now.




# Keeping all the below workings just in case we need to try another way
# but in the end I decided it was probably overkill.
# 
# 
# firstchoice_bytrial <- summarise(group_by(struct_dens_clean, trial_lvl),
#                                  lower = mean(lower)/32,
#                                  higher = mean(higher)/32,
#                                  firsthigh = sum(firsthigh),
#                                  ntrials = n())
# firstchoice_bytrial$diff <- firstchoice_bytrial$higher - firstchoice_bytrial$lower
# 
# head(firstchoice_bytrial)
# 
# # plot what we're aiming to model
# 
# 
# ggplot(struct_dens_clean, aes(first.choice/32)) + 
#   geom_bar(position = "dodge")
# 
# 
# 
# ggplot(struct_dens_clean, aes(as.factor(first.choice), colour = as.factor(trial_lvl))) + 
#   geom_bar(position = "dodge") 
# 
# 
# ggplot(struct_dens_clean, aes(higher, firsthigh)) + 
#   geom_point() + geom_smooth(se = FALSE)
# 
# ggplot(struct_dens_clean, aes(firsthigh, group = as.factor(higher))) + 
#   geom_bar() 
# 
# 
# ggplot(firstchoice_bytrial, aes(higher, firsthigh/ntrials)) + 
#   geom_point() + geom_smooth(se = FALSE)
# 
# ggplot(firstchoice_bytrial, aes(lower, firsthigh/ntrials)) + 
#   geom_point() + geom_smooth(se = FALSE)
# 
# 
# # try to model this
# m1 <- glm(cbind(firsthigh, ntrials-firsthigh) ~ higher + lower, 
#           data = firstchoice_bytrial, family = "binomial")
# summary(m1)
# 
# 
# m1 <- glm(cbind(firsthigh, ntrials-firsthigh) ~ higher/32, 
#           data = firstchoice_bytrial, family = "binomial")
# summary(m1)
# plot(m1)
# 
# 
# m1 <- glm(cbind(firsthigh, ntrials-firsthigh) ~ log(higher) + lower, 
#           data = firstchoice_bytrial, family = "binomial")
# summary(m1)
# plot(m1)
# 
# # ok - now try a model with random effects
# 
# 
# 
# m2 <- lmer(firsthigh ~ log(higher),
#            data = struct_dens_clean)
# summary(m2)
# 
# 
# 
# struct_dens_clean$difference <- struct_dens_clean$first.choice - struct_dens_clean$alternative
# 
# head(struct_dens_clean)
# summary(struct_dens_clean)
# 
# 
# 
# unique(c(struct_dens_clean$lower, struct_dens_clean$higher))
# 
# # Hypothesis: juvenile lobsters show preference for higher densities of seagrass
# 
# 
# ggplot(struct_dens_clean, aes(chosehigh)) +
#   geom_bar()
# 
# ggplot(struct_dens_clean, aes(higher, chosehigh)) + 
#   geom_jitter(alpha = 2/5) + 
#   geom_smooth()
# 
# ggplot(struct_dens_clean, aes(as.factor(higher), chosehigh)) + 
#   geom_violin() +
#   geom_boxplot(width = 0.1)
# 
# m1 <- glm(chosehigh ~ higher + lower, family = "binomial", data = struct_dens_clean)
# summary(m1)
# 
# 
# 
# # first lets plot the observations against a best-fit line
# 
# ggplot(firstchoice, aes(difference, first_choice)) + 
#   geom_jitter(alpha = 2/5) + 
#   geom_smooth()
# 
# 
# ggplot(finalchoice, aes(difference, final_choice)) + 
#   geom_jitter(alpha = 2/5) + 
#   geom_smooth()
# 
# 
# 
# firstchoice <- data.frame("first_choice" = struct_dens_clean$first.choice,
#                           "alternative" = c(struct_dens_clean$density.A + 
#                                             struct_dens_clean$density.B) - 
#                                             struct_dens_clean$first.choice,
#                           "round" = struct_dens_clean$round, 
#                           "trial" = struct_dens_clean$trial,
#                           "ymaze" = struct_dens_clean$ymaze, 
#                           "lobster" = struct_dens_clean$lobster)
# firstchoice$difference <- firstchoice$first_choice - firstchoice$alternative
# head(firstchoice)
# 
# # final choice wrangling
# 
# finalchoice <- data.frame("final_choice" = struct_dens_clean$final.choice,
#                           "alternative" = c(struct_dens_clean$density.A + 
#                                               struct_dens_clean$density.B) - 
#                             struct_dens_clean$final.choice,
#                           "round" = struct_dens_clean$round,
#                           "trial" = struct_dens_clean$trial,
#                           "ymaze" = struct_dens_clean$ymaze, 
#                           "lobster" = struct_dens_clean$lobster)
# finalchoice$difference <- finalchoice$final_choice - finalchoice$alternative
# head(finalchoice)
# 
# 
# 
# # test a basic model
# 
# m1 <- glm(first_choice ~ alternative + difference, data = firstchoice)
# summary(m1)
# plot(m1)
# 
# # ok we need to add a random effect for the lobster and the maze
# 
# m2 <- lmer(first_choice ~ difference + alternative, data = firstchoice)
# summary(m2)
# 
# 
# m1 <- glm(higher ~ first_choice + alternative, 
#           family = "binomial", data = firstchoice)
# 


