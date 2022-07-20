###
# Project: Seagrass as habitat for juvenile lobsters
# Data:    Structure vs scent trials
# Task:    Data wrangling and modelling for first/final choice data
# author:  Kaitlin McCloghry, Kingsley Griffin
# date:    April-July 2022


library(tidyverse)
library(reshape2)
library(ggplot2)
library(lme4)
library(dplyr)
library(tidyr)

# read in data
structure_vs_scent <- read.csv("data/Structure_vs_scent_20.07.2022.csv")

#view data
head(structure_vs_scent)
str(structure_vs_scent)
summary(structure_vs_scent)

# tidy up the data
struct_vs_scent_clean <- filter(structure_vs_scent, Useable.data == 1)
head(struct_vs_scent_clean)





#FIRST CHOICE

#count first choice by level
structure_scent_first <- sum(struct_vs_scent_clean$first.choice == 'st/sc')
structure_first       <- sum(struct_vs_scent_clean$first.choice == 'structure')
scent_first           <- sum(struct_vs_scent_clean$first.choice == 'scent')
blank_first           <- sum(struct_vs_scent_clean$first.choice == 'blank')

#create matrix with table info
matrix_first <- matrix(c(structure_scent_first, structure_first, scent_first, blank_first), ncol=2, byrow=TRUE)

#define column names and row names of matrix
colnames(matrix_first) <- c('scent', 'no scent')
rownames(matrix_first) <- c('structure', 'no structure')

#convert matrix to table 
table_first <- as.table(matrix_first)

#view table 
table_first

#table as proportions
prop_table_first <- prop.table(table_first)
prop_table_first

#Chi-squared test
chisq_first <- chisq.test(table_first)
chisq_first

#open file path to save plot
png(filename="C:/Users/kaitl/Documents/Kaitlin's Stuff/Dissertation/analysis-kaitlin-lobster/plots/structure_vs_scent_firstchoice.png")

#create barplot
barplot(prop_table_first, beside = TRUE, 
        xlab = 'presence of scent', ylab = 'proportion of final choice', 
        legend.text = TRUE, args.legend = 
          list(x = "topright",inset = c(0, 0), cex = 0.75))

dev.off()





#FINAL CHOICE

#replace NAs with 'neither' to not mess with next bit
struct_vs_scent_replaced <- struct_vs_scent_clean %>% replace_na(list(final.choice = 'neither')) 

#count final choice by level
structure_scent_final <- sum(struct_vs_scent_replaced$final.choice == 'st/sc')
structure_final       <- sum(struct_vs_scent_replaced$final.choice == 'structure')
scent_final           <- sum(struct_vs_scent_replaced$final.choice == 'scent')
blank_final           <- sum(struct_vs_scent_replaced$final.choice == 'blank')

structure_scent_final
structure_final
scent_final
blank_final

#create matrix with table info
matrix_final <- matrix(c(structure_scent_final, structure_final, scent_final, blank_final), ncol=2, byrow=TRUE)

#define column names and row names of matrix
colnames(matrix_final) <- c('scent', 'no scent')
rownames(matrix_final) <- c('structure', 'no structure')

#convert matrix to table 
table_final <- as.table(matrix_final)

#view table 
table_final

#table as proportions
prop_table_final <- prop.table(table_final)
prop_table_final

#Chi-squared test
chisq_final <- chisq.test(table_final)
chisq_final

#open file path to save plot
png(filename="C:/Users/kaitl/Documents/Kaitlin's Stuff/Dissertation/analysis-kaitlin-lobster/plots/structure_vs_scent_finalchoice.png")

#create barplot
barplot(prop_table_final, beside = TRUE, 
        xlab = 'presence of scent', ylab = 'proportion of final choice', 
        legend.text = TRUE, args.legend = 
          list(x = "topright",inset = c(0, 0), cex = 0.75))

dev.off()

