
library(ggplot2)
library(dplyr)
library(forcats)


working.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

data_dir <- paste(working.dir, "Data", sep="/")
setwd(working.dir)

###############################################
#       STRUCTURE DENSITY FIRST CHOICE        #
###############################################


density_first <- read.csv("C:/Users/kaitl/Documents/Kaitlin's Stuff/Dissertation/analysis-kaitlin-lobster/data/Plot_density_firstchoice.csv")

head(density_first)

density_first_plot <- ggplot(density_first, aes(x = xaxis.labels, y = prob.mean, ylim(-0.5, 0.5))) + 
                        geom_line(col="#183028") + 
                        geom_ribbon(aes(ymin = low.CI, ymax = up.CI), 
                           alpha=0.1, 
                           linetype="blank",
                           fill="#8DB2A6")+
                        theme_classic() +
                        expand_limits(y=c(0,0.25)) +
                        xlim(0,1200) +
                        theme(axis.line = element_line(color="gray57"),
                              axis.text.x = element_text(color = "gray57"),
                              axis.text.y = element_text(color = "gray57"),
                              axis.title = element_text(color = "gray57"),
                              axis.title.y = element_text(vjust = +4.75),
                              axis.title.x = element_text(vjust = -2.25),
                              plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))+
                        labs(x = bquote('Shoot density'~(m^2)), y = "Probability of choice relative to control")


density_first_plot




###############################################
#       STRUCTURE DENSITY FINAL CHOICE        #
###############################################



density_final <- read.csv("C:/Users/kaitl/Documents/Kaitlin's Stuff/Dissertation/analysis-kaitlin-lobster/data/Plot_density_finalchoice.csv")

head(density_final)

density_final_plot <- ggplot(density_final, aes(x = xaxis.labels, y = prob.mean, ylim(-0.5, 0.5))) + 
                        geom_line(col="#183028") + 
                        geom_ribbon(aes(ymin = low.CI, ymax = up.CI), 
                                    alpha=0.1, 
                                    linetype="blank",
                                    fill="#8DB2A6")+
                        theme_classic() +
                        expand_limits(y=c(0,0.25)) +
                        xlim(0,1200) +
                        theme(axis.line = element_line(color="gray57"),
                              axis.text.x = element_text(color = "gray57"),
                              axis.text.y = element_text(color = "gray57"),
                              axis.title = element_text(color = "gray57"),
                              axis.title.y = element_text(vjust = +4.75),
                              axis.title.x = element_text(vjust = -2.25),
                              plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))+
                        labs(x = bquote('Shoot density'~(m^2)), y = "Probability of choice relative to control")


density_final_plot








###############################################
#        STRUCTURE SCENT FIRST CHOICE         #
###############################################

structscent_first <- read.csv("C:/Users/kaitl/Documents/Kaitlin's Stuff/Dissertation/analysis-kaitlin-lobster/data/Plot_structscent_firstchoice.csv")

head(structscent_first)

structscent_first_plot <- structscent_first %>%
                             mutate(Treatment = fct_relevel(Treatment, 
                            "Scent", "Structure", "Both")) %>% 
                          ggplot(., aes(x=Treatment, y=prob.mean)) + 
                            geom_pointrange(aes(ymin=low.CI, ymax=up.CI, color = Treatment),  fatten = 2, size = 1.5) +
                            expand_limits(y=c(-0.5,0.5)) +
                            theme_classic() +
                            theme(axis.line = element_line(color="gray57"),
                                  axis.text.x = element_text(color = "gray57", size = 10),
                                  axis.text.y = element_text(color = "gray57", size = 10),
                                  axis.title = element_text(color = "gray57"),
                                  axis.title.y = element_text(vjust = +4.75, size = 12),
                                  axis.title.x = element_text(vjust = -2.25, size = 12),
                                  plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) +
                            labs(x = "Treatment", y = "Probability of choice relative to control") +
                            scale_color_manual(values = c("aquamarine4",
                                "slategray1",
                                "darkseagreen3")) + 
                            guides(fill="none", 
                                   color="none")
                            
structscent_first_plot



###############################################
#        STRUCTURE SCENT FINAL CHOICE         #
###############################################

structscent_final <- read.csv("C:/Users/kaitl/Documents/Kaitlin's Stuff/Dissertation/analysis-kaitlin-lobster/data/Plot_structscent_finalchoice.csv")

head(structscent_final)

structscent_final_plot <- structscent_final %>%
                             mutate(Treatment = fct_relevel(Treatment, 
                                 "Scent", "Structure", "Both")) %>% 
                          ggplot(., aes(x=Treatment, y=prob.mean)) + 
                            geom_pointrange(aes(ymin=low.CI, ymax=up.CI, color = Treatment),  fatten = 2, size = 1.5) +
                            expand_limits(y=c(-0.5,0.5)) +
                            theme_classic() +
                            theme(axis.line = element_line(color="gray57"),
                                  axis.text.x = element_text(color = "gray57", size = 10),
                                  axis.text.y = element_text(color = "gray57", size = 10),
                                  axis.title = element_text(color = "gray57"),
                                  axis.title.y = element_text(vjust = +4.75, size = 12),
                                  axis.title.x = element_text(vjust = -2.25, size = 12),
                                  plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) +
                            labs(x = "Treatment", y = "Probability of choice relative to control") +
                            scale_color_manual(values = c("aquamarine4",
                                                          "slategray1",
                                                          "darkseagreen3")) + 
                            guides(fill="none", 
                                   color="none")


structscent_final_plot






