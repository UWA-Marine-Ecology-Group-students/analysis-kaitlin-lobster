
install.packages("viridis")
install.packages("lemon")

library(ggplot2)
library(dplyr)
library(viridis)
library(lemon)

working.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

data_dir <- paste(working.dir, "Data", sep="/")
raster_dir <- paste(working.dir, "Raster_Class", sep="/")

setwd(working.dir)

bigpatch_res <- read.csv("C:/Users/kaitl/Documents/Kaitlin's Stuff/Dissertation/analysis-kaitlin-lobster/output/bigpatch_results.csv")


head(bigpatch_res)

bigpatch_summary <- bigpatch_res %>% 
  group_by(Year, CID) %>% 
  summarise_at(vars("No_Patches", "Mean_Area", "Mean_PAR", "Mean_Div"), list(mean=mean, sd=sd, se = ~sd(.)/sqrt(20))) %>% 
  mutate(Year=as.factor(Year))

head(bigpatch_summary)

##############################################
#          Mean number of patches            #
##############################################

# New facet label names for CID
CID.labs <- c("One year present", "Two years present", "Three years present")
names(CID.labs) <- c("1", "2", "3")


mean_patch_no_plot <- ggplot(bigpatch_summary)+
  geom_bar(aes(x=Year, y=No_Patches_mean,  fill=Year), stat="Identity")+
  geom_errorbar(aes(x=Year, ymin=No_Patches_mean-No_Patches_se, ymax=No_Patches_mean+No_Patches_se), width=.2,
                position=position_dodge(.9), colour="gray23")+
  facet_grid(~ CID,  labeller = labeller(CID = CID.labs))+
 # facet_wrap(~CID)+
 # facet_rep_grid(CID,
 #                labeller = labeller(CID = CID.labs))+
  scale_fill_manual("Year", values=c("aquamarine4", "slategray1", "darkseagreen3"))+
  theme_minimal()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color="gray57"),
        strip.text.x = element_text(color = "gray57"),
        strip.text.y = element_text(color = "gray57"),
        axis.text.x = element_text(color = "gray57"),
        axis.text.y = element_text(color = "gray57"),
        axis.title = element_text(color = "gray57"),
        axis.title.y = element_text(vjust = +4.75),
        axis.title.x = element_text(vjust = -2.25),
        line = element_line(color="gray23"),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        aspect.ratio = 1,
        strip.background = element_blank(),
        strip.placement = "outside")+
  labs(y="Mean Number of Patches")

mean_patch_no_plot



##############################################
#            Mean area of patches            #
##############################################

# New facet label names for CID
CID.labs <- c("One year present", "Two years present", "Three years present")
names(CID.labs) <- c("1", "2", "3")


Mean_Area_plot <- ggplot(bigpatch_summary)+
  geom_bar(aes(x=Year, y=Mean_Area_mean,  fill=Year), stat="Identity")+
  geom_errorbar(aes(x=Year, ymin=Mean_Area_mean-Mean_Area_se, ymax=Mean_Area_mean+Mean_Area_se), width=.2,
                position=position_dodge(.9), colour="gray23")+
  facet_grid(~ CID,  labeller = labeller(CID = CID.labs))+
  # facet_wrap(~CID)+
  # facet_rep_grid(CID,
  #                labeller = labeller(CID = CID.labs))+
  scale_fill_manual("Year", values=c("aquamarine4", "slategray1", "darkseagreen3"))+
  theme_minimal()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color="gray57"),
        strip.text.x = element_text(color = "gray57"),
        strip.text.y = element_text(color = "gray57"),
        axis.text.x = element_text(color = "gray57"),
        axis.text.y = element_text(color = "gray57"),
        axis.title = element_text(color = "gray57"),
        axis.title.y = element_text(vjust = +4.75),
        axis.title.x = element_text(vjust = -2.25),
        line = element_line(color="gray23"),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        aspect.ratio = 5/3,
        strip.background = element_blank(),
        strip.placement = "outside")+
  labs(y= bquote('Mean Area of Patches'~(m^2)))

Mean_Area_plot





##############################################################
#            Mean perimeter area ratio of patches            #
##############################################################

# New facet label names for CID
CID.labs <- c("One year present", "Two years present", "Three years present")
names(CID.labs) <- c("1", "2", "3")


Mean_PAR_plot <- ggplot(bigpatch_summary)+
  geom_bar(aes(x=Year, y=Mean_PAR_mean,  fill=Year), stat="Identity")+
  geom_errorbar(aes(x=Year, ymin=Mean_PAR_mean-Mean_PAR_se, ymax=Mean_PAR_mean+Mean_PAR_se), width=.2,
                position=position_dodge(.9), colour="gray23")+
  facet_grid(~ CID,  labeller = labeller(CID = CID.labs))+
  # facet_wrap(~CID)+
  # facet_rep_grid(CID,
  #                labeller = labeller(CID = CID.labs))+
  scale_fill_manual("Year", values=c("aquamarine4", "slategray1", "darkseagreen3"))+
  theme_minimal()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color="gray57"),
        strip.text.x = element_text(color = "gray57"),
        strip.text.y = element_text(color = "gray57"),
        axis.text.x = element_text(color = "gray57"),
        axis.text.y = element_text(color = "gray57"),
        axis.title = element_text(color = "gray57"),
        axis.title.y = element_text(vjust = +4.75),
        axis.title.x = element_text(vjust = -2.25),
        line = element_line(color="gray23"),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        aspect.ratio = 1,
        strip.background = element_blank(),
        strip.placement = "outside")+
  labs(y= bquote('Mean Perimeter Area Ratio'))

Mean_PAR_plot








#################################################
#           Mean Landscape Division             #
#################################################
# New facet label names for CID
CID.labs <- c("One year present", "Two years present", "Three years present")
names(CID.labs) <- c("1", "2", "3")


Mean_DIV_plot <- ggplot(bigpatch_summary)+
  geom_bar(aes(x=Year, y=Mean_Div_mean,  fill=Year), stat="Identity")+
  geom_errorbar(aes(x=Year, ymin=Mean_Div_mean-Mean_Div_se, ymax=Mean_Div_mean+Mean_Div_se), width=.2,
                position=position_dodge(.9), colour="gray23")+
  facet_grid(~ CID,  labeller = labeller(CID = CID.labs))+
  # facet_wrap(~CID)+
  # facet_rep_grid(CID,
  #                labeller = labeller(CID = CID.labs))+
  scale_fill_manual("Year", values=c("aquamarine4", "slategray1", "darkseagreen3"))+
  theme_minimal()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color="gray57"),
        strip.text.x = element_text(color = "gray57"),
        strip.text.y = element_text(color = "gray57"),
        axis.text.x = element_text(color = "gray57"),
        axis.text.y = element_text(color = "gray57"),
        axis.title = element_text(color = "gray57"),
        axis.title.y = element_text(vjust = +4.75),
        axis.title.x = element_text(vjust = -2.25),
        line = element_line(color="gray23"),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        aspect.ratio = 1,
        strip.background = element_blank(),
        strip.placement = "outside")+
  labs(y='Mean Landscape Division')

Mean_DIV_plot

















# New facet label names for window
window.labs <- c("2010 Neighbourhood Window", "2016 Neighbourhood Window", "2019 Neighbourhood Window")
names(window.labs) <- c("2010", "2016", "2019")

# New facet label names for CID
CID.labs <- c("One year present", "Two years present", "Three years present")
names(CID.labs) <- c("1", "2", "3")


Mean_DIV_plot <- ggplot(frag_class_summary)+
  geom_bar(aes(x=Year, y=Mean_Div_mean,  fill=Year), stat="Identity")+
  geom_errorbar(aes(x=Year, ymin=Mean_Div_mean-Mean_Div_se, ymax=Mean_Div_mean+Mean_Div_se), width=.2,
                position=position_dodge(.9), colour="gray23")+
  facet_rep_grid(Window~CID,
                 labeller = labeller(Window = window.labs, CID = CID.labs))+
  scale_fill_manual("Year", values=c("aquamarine4", "slategray1", "darkseagreen3"))+
  theme_minimal()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color="gray57"),
        strip.text.x = element_text(color = "gray57"),
        strip.text.y = element_text(color = "gray57"),
        axis.text.x = element_text(color = "gray57"),
        axis.text.y = element_text(color = "gray57"),
        axis.title = element_text(color = "gray57"),
        axis.title.y = element_text(vjust = +4.75),
        axis.title.x = element_text(vjust = -2.25),
        line = element_line(color="gray23"),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))+
  labs(y="Mean Probability of Landscape Division")

Mean_DIV_plot


