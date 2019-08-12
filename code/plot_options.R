library(scales)
library(RColorBrewer)
#library(patchwork)

#preferred themes----
#no legend, x-axis labels at 45 angle
my_theme <- theme_classic() + 
  theme(legend.position = "none", axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), 
        plot.title = element_text(size=16,face="bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1))

#no legend, x-axis labels horizontal
my_theme_horiz <- theme_classic() + 
  theme(legend.position = "none", axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), 
        plot.title = element_text(size=16,face="bold"))

#legend, x-axis labels at 45 angle
my_theme_leg <- theme_classic() + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), 
        plot.title = element_text(size=16,face="bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1))

#legend at left, x-axis lables at 45 angle
my_theme_leg_left <- theme_classic() + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), 
        plot.title = element_text(size=16,face="bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "left")

#legend, x-axis labels horizontal
my_theme_leg_horiz <- theme_classic() + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), 
        plot.title = element_text(size=16,face="bold"))
