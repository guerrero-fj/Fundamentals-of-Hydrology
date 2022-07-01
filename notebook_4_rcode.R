# Regression Analysis (DOC/PON)

library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)


theme_fg = theme(axis.text=element_text(colour="black",size=14),
                 axis.title.y = element_text(size = 20),
                 axis.title.x = element_text(size = 20),
                 panel.grid.minor= element_line(colour = "gray", linetype = "dotted"), 
                 panel.grid.major = element_line(colour = "gray", linetype = "dashed"),
                 panel.border = element_rect(fill=NA, colour = "black", size=1),
                 panel.background=element_blank(),
                 axis.ticks.length = unit(0.254, "cm"),
                 axis.ticks = element_line(colour = "black", size=1), 
                 axis.line = element_line(colour = "black"),
                 legend.position = c(.90,.90),
                 legend.direction = "vertical",
                 legend.background = element_rect(fill=alpha(0.1)),
                 legend.title = element_blank())

dat <- hjs
