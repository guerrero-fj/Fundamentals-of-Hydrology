# Regression Analysis (DOC/PON)

library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(GGally)

#You need to install GGally package


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

p_dat <- dplyr::select(dat,ws.f,dt,ssn,prd,log.uq,log.sed.mg_l,log.slc.mg_l,log.ptn.mg_l,log.doc.mg_l)

require(GGally)
my_fn <- function(data,mapping, method="loess",...){
  p <- ggplot(data = data, mapping = mapping) +
    geom_point() +
    geom_smooth(method=method, ...)
  p
}

p <- ggpairs(p_dat, columns = 5:9, ggplot2::aes(colour = ws.f, alpha = 0.6),
             lower = list(continuous = wrap(my_fn, method="lm", se=FALSE)))
for(i in 1:p$nrow){
  for(j in 1:p$ncol){
    p[i,j] <- p[i,j]+
      scale_fill_manual(values= c("#00BFC4","#F8766D"))+
      scale_color_manual(values= c("#00BFC4","#F8766D"))
  }
}
p

