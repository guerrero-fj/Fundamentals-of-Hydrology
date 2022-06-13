###########################################################################################################################################################
#   GEOCHEMICAL DATASET H.J.ANDREWS
###########################################################################################################################################################

#Note: Questionable values for SI, ALK, and COND were previoulsy removed.
#      Also, discontinous records (early 80's) for WS-1, 6, and 7 were removed

###################################################
#LOADING LIBRARIES AND SETTING WORKING DIRECTORIES
###################################################

rm(list=ls())#Cleaning global environment
gc()#garbage collector

#Loading libraries

library(ggplot2)
library(reshape2)
library(lubridate)
library(gridExtra)
library(grid)
library(plyr)
library(dplyr)
library(nlme)
library(doBy)
library(MASS)
library(lsmeans)
library(carData)
library(utils)
library(multcompView)
library(VIF)
library(cowplot)
library(ggExtra)


hjg<- read.csv("200114_hja_gchm_cont.csv", na.strings = "")#all data points including high suspended sediment loads >800mg.l

#Season function
getSeason <- function(DATES) {
  WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Fall")))
}

#Adding season to the dataset

hjg$ssn <- getSeason(as.Date(hjg$dt,origin = "1899-12-30", format = "%Y-%m-%d"))


#Phase graph for conductivity and silica

#Temporal changes in hydrological paths (Conductivity)- All watersheds
aw_thp_fig <- ggplot(hjg, aes(y=slc.mg_l, x=log(q.cm),color = ws))+
  # geom_path(alpha=0.5,size=0.5)+
  geom_point(alpha=0.6)+
  ylab(expression(paste("Dissolved Silica (mg ",l^{-1}, ")"))) +
  xlab(expression(paste("Electrical conductivity (",mu,"S",s^{-1}, ")")))+
  theme(axis.text=element_text(colour="black"),
        text = element_text(size = 12),
        axis.title = element_text(size=18),
        legend.text = element_text(size = 16),
        panel.grid.minor= element_line(colour = "gray", linetype = "dotted"), 
        panel.grid.major = element_line(colour = "gray", linetype = "dashed"),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        panel.background=element_blank(),
        axis.ticks.length = unit(0.254, "cm"),
        axis.ticks = element_line(colour = "black", size=1), 
        axis.line = element_line(colour = "black"))+
   facet_wrap(~ws)
aw_thp_fig


#Temporal changes in hydrological paths (Conductivity)
wd_thp_fig <- ggplot(hjg, aes(y=slc.mg_l, x=cnd.us_s,size=q.cm,color = ssn))+
  geom_path(alpha=0.2,size=0.8)+
  geom_point(alpha=0.6)+
  ylab(expression(paste("Dissolved Silica (mg ",l^{-1}, ")"))) +
  xlab(expression(paste("Electrical conductivity (",mu,"S",s^{-1}, ")")))+
  theme(axis.text=element_text(colour="black"),
        text = element_text(size = 16),
        axis.title = element_text(size=18),
        legend.text = element_text(size = 16),
        panel.grid.minor= element_line(colour = "gray", linetype = "dotted"), 
        panel.grid.major = element_line(colour = "gray", linetype = "dashed"),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        panel.background=element_blank(),
        axis.ticks.length = unit(0.254, "cm"),
        axis.ticks = element_line(colour = "black", size=1), 
        axis.line = element_line(colour = "black"))+
  facet_wrap(~ws)
wd_thp_fig

#Other geochemical tracers (See Feller (2005) for a review of the role of the different ions and cations in Western 
#Forests and Sollins et al., 1980 for elemental cycling in WS-10; Also Luo et al. for interpretations of Mg/Na vs
#Ca/Na bivariate plots)
hjg$Si_Na <- with(hjg,slc.mg_l/sod.mg_l)
hjg$Si_Ca <- with(hjg,slc.mg_l/cal.mg_l)
hjg$Na_Ca <- with(hjg,sod.mg_l/cal.mg_l)
hjg$Ca_Na <- with(hjg,cal.mg_l/sod.mg_l)
hjg$Mg_Na <- with(hjg,mg.mg_l/sod.mg_l)

#Checking for outliers
p <- ggplot(hjg,aes(ws,Si_Na))+
  geom_boxplot()
p

#Ploting without oultliers 
hjg_wo <- filter(hjg,Si_Ca<10 & Si_Na<15)


p <- ggplot(hjg_wo,aes(ws,Ca_Na))+
  geom_boxplot()
p


wd_geo_fig <- ggplot(hjg_wo, aes(y=Si_Na, x=Ca_Na,size=q.cm,color = ws))+
  geom_point(alpha=0.6)+
  ylab("Si/Na ratio") +
  xlab("Ca/Na ratio")+
  scale_color_manual(name = "Watershed",values = c("#F8766D", "#D39200", "#93AA00","#00BA38",
                                                   "#00C19F", "#00B9E3", "#619CFF" ,"#5e3c99","#e66101"))+
  scale_x_continuous(limits = c(0.5,1.3))+
  theme(axis.text=element_text(colour="black"),
        text = element_text(size = 16),
        axis.title = element_text(size=18),
        legend.text = element_text(size = 16),
        panel.grid.minor= element_line(colour = "gray", linetype = "dotted"), 
        panel.grid.major = element_line(colour = "gray", linetype = "dashed"),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        panel.background=element_blank(),
        axis.ticks.length = unit(0.254, "cm"),
        axis.ticks = element_line(colour = "black", size=1), 
        axis.line = element_line(colour = "black"))+
  facet_wrap(~ssn)
wd_geo_fig

hjg_wo <- filter(hjg_wo, mg.mg_l/sod.mg_l < 3)

#Marx et al., 2017. Headwaters geochemistry
wd_geo_fig <- ggplot(hjg_wo, aes(y=Mg_Na, x=Ca_Na,size=q.cm,color = ws))+
  geom_point(alpha=0.6)+
  ylab("Mg/Na ratio") +
  xlab("Ca/Na ratio")+
  scale_color_manual(name = "Watershed",values = c("#F8766D", "#D39200", "#93AA00","#00BA38",
                                                   "#00C19F", "#00B9E3", "#619CFF" ,"#5e3c99","#e66101"))+
  scale_x_continuous(limits = c(0.5,3.0))+
  scale_y_continuous(limits = c(0.1,1.0))+
  # scale_x_log10(limits = c(0.1,10))+
  theme(axis.text=element_text(colour="black"),
        text = element_text(size = 16),
        axis.title = element_text(size=18),
        legend.text = element_text(size = 16),
        panel.grid.minor= element_line(colour = "gray", linetype = "dotted"), 
        panel.grid.major = element_line(colour = "gray", linetype = "dashed"),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        panel.background=element_blank(),
        axis.ticks.length = unit(0.254, "cm"),
        axis.ticks = element_line(colour = "black", size=1), 
        axis.line = element_line(colour = "black"))
wd_geo_fig



wd_geo_910_fig <- ggplot(filter(hjg_wo, ws=="GSWS09"| ws=="GSWS10"), aes(y=slc.mg_l/sod.mg_l, x=cal.mg_l/sod.mg_l,size=q.cm,color = ws))+
  geom_point(alpha=0.6)+
  ylab("Si/Na ratio") +
  xlab("Ca/Na ratio")+
  # scale_y_log10(limits = c(0.001, 10))+
  # scale_x_log10(limits = c(0.001, 100))+
  scale_color_manual(name = "Watershed",values = c("#5e3c99","#e66101"), guide = "none")+
  scale_size(name= "Unit discharge")+
  theme(axis.text=element_text(colour="black"),
        text = element_text(size = 16),
        axis.title = element_text(size=18),
        legend.text = element_text(size = 16),
        panel.grid.minor= element_line(colour = "gray", linetype = "dotted"), 
        panel.grid.major = element_line(colour = "gray", linetype = "dashed"),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        panel.background=element_blank(),
        axis.ticks.length = unit(0.254, "cm"),
        axis.ticks = element_line(colour = "black", size=1), 
        axis.line = element_line(colour = "black"))+
        theme(legend.position = c(0.15,0.85))
  # facet_wrap(~ssn)
wd_geo_910_fig

wd_geo_910_fig1 <- ggMarginal(wd_geo_910_fig, groupColour = TRUE, groupFill = TRUE)
wd_geo_910_fig1


#Relationship between discharge and Si, Ca, and Na ratios
q_si_na_910_fig <- ggplot(filter(hjg_wo, ws=="GSWS09"| ws=="GSWS10"), aes(y=slc.mg_l/sod.mg_l, x=q.cm,color = ws))+
  geom_point(alpha=0.6)+
  ylab("Si/Na ratio") +
  xlab("Unit Discharge")+
  # scale_y_log10(limits = c(0.001, 10))+
  scale_x_log10(limits = c(0.001, 100))+
  scale_color_manual(name = "Watershed",values = c("#5e3c99","#e66101"), guide = "none")
q_si_na_910_fig 

q_ca_na_910_fig <- ggplot(filter(hjg_wo, ws=="GSWS09"| ws=="GSWS10"), aes(y=cal.mg_l/sod.mg_l, x=q.cm,color = ws))+
  geom_point(alpha=0.6)+
  ylab("Ca/Na ratio") +
  xlab("Unit Discharge")+
  # scale_y_log10(limits = c(0.001, 10))+
  scale_x_log10(limits = c(0.001, 100))+
  scale_color_manual(name = "Watershed",values = c("#5e3c99","#e66101"), guide = "none")
q_ca_na_910_fig 

q_si_cnd_910_fig <- ggplot(filter(hjg_wo, ws=="GSWS09"| ws=="GSWS10"), aes(y=slc.mg_l/cnd.us_s, x=q.cm,color = ws))+
  geom_point(alpha=0.6)+
  ylab("Si/Conductivity ratio") +
  xlab("Unit Discharge")+
  # scale_y_log10(limits = c(0.001, 10))+
  scale_x_log10(limits = c(0.001, 100))+
  scale_color_manual(name = "Watershed",values = c("#5e3c99","#e66101"), guide = "none")
q_si_cnd_910_fig 

q_si_chl_910_fig <- ggplot(filter(hjg_wo, ws=="GSWS09"| ws=="GSWS10"), aes(y=slc.mg_l/chl.mg_l, x=q.cm,color = ws))+
  geom_point(alpha=0.6)+
  ylab("Si/Cl ratio") +
  xlab("Unit Discharge")+
  scale_y_continuous(limits = c(0,20))+
  scale_x_log10(limits = c(0.001, 100))+
  scale_color_manual(name = "Watershed",values = c("#5e3c99","#e66101"), guide = "none")
q_si_chl_910_fig 

SiNa_t_910_fig <- ggplot(filter(hjg_wo, ws=="GSWS09"| ws=="GSWS10"), aes(y=slc.mg_l/sod.mg_l, x= as.Date(dt,origin = "1899-12-30"),color = ws))+
  geom_line()+
  ylab("Si/Na ratio") +
  xlab("Water year")+
  # scale_y_log10(limits = c(0.001, 10))+
  # scale_x_log10(limits = c(0.001, 100))+
  scale_color_manual(name = "Watershed",values = c("#5e3c99","#e66101"), guide = "none")+
  scale_size(name= "Unit discharge")+
  theme(axis.text=element_text(colour="black"),
        text = element_text(size = 16),
        axis.title = element_text(size=18),
        legend.text = element_text(size = 16),
        panel.grid.minor= element_line(colour = "gray", linetype = "dotted"), 
        panel.grid.major = element_line(colour = "gray", linetype = "dashed"),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        panel.background=element_blank(),
        axis.ticks.length = unit(0.254, "cm"),
        axis.ticks = element_line(colour = "black", size=1), 
        axis.line = element_line(colour = "black"))+
  theme(legend.position = c(0.15,0.85))+
  facet_wrap(~ws,nrow=2)
SiNa_t_910_fig 

require(cowplot)
SiNa_plot <- plot_grid(SiNa_t_910_fig,wd_geo_910_fig1,ncol = 2)
SiNa_plot

tiff(filename = "SiNa_Plot.tiff",
     units="in",
     width=14.0,
     height=6.5,
     pointsize=5,
     compression = 'lzw',
     res=300)
grid.arrange(SiNa_plot, ncol = 1,heights=c(3.85))
dev.off()

hjg_wo <- filter(hjg,Si_Na<10)

wd_geo_910p_fig <- ggplot(filter(hjg_wo, ws=="GSWS09"| ws=="GSWS10"), aes(y=slc.mg_l/sod.mg_l, x=cal.mg_l/sod.mg_l,size=q.cm,color = ssn))+
  geom_path(alpha=0.2,size=0.8)+
  geom_point(alpha=0.6)+
  ylab("Si/Na ratio") +
  xlab("Ca/Na ratio")+
  # scale_x_continuous(limits = c(0.3,1))+
  theme(axis.text=element_text(colour="black"),
        text = element_text(size = 16),
        axis.title = element_text(size=18),
        legend.text = element_text(size = 16),
        panel.grid.minor= element_line(colour = "gray", linetype = "dotted"), 
        panel.grid.major = element_line(colour = "gray", linetype = "dashed"),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        panel.background=element_blank(),
        axis.ticks.length = unit(0.254, "cm"),
        axis.ticks = element_line(colour = "black", size=1), 
        axis.line = element_line(colour = "black"))+
  facet_wrap(~ws)
wd_geo_910p_fig

#Temporal changes in elemental ratios

wy_SiNa_910p_fig <- ggplot(filter(hjg_wo, ws=="GSWS09"| ws=="GSWS10"), aes(y=Na_Ca, x= as.factor(wy), color = ws, fill = ws))+
  # geom_smooth(span=0.15, alpha = 0.25)+
  geom_boxplot(alpha=0.5)+
  ylab("Si/Na ratio") +
  xlab("Water year")+
  scale_colour_manual(name="Watershed",labels=c("Old-growth","Logged"),values = c("#5e3c99","#e66101"))+
  scale_fill_manual(name="Watershed",labels=c("Old-growth","Logged"),values = c("#5e3c99","#e66101"))+
  # scale_x_continuous(limits=c(1985,2017))+
  theme(axis.text=element_text(colour="black"),
        text = element_text(size = 16),
        axis.title = element_text(size=18),
        legend.text = element_text(size = 16),
        panel.grid.minor= element_line(colour = "gray", linetype = "dotted"), 
        panel.grid.major = element_line(colour = "gray", linetype = "dashed"),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        panel.background=element_blank(),
        axis.ticks.length = unit(0.254, "cm"),
        axis.ticks = element_line(colour = "black", size=1), 
        axis.line = element_line(colour = "black"))+
   facet_wrap(~ws, nrow = 2)
wy_SiNa_910p_fig

wy_SiNa_910p_fig <- ggplot(filter(hjg_wo, ws=="GSWS09"| ws=="GSWS10"), aes(y=slc.mg_l/sod.mg_l, x= as.Date(dt,origin = "1899-12-30"), color = ws, fill = ws))+
  geom_line()+
  ylab("Si/Na ratio") +
  xlab("Water year")+
  scale_colour_manual(name="Watershed",labels=c("Old-growth","Logged"),values = c("#5e3c99","#e66101"))+
  scale_fill_manual(name="Watershed",labels=c("Old-growth","Logged"),values = c("#5e3c99","#e66101"))+
  # scale_x_continuous(limits=c(1985,2017))+
  theme(axis.text=element_text(colour="black"),
        text = element_text(size = 16),
        axis.title = element_text(size=18),
        legend.text = element_text(size = 16),
        panel.grid.minor= element_line(colour = "gray", linetype = "dotted"), 
        panel.grid.major = element_line(colour = "gray", linetype = "dashed"),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        panel.background=element_blank(),
        axis.ticks.length = unit(0.254, "cm"),
        axis.ticks = element_line(colour = "black", size=1), 
        axis.line = element_line(colour = "black"))#+
  #facet_wrap(~ws, nrow = 2)
wy_SiNa_910p_fig

wy_SiCa_910p_fig <- ggplot(filter(hjg_wo, ws=="GSWS09"| ws=="GSWS10"), aes(y=slc.mg_l/cal.mg_l, x= as.Date(dt,origin = "1899-12-30"), color = ws, fill = ws))+
  geom_line()+
  ylab("Si/Ca ratio") +
  xlab("Water year")+
  scale_colour_manual(name="Watershed",labels=c("Old-growth","Logged"),values = c("#5e3c99","#e66101"))+
  scale_fill_manual(name="Watershed",labels=c("Old-growth","Logged"),values = c("#5e3c99","#e66101"))+
  # scale_x_continuous(limits=c(1985,2017))+
  theme(axis.text=element_text(colour="black"),
        text = element_text(size = 16),
        axis.title = element_text(size=18),
        legend.text = element_text(size = 16),
        panel.grid.minor= element_line(colour = "gray", linetype = "dotted"), 
        panel.grid.major = element_line(colour = "gray", linetype = "dashed"),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        panel.background=element_blank(),
        axis.ticks.length = unit(0.254, "cm"),
        axis.ticks = element_line(colour = "black", size=1), 
        axis.line = element_line(colour = "black"))#+
#  facet_wrap(~ws, nrow = 2)
wy_SiCa_910p_fig

wy_CaNa_910p_fig <- ggplot(filter(hjg_wo, ws=="GSWS09"| ws=="GSWS10"), aes(y=cal.mg_l/sod.mg_l, x= as.Date(dt,origin = "1899-12-30"), color = ws, fill = ws))+
  # geom_point()+
  geom_line()+
  # geom_smooth(span=0.15, alpha = 0.25)+
  ylab("Ca/Na ratio") +
  xlab("Water day")+
  scale_colour_manual(name="Watershed",labels=c("Old-growth","Logged"),values = c("#5e3c99","#e66101"))+
  scale_fill_manual(name="Watershed",labels=c("Old-growth","Logged"),values = c("#5e3c99","#e66101"))+
  # scale_x_continuous(limits=c(1985,2017))+
  theme(axis.text=element_text(colour="black"),
        text = element_text(size = 16),
        axis.title = element_text(size=18),
        legend.text = element_text(size = 16),
        panel.grid.minor= element_line(colour = "gray", linetype = "dotted"), 
        panel.grid.major = element_line(colour = "gray", linetype = "dashed"),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        panel.background=element_blank(),
        axis.ticks.length = unit(0.254, "cm"),
        axis.ticks = element_line(colour = "black", size=1), 
        axis.line = element_line(colour = "black"))#+
  #facet_wrap(~ws, nrow = 2)
wy_CaNa_910p_fig

wy_SiNa_fig <- ggplot(hjg_wo, aes(y=slc.mg_l/sod.mg_l, x= as.Date(dt,origin = "1899-12-30"), color = ws, fill = ws))+
  geom_line()+
  ylab("Si/Na ratio") +
  xlab("Water year")+
  scale_color_manual(name = "Watershed",values = c("#F8766D", "#D39200", "#93AA00","#00BA38",
                                                   "#00C19F", "#00B9E3", "#619CFF" ,"#5e3c99","#e66101"))+
  # scale_x_continuous(limits=c(1985,2017))+
  theme(axis.text=element_text(colour="black"),
        text = element_text(size = 16),
        axis.title = element_text(size=18),
        legend.text = element_text(size = 16),
        panel.grid.minor= element_line(colour = "gray", linetype = "dotted"), 
        panel.grid.major = element_line(colour = "gray", linetype = "dashed"),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        panel.background=element_blank(),
        axis.ticks.length = unit(0.254, "cm"),
        axis.ticks = element_line(colour = "black", size=1), 
        axis.line = element_line(colour = "black"))+
  facet_wrap(~ws, nrow = 5, ncol = 2)
wy_SiNa_fig

wy_CaNa_910p_fig <- ggplot(filter(hjg_wo, ws=="GSWS09"| ws=="GSWS10"), aes(y=cal.mg_l/sod.mg_l, x= as.Date(dt,origin = "1899-12-30"), color = ws, fill = ws))+
  # geom_point()+
  geom_line()+
  # geom_smooth(span=0.15, alpha = 0.25)+
  ylab("Ca/Na ratio") +
  xlab("Water day")+
  scale_colour_manual(name="Watershed",labels=c("Old-growth","Logged"),values = c("#5e3c99","#e66101"))+
  scale_fill_manual(name="Watershed",labels=c("Old-growth","Logged"),values = c("#5e3c99","#e66101"))+
  # scale_x_continuous(limits=c(1985,2017))+
  theme(axis.text=element_text(colour="black"),
        text = element_text(size = 16),
        axis.title = element_text(size=18),
        legend.text = element_text(size = 16),
        panel.grid.minor= element_line(colour = "gray", linetype = "dotted"), 
        panel.grid.major = element_line(colour = "gray", linetype = "dashed"),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        panel.background=element_blank(),
        axis.ticks.length = unit(0.254, "cm"),
        axis.ticks = element_line(colour = "black", size=1), 
        axis.line = element_line(colour = "black"))+
  facet_wrap(~ws, nrow = 2)
wy_CaNa_910p_fig

tiff(filename = "HJA_Growndwater_Paths_1.tiff",
     units="in",
     width=18,
     height=10,
     pointsize=5,
     compression = 'lzw',
     res=300)
grid.arrange(wd_thp_fig, ncol = 1,heights=c(3.85))
dev.off()

#Looking over summer changes across watersheds
wd_thp_smr_fig <- ggplot(filter(hjg,ssn=="Summer"), aes(y=slc.mg_l, x=wy,color = ws))+
  geom_vline(xintercept = 2007)+
  geom_smooth(span=0.15, se = FALSE)+
  ylab(expression(paste("Dissolved Silica (mg ",l^{-1}, ")"))) +
  xlab("Water year")+
  scale_x_continuous(limits=c(1980,2018))+
  scale_color_manual(name = "Watershed",values = c("#F8766D", "#D39200", "#93AA00","#00BA38",
                                                   "#00C19F", "#00B9E3", "#619CFF" ,"#5e3c99","#e66101"))+
  # scale_color_discrete(name="Watershed")+
  theme(axis.text=element_text(colour="black"),
        axis.title = element_text(size = 18),
        text = element_text(size = 16),
        panel.grid.minor= element_line(colour = "gray", linetype = "dotted"), 
        panel.grid.major = element_line(colour = "gray", linetype = "dashed"),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        panel.background=element_blank(),
        axis.ticks.length = unit(0.254, "cm"),
        axis.ticks = element_line(colour = "black", size=1), 
        axis.line = element_line(colour = "black"),
        legend.position = "none")+
  # facet_wrap(~ws)+
  ggtitle("Average summer dissolved [Si] H.J. Andrews (1980-2018)")
wd_thp_smr_fig 

# a <- as.data.frame(ggplot_build(wd_thp_smr_fig)$data)
# b <- unique(a$colour.1)


wd_smr_q_fig <- ggplot(filter(hjg,ssn=="Summer"), aes(y=q.cm, x=wy,color = ws))+
  geom_smooth(span=0.15, se = FALSE)+
  ylab("Average summer unit discharge (cm)") +
  xlab("Water year")+
  scale_x_continuous(limits=c(1980,2018))+
  scale_color_manual(name = "Watershed",values = c("#F8766D", "#D39200", "#93AA00","#00BA38",
                                                   "#00C19F", "#00B9E3", "#619CFF" ,"#5e3c99","#e66101"))+
  # scale_color_discrete(name="Watershed")+
  geom_vline(xintercept = 2007)+
  theme(axis.text=element_text(colour="black"),
        axis.title = element_text(size = 18),
        text = element_text(size = 16),
        panel.grid.minor= element_line(colour = "gray", linetype = "dotted"), 
        panel.grid.major = element_line(colour = "gray", linetype = "dashed"),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        panel.background=element_blank(),
        axis.ticks.length = unit(0.254, "cm"),
        axis.ticks = element_line(colour = "black", size=1), 
        axis.line = element_line(colour = "black"), 
        legend.position = c(0.3,0.8))+
  ggtitle("Average summer discharge in the H.J. Andrews (1980-2018)")
wd_smr_q_fig 

require (cowplot)
srm_hyd_fig <- plot_grid(wd_thp_smr_fig ,wd_smr_q_fig , ncol = 2)

tiff(filename = "Summer_Hydrology.tiff",
     units="in",
     width=18,
     height=10,
     pointsize=5,
     compression = 'lzw',
     res=300)
grid.arrange(srm_hyd_fig, ncol = 1,heights=c(3.85))
dev.off()

#Watershed 9 vs 10

hjg_cp <- filter(hjg,ws=="GSWS09"|ws=="GSWS10")
hjg_cp <- filter(hjg_cp,wy>1980)
hjg_cp <- filter(hjg_cp,wy<2017)

wd_thp_910_fig <- ggplot(hjg_cp, aes(y=slc.mg_l, x=cnd.us_s,size=q.cm,color = ws))+
  geom_path(alpha=0.2,size=0.8)+
  geom_point(alpha=0.6)+
  ylab(expression(paste("Dissolved Silica (mg ",l^{-1}, ")"))) +
  xlab(expression(paste("Electrical conductivity (",mu,"S",s^{-1}, ")")))+
  scale_colour_manual(name="Watershed",labels=c("Old-growth","Logged"),values = c("#5e3c99","#e66101"))+
  theme(axis.text=element_text(colour="black", size = 16),
        axis.title = element_text(size = 18),
        text = element_text(size = 12),
        panel.grid.minor= element_line(colour = "gray", linetype = "dotted"), 
        panel.grid.major = element_line(colour = "gray", linetype = "dashed"),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        panel.background=element_blank(),
        axis.ticks.length = unit(0.254, "cm"),
        axis.ticks = element_line(colour = "black", size=1), 
        axis.line = element_line(colour = "black"),
        legend.position = c(0.15,0.85),
        legend.background = element_blank())
wd_thp_910_fig 

tiff(filename = "WS09&WS10_hydropath.tiff",
     units="in",
     width=6.0,
     height=6.5,
     pointsize=5,
     compression = 'lzw',
     res=300)
grid.arrange(wd_thp_910_fig , ncol = 1,heights=c(3.85))
dev.off()


#Changes by season

wd_thp_910_ssn_fig <- ggplot(hjg_cp, aes(y=slc.mg_l, x=wy,color = ws, fill = ws))+
  # geom_point(alpha=0.1)+
  geom_smooth(span=0.15, alpha = 0.25)+
  ylab(expression(paste("Dissolved Silica (mg ",l^{-1}, ")"))) +
  xlab("Water year")+
  scale_colour_manual(name="Watershed",labels=c("Old-growth","Logged"),values = c("#5e3c99","#e66101"))+
  scale_fill_manual(name="Watershed",labels=c("Old-growth","Logged"),values = c("#5e3c99","#e66101"))+
  scale_x_continuous(limits=c(1985,2017))+
  theme(axis.text=element_text(colour="black"),
        axis.title = element_text(size = 18),
        text = element_text(size = 16),
        panel.grid.minor= element_line(colour = "gray", linetype = "dotted"), 
        panel.grid.major = element_line(colour = "gray", linetype = "dashed"),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        panel.background=element_blank(),
        axis.ticks.length = unit(0.254, "cm"),
        axis.ticks = element_line(colour = "black", size=1), 
        axis.line = element_line(colour = "black"),
        legend.position = "none")+
   facet_wrap(~ssn,ncol=2, scales = "free_y")
wd_thp_910_ssn_fig 

tiff(filename = "WS09&WS10_Si_historical.tiff",
     units="in",
     width=6.0,
     height=6.5,
     pointsize=5,
     compression = 'lzw',
     res=300)
grid.arrange(wd_thp_910_ssn_fig  , ncol = 1,heights=c(3.85))
dev.off()


#Zooming into summer season

wd_thp_910_smr_fig <- ggplot(filter(hjg_cp,ssn=="Summer"), aes(y=slc.mg_l, x=wy,color = ws))+
  geom_point(alpha=0.1)+
  geom_smooth(span=0.15)+
  ylab(expression(paste("Dissolved Silica (mg ",l^{-1}, ")"))) +
  xlab("Water year")+
  scale_colour_manual(name="Watershed",labels=c("Old-growth","Logged"),values = c("#5e3c99","#e66101"))+
  scale_x_continuous(limits=c(1980,2017))+
  theme(axis.text=element_text(colour="black"),
        text = element_text(size = 10),
        panel.grid.minor= element_line(colour = "gray", linetype = "dotted"), 
        panel.grid.major = element_line(colour = "gray", linetype = "dashed"),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        panel.background=element_blank(),
        axis.ticks.length = unit(0.254, "cm"),
        axis.ticks = element_line(colour = "black", size=1), 
        axis.line = element_line(colour = "black"))#+
wd_thp_910_smr_fig 


#Looking for temporal changes in hydrological paths
wd_thp_910_wy_fig <- ggplot(hjg_cp, aes(y=slc.mg_l, x=cnd.us_s,size=q.cm,color = ws))+
  geom_path(alpha=0.2,size=0.8)+
  geom_point(alpha=0.6)+
  ylab(expression(paste("Dissolved Silica (mg ",l^{-1}, ")"))) +
  xlab(expression(paste("Electrical conductivity (",mu,"S",s^{-1}, ")")))+
  scale_colour_manual(name="Watershed",labels=c("Old-growth","Logged"),values = c("#5e3c99","#e66101"))+
  theme(axis.text=element_text(colour="black"),
        text = element_text(size = 10),
        panel.grid.minor= element_line(colour = "gray", linetype = "dotted"), 
        panel.grid.major = element_line(colour = "gray", linetype = "dashed"),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        panel.background=element_blank(),
        axis.ticks.length = unit(0.254, "cm"),
        axis.ticks = element_line(colour = "black", size=1), 
        axis.line = element_line(colour = "black"))+
  facet_wrap(~wy,ncol=7)
wd_thp_910_wy_fig 


#Changes by Season DON
wd_thp_910_ssn_fig <- ggplot(hjg_cp, aes(y=don.mg_l, x=wy,color = ws, fill = ws))+
  # geom_point(alpha=0.1)+
  geom_smooth(span=0.15, alpha = 0.25)+
  ylab(expression(paste("Dissolved Organic Nitrogen (mg ",l^{-1}, ")"))) +
  xlab("Water year")+
  scale_colour_manual(name="Watershed",labels=c("Old-growth","Logged"),values = c("#5e3c99","#e66101"))+
  scale_fill_manual(name="Watershed",labels=c("Old-growth","Logged"),values = c("#5e3c99","#e66101"))+
  scale_x_continuous(limits=c(1985,2017))+
  theme(axis.text=element_text(colour="black"),
        axis.title = element_text(size = 18),
        text = element_text(size = 16),
        panel.grid.minor= element_line(colour = "gray", linetype = "dotted"), 
        panel.grid.major = element_line(colour = "gray", linetype = "dashed"),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        panel.background=element_blank(),
        axis.ticks.length = unit(0.254, "cm"),
        axis.ticks = element_line(colour = "black", size=1), 
        axis.line = element_line(colour = "black"),
        legend.position = "none")+
 facet_wrap(~ssn,ncol=2, scales = "free_y")
wd_thp_910_ssn_fig 


#Changes by Season Unit Discharge
wd_thp_910_ssn_fig <- ggplot(hjg_cp, aes(y=q.cm, x=wy,color = ws, fill = ws))+
  # geom_point(alpha=0.1)+
  geom_smooth(span=0.15, alpha = 0.25)+
  ylab("Unit Discharge (cm)") +
  xlab("Water year")+
  scale_colour_manual(name="Watershed",labels=c("Old-growth","Logged"),values = c("#5e3c99","#e66101"))+
  scale_fill_manual(name="Watershed",labels=c("Old-growth","Logged"),values = c("#5e3c99","#e66101"))+
  scale_x_continuous(limits=c(1985,2017))+
  theme(axis.text=element_text(colour="black"),
        axis.title = element_text(size = 18),
        text = element_text(size = 16),
        panel.grid.minor= element_line(colour = "gray", linetype = "dotted"), 
        panel.grid.major = element_line(colour = "gray", linetype = "dashed"),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        panel.background=element_blank(),
        axis.ticks.length = unit(0.254, "cm"),
        axis.ticks = element_line(colour = "black", size=1), 
        axis.line = element_line(colour = "black"),
        legend.position = "none")+
  facet_wrap(~ssn,ncol=2, scales = "free_y")
wd_thp_910_ssn_fig 

#Changes by Season TSS
wd_thp_910_ssn_fig <- ggplot(hjg_cp, aes(y=sed.mg_l, x=wy,color = ws, fill = ws))+
  # geom_point(alpha=0.1)+
  geom_smooth(span=0.15, alpha = 0.25)+
  ylab(expression(paste("Total Suspended Solids (mg ",l^{-1}, ")"))) +
  xlab("Water year")+
  scale_colour_manual(name="Watershed",labels=c("Old-growth","Logged"),values = c("#5e3c99","#e66101"))+
  scale_fill_manual(name="Watershed",labels=c("Old-growth","Logged"),values = c("#5e3c99","#e66101"))+
  scale_x_continuous(limits=c(1985,2017))+
  scale_y_log10()+
  theme(axis.text=element_text(colour="black"),
        axis.title = element_text(size = 18),
        text = element_text(size = 16),
        panel.grid.minor= element_line(colour = "gray", linetype = "dotted"), 
        panel.grid.major = element_line(colour = "gray", linetype = "dashed"),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        panel.background=element_blank(),
        axis.ticks.length = unit(0.254, "cm"),
        axis.ticks = element_line(colour = "black", size=1), 
        axis.line = element_line(colour = "black"),
        legend.position = "none")+
  facet_wrap(~ssn,ncol=2)
wd_thp_910_ssn_fig 

#Dissolved Silica and Discharge by Season
wd_thp_910_ssn_fig <- ggplot(hjg_cp, aes(y=slc.mg_l, x=q.cm,color = ssn, fill = ssn))+
  geom_point(alpha=0.5)+
  geom_smooth(span=0.15, alpha = 0.25, method = "lm")+
  ylab(expression(paste("Dissolve Silica (mg ",l^{-1}, ")"))) +
  xlab("Unit Discharge (cm)")+
  # scale_colour_manual(name="Watershed",labels=c("Old-growth","Logged"),values = c("#5e3c99","#e66101"))+
  # scale_fill_manual(name="Watershed",labels=c("Old-growth","Logged"),values = c("#5e3c99","#e66101"))+
  scale_x_log10()+
  scale_y_log10()+
  theme(axis.text=element_text(colour="black"),
        axis.title = element_text(size = 18),
        text = element_text(size = 16),
        panel.grid.minor= element_line(colour = "gray", linetype = "dotted"), 
        panel.grid.major = element_line(colour = "gray", linetype = "dashed"),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        panel.background=element_blank(),
        axis.ticks.length = unit(0.254, "cm"),
        axis.ticks = element_line(colour = "black", size=1), 
        axis.line = element_line(colour = "black"),
        legend.position = "none")+
  # facet_wrap(~ws,ncol=2)
  facet_wrap(wy~ws)
wd_thp_910_ssn_fig 


wd_thp_10_ssn_fig <- ggplot(filter(hjg_cp,ws=="GSWS10"), aes(y=slc.mg_l, x=q.cm,color = ssn, fill = ssn))+
  geom_point(alpha=0.5)+
  geom_smooth(span=0.15, alpha = 0.25, method = "lm", se = FALSE)+
  ylab(expression(paste("Dissolve Silica (mg ",l^{-1}, ")"))) +
  xlab("Unit Discharge (cm)")+
  scale_x_log10()+
  # scale_y_log10()+
  theme(axis.text=element_text(colour="black"),
        axis.title = element_text(size = 18),
        text = element_text(size = 16),
        panel.grid.minor= element_line(colour = "gray", linetype = "dotted"), 
        panel.grid.major = element_line(colour = "gray", linetype = "dashed"),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        panel.background=element_blank(),
        axis.ticks.length = unit(0.254, "cm"),
        axis.ticks = element_line(colour = "black", size=1), 
        axis.line = element_line(colour = "black"),
        legend.position = "none")+
  facet_wrap(~wy)
wd_thp_10_ssn_fig 


wd_thp_9_ssn_fig <- ggplot(filter(hjg_cp,ws=="GSWS09"), aes(y=slc.mg_l, x=q.cm,color = ssn, fill = ssn))+
  geom_point(alpha=0.5)+
  geom_smooth(span=0.15, alpha = 0.25, method = "lm", se = FALSE)+
  ylab(expression(paste("Dissolve Silica (mg ",l^{-1}, ")"))) +
  xlab("Unit Discharge (cm)")+
  scale_x_log10()+
  # scale_y_log10()+
  theme(axis.text=element_text(colour="black"),
        axis.title = element_text(size = 18),
        text = element_text(size = 16),
        panel.grid.minor= element_line(colour = "gray", linetype = "dotted"), 
        panel.grid.major = element_line(colour = "gray", linetype = "dashed"),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        panel.background=element_blank(),
        axis.ticks.length = unit(0.254, "cm"),
        axis.ticks = element_line(colour = "black", size=1), 
        axis.line = element_line(colour = "black"),
        legend.position = "none")+
  facet_wrap(~wy)
wd_thp_9_ssn_fig 



#####
# Relationships with suspended sediments
#


tss_si <- ggplot(hjg_cp, aes(slc.mg_l,sed.mg_l, fill = ws))+
  geom_point()+
  facet_wrap(~ws)+
  scale_y_log10()
tss_si

tss_si_na <- ggplot(hjg_cp, aes(Mg_Na,sed.mg_l, fill = ws))+
  geom_point()+
  facet_wrap(~ws)+
  scale_x_log10()+
  scale_y_log10()
tss_si_na


model <- lm(log(ptn.mg_l)~log(sed.mg_l)+log(q.cm)+log(cnd.us_s)+log(slc.mg_l)+log(Si_Ca)+ssn+ws,na.omit(hjg_cp))
summary(model)
