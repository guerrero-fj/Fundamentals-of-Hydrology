# PRECIPITATION

library(ggplot2)
library(dplyr)
library(lubridate)


#Season function
getSeason <- function(DATES){
  WS <- as.Date("2012-12-15", format = "%Y-%m-%d")#Winter solstice
  SE <- as.Date("2012-3-15",  format = "%Y-%m-%d")#Spring equinox
  SS <- as.Date("2012-6-15",  format = "%Y-%m-%d")#Summer solstice
  FE <- as.Date("2012-9-15",  format = "%Y-%m-%d")#Fall Equinox
  
  #Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format = "2012-%m-%d"))
  
  ifelse(d >= WS | d < SE, "Winter",
         ifelse (d >= SE & d < SS, "Spring",
                 ifelse (d >= SS & d < FE, "Summer", "Fall")))
}


#Defining a common theme for our graphics
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

#Processing dataset
hjp_0 <- read.csv("220627_hja_precipt.csv")
hjp_1 <- dplyr::select(hjp_0,c("SITECODE","HEIGHT", "DATE", "PRECIP_TOT_DAY"))
hjp_2 <- dplyr::filter(hjp_1, HEIGHT != "455")
hjp_2$DATE <- as.Date(hjp_2$DATE, origin = "1899-12-30", format = "%Y-%m-%d")
hjp_2$SEASON <- getSeason(hjp_2$DATE)
colnames(hjp_2) <- c("site", "elev","date","p_mm_day","ssn")


p_select_plot <- ggplot(hjp_2, aes(date, p_mm_day, color = as.factor(elev), fill = as.factor(elev)))+
  geom_line()+
  ylab("Total precipitation (daily)")+
  xlab("Year")+
  theme_fg+
  facet_wrap(~as.factor(elev), ncol = 4)
p_select_plot

hjp_2$log.p_mm_day <- log10(hjp_2$p_mm_day+1)

p_select_plot2 <- ggplot(hjp_2, aes(x = log.p_mm_day, color = as.factor(elev), fill = as.factor(elev)))+
  geom_density(alpha = 0.35)+
  xlab("Total precipitation (daily)")+
  ylab("Estimated kernel density")+
  theme_fg+
  facet_wrap(~as.factor(elev), ncol = 4)
p_select_plot2

p_select_plot3 <- ggplot(hjp_2, aes(x = log.p_mm_day, color = as.factor(ssn), fill = as.factor(ssn)))+
  geom_density(alpha = 0.35)+
  xlab("Total precipitation (daily)")+
  ylab("Estimated kernel density")+
  theme_fg+
  facet_wrap(~as.factor(elev), ncol = 4)
p_select_plot3

hjp_2r <- dplyr::filter(hjp_2,log.p_mm_day > 0)

p_select_plot4 <- ggplot(hjp_2r, aes(x = log.p_mm_day, color = as.factor(ssn), fill = as.factor(ssn)))+
  geom_density(alpha = 0.35)+# perhaps you need to use a kernel density method to account for differences in record lenght.
  xlab("Total precipitation (daily)")+
  ylab("Estimated kernel density")+
  theme_fg+
  facet_wrap(~as.factor(elev), ncol = 4)
p_select_plot4

#Continue exploring temporal trends of seasonal changes. 