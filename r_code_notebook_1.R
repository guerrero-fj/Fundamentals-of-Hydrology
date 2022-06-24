library(ggplot2)

hjg <- read.csv("220621_fho_hjandrews.csv")
hjg$ws.f <- factor(hjg$ws.f,levels = c("Old-growth","Logged"))
hjg$ssn <- factor(hjg$ssn,levels = c("Fall","Winter","Spring","Summer"))

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


uq_plot <- ggplot(hjg, aes(y=uq.cm, x=wd,color=as.factor(ws.f)))+
  geom_line(alpha=0.6, size = 0.8)+# by changing alpha, we change the transparency
  geom_smooth(span = 0.15, se = FALSE)+# by changing span, I control the wiggles in the trend
  ylab("Unit discharge (cm)") +
  xlab("Water day")+
  scale_color_manual(values = c("#00BFC4","#F8766D"))+
  scale_fill_manual(values = c("#00BFC4","#F8766D"))+
  theme_fg +
  facet_wrap(~ws.f)
uq_plot

uq_p_plot <- ggplot(hjg, aes(x=uq.cm,color= ws.f, fill = ws.f))+
  geom_density(alpha = 0.5, size = 1.0)+
  ylab("Kernel Density Estimate") +
  xlab("Unit discharge (cm)")+
  scale_color_manual(values = c("#00BFC4","#F8766D"))+
  scale_fill_manual(values = c("#00BFC4","#F8766D"))+
  theme_fg +
  facet_wrap(~ws.f)
uq_p_plot

uq_p_plot + scale_x_log10() + theme(legend.position = c(0.1,0.8))

ssn_plot <- ggplot(hjg, aes(x=uq.cm, color=as.factor(ssn), fill = as.factor (ssn)))+
  geom_density(alpha = 0.5)+
  ylab("Kernel density estimate") +
  xlab("Unit discharge (cm)")+
  scale_x_log10()+
  theme_fg +
  theme(legend.position = c(0.1,0.8))+
  facet_wrap(~ws.f)
ssn_plot

ssn_plot_c <- ggplot(hjg, aes(x=uq.cm, color=as.factor(ws.f), fill = as.factor (ws.f)))+
  geom_density(alpha = 0.2, size = 1.2)+
  ylab("Kernel density estimate") +
  xlab("Unit discharge (cm)")+
  scale_x_log10()+
  scale_color_manual(values = c("#00BFC4","#F8766D"))+
  scale_fill_manual(values = c("#00BFC4","#F8766D"))+
  theme_fg +
  theme(legend.position = c(0.1,0.8))+
  facet_wrap(~ssn, nrow = 2)
ssn_plot_c