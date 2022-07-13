# Regression Analysis DOC

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

#Multiplot function for GGally
my_fn <- function(data,mapping, method="loess",...){
  p <- ggplot(data = data, mapping = mapping) +
    geom_point() +
    geom_smooth(method=method, ...)
  p
}


#Loading the data and making some internal arrangement
hjs <- read.csv("220621_fho_hjandrews.csv")
hjs$dt <- as.Date(hjs$dt, origin = "1899-12-30", format = "%Y-%m-%d")
hjs$ws.f <- factor(hjs$ws.f,levels = c("Old-growth","Logged"))
hjs$ssn <- factor(hjs$ssn,levels = c("Fall","Winter","Spring","Summer"))

hjm <- dplyr::filter(hjs,wy>1983)


#DOC time series and collection

doc_uq_ts <- ggplot(hjm,aes(x=as.Date(dt),y=uq.cm, color = ws.f))+
  geom_line()+
  geom_point(data=dplyr::filter(hjs,is.na(hjs$doc.mg_l)==FALSE),aes(x=as.Date(dt),y=uq.cm))+
  scale_color_manual(values = c("#00BFC4","#F8766D"), name = "Forest type")+
  facet_wrap(~ws.f)
doc_uq_ts
  

#Representativeness

doc_uq_bp <- ggplot(hjm,aes(x=ws.f,y=uq.cm, color = ws.f, fill = ws.f))+
  geom_boxplot(alpha = 0.5)+
  geom_point(data=dplyr::filter(hjs,is.na(hjs$doc.mg_l)==FALSE),aes(x=ws.f,y=uq.cm))+
  scale_color_manual(values = c("#00BFC4","#F8766D"), name = "Forest type")+
  scale_fill_manual(values = c("#00BFC4","#F8766D"), name = "Forest type")+
  scale_y_log10()+
  theme_fg
doc_uq_bp


doc_uq_tss <- ggplot(hjm,aes(x=uq.cm,y=sed.mg_l, color = ws.f, fill = ws.f))+
  geom_point(alpha = 0.25, size = 3.5)+
  geom_point(data=dplyr::filter(hjs,is.na(hjs$doc.mg_l)==FALSE),aes(x=uq.cm,y=sed.mg_l), size = 2.5)+
  scale_color_manual(values = c("#00BFC4","#F8766D"), name = "Forest type")+
  scale_fill_manual(values = c("#00BFC4","#F8766D"), name = "Forest type")+
  scale_x_log10()+
  scale_y_log10()+
  facet_wrap(~ws.f)+
  theme_fg
doc_uq_tss

doc_uq_slc <- ggplot(hjm,aes(x=uq.cm,y=slc.mg_l, color = ws.f, fill = ws.f))+
  geom_point(alpha = 0.25, size = 3.5)+
  geom_point(data=dplyr::filter(hjs,is.na(hjs$doc.mg_l)==FALSE),aes(x=uq.cm,y=slc.mg_l), size = 2.5)+
  scale_color_manual(values = c("#00BFC4","#F8766D"), name = "Forest type")+
  scale_fill_manual(values = c("#00BFC4","#F8766D"), name = "Forest type")+
  scale_x_log10()+
  scale_y_log10()+
  facet_wrap(~ws.f)+
  theme_fg
doc_uq_slc


require(GGally)

p_dat <- dplyr::select(hjm,ws.f,dt,ssn,log.uq,log.sed.mg_l,log.slc.mg_l,log.don.mg_l,log.doc.mg_l)

p <- ggpairs(p_dat, columns = 4:8, ggplot2::aes(colour = ws.f, alpha = 0.6),
             lower = list(continuous = wrap(my_fn, method="lm", se=FALSE)))
for(i in 1:p$nrow){
  for(j in 1:p$ncol){
    p[i,j] <- p[i,j]+
      scale_fill_manual(values= c("#00BFC4","#F8766D"))+
      scale_color_manual(values= c("#00BFC4","#F8766D"))
  }
}
p

# Looking at DOC seasonal patterns per watershed

p1 <- ggpairs(dplyr::filter(p_dat,ws.f=="Old-growth"), columns = 4:8, ggplot2::aes(colour = ssn, alpha = 0.6),
             lower = list(continuous = wrap(my_fn, method="lm", se=FALSE)))
for(i in 1:p$nrow){
  for(j in 1:p$ncol){
    p[i,j] <- p[i,j]
  }
}
p1


p2 <- ggpairs(dplyr::filter(p_dat,ws.f=="Logged"), columns = 4:8, ggplot2::aes(colour = ssn, alpha = 0.6),
              lower = list(continuous = wrap(my_fn, method="lm", se=FALSE)))
for(i in 1:p$nrow){
  for(j in 1:p$ncol){
    p[i,j] <- p[i,j]
  }
}
p2


doc_mod <- lm(log.doc.mg_l ~ log.uq + log.sed.mg_l + (log.slc.mg_l + log.don.mg_l) * ssn + ws.f, 
              data = dplyr::filter(hjm,is.na(hjm$doc.mg_l)==FALSE))
summary(doc_mod)

hjm$p.doc.mg_l <- exp(predict.lm(doc_mod,newdata = hjm,na.action = na.pass))
cm <- lm(doc.mg_l~p.doc.mg_l,hjm)
hjm$p.doc.mg_l <- (cm$coefficients[1] + hjm$p.doc.mg_l)/cm$coefficients[2]
hjm$log.p.doc.mg_l <- log10(hjm$p.doc.mg_l)

obs_prd_doc <- ggplot(hjm, aes(x=doc.mg_l,y=p.doc.mg_l,color = ws.f))+
  geom_point()+
  geom_abline()+
  scale_fill_manual(values= c("#00BFC4","#F8766D"))+
  scale_color_manual(values= c("#00BFC4","#F8766D"))
obs_prd_doc  


m_dat <- dplyr::select(hjm,ws.f,dt,ssn,log.uq,log.sed.mg_l,log.slc.mg_l,log.don.mg_l,log.p.doc.mg_l)
m_dat$ws.f <- factor(m_dat$ws.f,levels = c("Old-growth","Logged"))
m_dat$ssn <- factor(m_dat$ssn,levels = c("Fall","Winter","Spring","Summer"))

p3 <- ggpairs(dplyr::filter(m_dat,ws.f=="Old-growth"), columns = 4:8, ggplot2::aes(colour = ssn, alpha = 0.6),
             lower = list(continuous = wrap(my_fn, method="lm", se=FALSE)))
for(i in 1:p$nrow){
  for(j in 1:p$ncol){
    p[i,j] <- p[i,j]
  }
}
p3


p4 <- ggpairs(dplyr::filter(m_dat,ws.f=="Logged"), columns = 4:8, ggplot2::aes(colour = ssn, alpha = 0.6),
              lower = list(continuous = wrap(my_fn, method="lm", se=FALSE)))
for(i in 1:p$nrow){
  for(j in 1:p$ncol){
    p[i,j] <- p[i,j]
  }
}
p4


#Comparing seasonal dynamics between watersheds

pdoc_ssn <- ggplot(hjm, aes(x=p.doc.mg_l, color = ws.f, fill = ws.f))+
  geom_density(alpha = 0.5)+
  scale_fill_manual(values= c("#00BFC4","#F8766D"))+
  scale_color_manual(values= c("#00BFC4","#F8766D"))+
  facet_wrap(~ssn, nrow = 2)
pdoc_ssn

pdoc_ssn_p <- ggplot(hjm,aes(x=slc.mg_l,p.doc.mg_l, color = ssn, fill =ssn, size = uq.cm))+
  geom_path(data = hjs, aes(x=slc.mg_l,p.doc.mg_l), alpha=0.25, size = 0.38, inherit.aes = FALSE)+
  geom_point(alpha = 0.75)+
  facet_wrap(~ws.f)
pdoc_ssn_p


pdoc_ssn_ts <- ggplot(hjm,aes(x=as.Date(dt),y=p.doc.mg_l, color = ws.f, fill = ws.f))+
  geom_line(size = 0.5, alpha = 0.5)+
  geom_smooth(span=0.10)+
  scale_y_log10()
pdoc_ssn_ts
  
  
pdoc_ssn_ts <- ggplot(hjm,aes(x=as.Date(dt),y=p.doc.mg_l))+
  geom_line(size = 0.5, alpha = 0.5)+
  geom_point(data=hjm,aes(x=as.Date(dt),y=p.doc.mg_l, color = ssn), size = 2.5)+
  # scale_y_log10()+
  facet_wrap(~ws.f)
pdoc_ssn_ts 
  

#Additional predictors

p<-ggplot(filter(hjm, log.utn.mg_l<0.2), aes(log.uq, log.doc.mg_l, color = ws.f))+
  # geom_smooth(method = "lm")+
  geom_point()
p

