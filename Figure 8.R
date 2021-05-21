library(tseries)
library(forecast)
library(dplyr)
library(ks)
library(plyr)
library(ggplot2)
library(dplyr)
library(car)
library(data.table)
#setwd("G:/My Drive/RCPP/Data/SHAT/metabolism modeling/R_code_for modeling")
#alldata<-read.csv("All Metabolism_Metabolizer_2008-2017_041519_CTL_estimated light.csv")
alldata<-read.csv("All Metabolism_Metabolizer_2008-2017_61919_CTL.csv")
alldata$Treatment <- factor(alldata$Treatment, levels = c("Pre", "Post"))
#alldata<-read.csv("SDW_2018 output.csv")
#alldata<-read.csv("All Metabolism_Rcode_2007-2017_080918_CTL.csv")
#alldata<-read.csv("2017.2016comparison.csv")
dt<-data.table(alldata)
dt.out<-dt[,list(GPP.mean=mean(GPP, na.rm=TRUE),GPP.sd=sd(GPP, na.rm=TRUE), GPP.count=length(GPP), 
                 ER.mean=mean(ER, na.rm=TRUE),ER.sd=sd(ER, na.rm=TRUE), ER.count=length(ER)),
           by=c("Treatment","Year")]



compare<-read.csv("metcompLINX.csv")
compare<-subset(compare, LU=="AGR")

###550 vs 500
###Figure 2
plot8<-ggplot(dt.out, aes(x=GPP.mean, y=ER.mean))+
  geom_abline(intercept = 0, slope = -1.2,color="black",size=1)+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.title.x=element_text(size=16, color="black"))+
  theme(axis.title.y=element_text(size=16, color="black"))+
  theme(axis.text.y=element_text(size=16, color="black"))+
  theme(axis.text.x=element_text(size=16, color="black"))+
  geom_point(data=compare,aes(x=GPP, y=R, shape=LU), size=3, color="black")+
  geom_point(aes(fill=Treatment),color="black",size=4,shape=21,stroke=1)+
  xlab(bquote('GPP ('*g~O[2]~m^-2~d^-1*')'))+
  ylab(bquote('ER ('*g~O[2]~m^-2~d^-1*')'))+
  scale_fill_manual(values=c('gray85', "olivedrab3"))+
  scale_y_continuous(limits=c(-17,0),breaks=seq(-15,0,5))+
# scale_y_continuous(limits=c(0,-17),breaks=seq(0,-15,-5),expand=c(0,0))+
  theme(legend.text = element_text( size = 14),legend.title=element_text(size=14),#legend.position=c(0.9,.7),
        legend.background = element_blank())+
  guides(shape = guide_legend(title = "LINX II", nrow=1), fill = guide_legend(title = "This Study",nrow=1))+
  theme(legend.position="top")
  
ggsave(filename = 
      "G://My Drive/RCPP/Data/SHAT/metabolism modeling/Manuscript/Figures/Figure8.tiff",
       plot = plot8,
       dpi = 300,
      device= "tiff")

