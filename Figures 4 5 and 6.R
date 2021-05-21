library(tidyr)
library(ggplot2)
library(chron)
library(scales)
library(dplyr)
library(data.table)
library(quantreg)
##Figures were mdae at 850 vs 350 without axes
#Load Data
ts.SDW<-read.csv("full metabolism.csv")
ts.SDW$year<-as.factor(ts.SDW$year)
ts.SDW$treatment <- factor(ts.SDW$treatment, levels = c("Pre","Post"))
ts.SDW$precip.cat <- factor(ts.SDW$precip.cat, levels = c("Zero","Elevated", "High"))
ts.SDW$class <- factor(ts.SDW$class, levels = c("Early CC","Late CC", "No CC"))

####Figure 4####
##Plot Turbidity by cover crop class and precipitation category
ggplot(ts.SDW, aes(x=precip.cat, y=turb, fill=treatment))+
  theme_classic()+
  geom_boxplot(outlier.size=1,coef = 15)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.title.x=element_text(size=15,color="black"))+
  theme(axis.title.y=element_text(size=16,color="black"))+
  theme(axis.text.y=element_text(size=16,color="black"))+
  theme(axis.text.x=element_text(size=15,color="black"))+
  ylab(bquote('Turbidity (NTU)'))+
  theme(legend.title=element_text(size=14),
        legend.text = element_text( size = 16))
  xlab(bquote('Precipitation'))+
  scale_fill_manual(values= c("gray85", "olivedrab3"),labels=c("Pre", "Post"))+
  facet_grid(~class)+
  labs(fill="Treatment")+
  theme(strip.text.x = element_text(size = 15))+
  scale_y_continuous( trans="log10")+
  theme(legend.position="top")+
  scale_x_discrete(labels= c("Zero", "Bottom \n 75%", "Top \n 25%"))+
 theme(legend.position="NONE")


##Plot Dicharge (Q) by cover crop class and precipitation category  
ggplot(ts.SDW, aes(x=precip.cat, y=Q, fill=treatment))+
    theme_classic()+
    geom_boxplot(outlier.size=1,coef = 15)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(axis.title.x=element_text(size=15,color="black"))+
    theme(axis.title.y=element_text(size=16,color="black"))+
    theme(axis.text.y=element_text(size=16,color="black"))+
    theme(axis.text.x=element_text(size=15,color="black"))+
    ylab(bquote('Discharge ('*L~s^-1*')'))+
    theme(legend.title=element_text(size=14), 
          legend.text = element_text( size = 16))+
    xlab(bquote('Precipitation'))+
    scale_fill_manual(values= c("gray85", "olivedrab3"),labels=c("Pre", "Post"))+
    facet_grid(~class)+
    labs(fill="Treatment")+
    theme(strip.text.x = element_text(size = 15))+
    scale_y_continuous( trans="log10")+
    theme(legend.position="top")+
    scale_x_discrete(labels= c("Zero", "Bottom \n 75%", "Top \n 25%"))+
    theme(legend.position="NONE")
  
####Figure 5####
##Remove negative GPP values and positive ER
ts.SDW<- subset(ts.SDW,GPP >= 0)
ts.SDW<- subset(ts.SDW,ER < 0)


##Plot GPP by cover crop class and precipitation category  
ggplot(ts.SDW, aes(x=precip.cat, y=GPP, fill=treatment))+
  theme_classic()+
  geom_boxplot(outlier.size=1,coef = 15)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.title.x=element_text(size=15,color="black"))+
  theme(axis.title.y=element_text(size=16,color="black"))+
  theme(axis.text.y=element_text(size=16,color="black"))+
  theme(axis.text.x=element_text(size=15,color="black"))+
  ylab(bquote('GPP ('*g~ m^-2~d^-1*')'))+
  theme(legend.title=element_text(size=14), 
        legend.text = element_text( size = 16))+
  xlab(bquote('Precipitation'))+
  scale_fill_manual(values= c("gray85", "olivedrab3"),labels=c("Pre", "Post"))+
  facet_grid(~class)+
  labs(fill="Treatment")+
  theme(strip.text.x = element_text(size = 15))+
  theme(legend.position="top")+
  scale_x_discrete(labels= c("Zero", "Bottom \n 75%", "Top \n 25%"))+
  theme(legend.position="NONE")

##Plot ER by cover crop class and precipitation category  
ggplot(ts.SDW, aes(x=precip.cat, y=ER, fill=treatment))+
  theme_classic()+
  geom_boxplot(outlier.size=1,coef = 15)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.title.x=element_text(size=15,color="black"))+
  theme(axis.title.y=element_text(size=16,color="black"))+
  theme(axis.text.y=element_text(size=16,color="black"))+
  theme(axis.text.x=element_text(size=15,color="black"))+
  ylab(bquote('ER ('*g~ m^-2~d^-1*')'))+
  theme(legend.title=element_text(size=14), 
        legend.text = element_text( size = 16))+
  xlab(bquote('Precipitation'))+
  scale_fill_manual(values= c("gray85", "olivedrab3"),labels=c("Pre", "Post"))+
  facet_grid(~class)+
  labs(fill="Treatment")+
  theme(strip.text.x = element_text(size = 15))+
  theme(legend.position="top")+
  scale_x_discrete(labels= c("Zero", "Bottom \n 75%", "Top \n 25%"))+
  theme(legend.position="NONE")


####Figure 6####
#Load Data
mydata<-read.csv("HR_AR_metabolism.csv")
mydata$Year<-as.factor(mydata$Year)
mydata$Treatment <- factor(mydata$Treatment, levels = c("Pre","Post"))
mydata$precip.cat <- factor(mydata$precip.cat, levels = c("Zero","Elevated", "High"))

##Plot HR by cover crop class and precipitation category  
HR<-subset(mydata, metabo.type=="HR" & Rate <0)
ggplot(HR, aes(x=precip.cat, y=Rate, fill=Treatment))+
  theme_classic()+
  geom_boxplot(outlier.size=1,coef = 15)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.title.x=element_text(size=15,color="black"))+
  theme(axis.title.y=element_text(size=16,color="black"))+
  theme(axis.text.y=element_text(size=16,color="black"))+
  theme(axis.text.x=element_text(size=15,color="black"))+
  ylab(bquote('Heterotrophic R ('*g~ m^-2~d^-1*')'))+
  theme(legend.title=element_text(size=14), 
        legend.text = element_text( size = 16))+
  xlab(bquote('Precipitation'))+
  scale_fill_manual(values= c("gray85", "olivedrab3"),labels=c("Pre", "Post"))+
  labs(fill="Treatment")+
  theme(strip.text.x = element_text(size = 15))+
  scale_y_continuous(limits=c(-25, 2))+
  theme(legend.position="top")+
  facet_grid(~class)+
  scale_x_discrete(labels= c("Zero", "Bottom \n 70%", "Top \n 30%"))+
  theme(legend.position="NONE")

##Plot AR by cover crop class and precipitation category  
AR<-subset(mydata, metabo.type=="AR" & Rate <0)
ggplot(AR, aes(x=precip.cat, y=Rate, fill=Treatment))+
  theme_classic()+
  geom_boxplot(outlier.size=1,coef = 15)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.title.x=element_text(size=15,color="black"))+
  theme(axis.title.y=element_text(size=16,color="black"))+
  theme(axis.text.y=element_text(size=16,color="black"))+
  theme(axis.text.x=element_text(size=15,color="black"))+
  ylab(bquote('Heterotrophic R ('*g~ m^-2~d^-1*')'))+
  theme(legend.title=element_text(size=14), 
        legend.text = element_text( size = 16))+
  xlab(bquote('Precipitation'))+
  scale_fill_manual(values= c("gray85", "olivedrab3"),labels=c("Pre", "Post"))+
  labs(fill="Treatment")+
  theme(strip.text.x = element_text(size = 15))+
  scale_y_continuous(limits=c(-15, 0))+
  theme(legend.position="top")+
  facet_grid(~class)+
  scale_x_discrete(labels= c("Zero", "Bottom \n 70%", "Top \n 30%"))+
  theme(legend.position="NONE")
