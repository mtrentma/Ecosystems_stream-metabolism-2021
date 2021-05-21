library(ggplot2)

mydata<-read.csv("SDW.storm.summary.forR.csv")
mydata$treatment <- factor(mydata$treatment, levels = c("Pre","Post"))

figure<-ggplot(mydata, aes(x=turb.increase, y=abs(gpp.decrease), fill=treatment))+
  theme_classic()+
  #geom_smooth(method=lm,se=FALSE, aes(color=treatment),size=2.5)+
  geom_point(aes(size=precip.mm), shape=21,stroke=1)+
  scale_size(range = c(3, 8))+
  #geom_point(color="black",size=6,stroke=1)+
  scale_shape_manual(values=c(21,22))+
  scale_colour_manual(values = c('gray85', "olivedrab3"))+
  scale_fill_manual(values = c('gray85', "olivedrab3"))+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.title.x=element_text(size=16, color="black"))+
  theme(axis.title.y=element_text(size=16, color="black"))+
  theme(axis.text.y=element_text(size=16, color="black"))+
  theme(axis.text.x=element_text(size=16, color="black"))+
  xlab("Turbidity increase (%)")+
  ylab(bquote('GPP decrease (%)'))+
  theme(# legend.key.width = unit(.5, "cm"), legend.key.height=unit(0.1, "cm"),
        legend.text = element_text( size = 14),legend.position=c(.85,.3),
        legend.background = element_blank(),legend.title=element_text(size=14),
        legend.box.background = element_rect(colour = "black"))+
  labs(fill="Treatment", size="Precip. (mm)")+
  guides(fill = guide_legend(order = 1,override.aes = list(size=5)),
         size = guide_legend(order = 2))
  
  
ggsave(filename = 
         "C:/Users/mtrentman/Documents/Data/SHAT/metabolism modeling/Manuscript/Ecosystems_submitted/Major Revisions/figure 7.tiff",
       plot = figure,
       dpi = 300,
       device= "tiff")

