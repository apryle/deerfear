levels(purevidACC$behaviorcat)

WP2MF2all <- purevidACC[purevidACC$deerID== "WP2MF2" & purevidACC$behaviorcat=="RunOnly", ]

spy<-ggplot(WP2MF2all, aes(x=WP2MF2all$ACCSeconds, y=WP2MF2all$accy, group=DeerDate)) + 
  geom_line(aes(color=DeerDate))+
  ggtitle("y-axis raw ACC") +
  theme(legend.position = "none")



NW2MF9all <- purevidACC[purevidACC$deerID== "NW2MF9" & purevidACC$behaviorcat=="RunOnly", ]

spy<-ggplot(NW2MF9all, aes(x=NW2MF9all$ACCSeconds, y=NW2MF9all$accy, group=DeerDate)) + 
  geom_line(aes(color=DeerDate))+
  ggtitle("y-axis raw ACC") +
  theme(legend.position = "none")

################ same deer different behavior

NW2MF9vig <- purevidACC[purevidACC$deerID== "NW2MF9" & purevidACC$behaviorcat=="VigilantOnly", ]

spy<-ggplot(NW2MF9vig, aes(x=NW2MF9vig$ACCSeconds, y=NW2MF9vig$accy, group=DeerDate)) + 
  geom_line(aes(color=DeerDate))+
  ggtitle("y-axis raw ACC vigilant") +
  theme(legend.position = "none")



############## WHy is the mypsd function breaking?
############# we have some with 0 variance such as NW2MF14_20150106_073224

NW2MF14_20150106_073224 <- purevidACC[purevidACC$DeerDate == "NW2MF14_20150106_073224", ]

head(NW2MF14_20150106_073224)

spx<-ggplot(NW2MF14_20150106_073224, 
            aes(x=NW2MF14_20150106_073224$ACCSeconds, 
                y=NW2MF14_20150106_073224$accx)) + 
                  geom_line()+ 
                  ggtitle("x-axis raw ACC vigilant") 

spy<-ggplot(NW2MF14_20150106_073224, 
            aes(x=NW2MF14_20150106_073224$ACCSeconds, 
                y=NW2MF14_20150106_073224$accy)) + 
  geom_line()+ 
  ggtitle("y-axis raw ACC vigilant") 

spz<-ggplot(NW2MF14_20150106_073224, 
            aes(x=NW2MF14_20150106_073224$ACCSeconds, 
                y=NW2MF14_20150106_073224$accz)) + 
  geom_line()+ 
  ggtitle("z-axis raw ACC vigilant") 

head(NW2MF14_20150106_073224)
specx <- spec.ar(NW2MF14_20150106_073224$accx, method="ols", plot=FALSE)
specy <- spec.ar(NW2MF14_20150106_073224$accy, method="ols", plot=FALSE)
specz <- spec.ar(NW2MF14_20150106_073224$accz, method="ols", plot=FALSE)

specz$spec

specxAR <- spectrum(NW2MF14_20150106_073224$accx, method="ar")

ar.yw(NW2MF14_20150106_073224$accy, AIC=FALSE, order.max=5, demean=FALSE, detrend=FALSE)
