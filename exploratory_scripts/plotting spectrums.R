# plot some 
library("dplyr")
library("data.table")
library("ggplot2")

allspecs <- purebehaviorspectrums

head(allspecs)
table(allspecs$deerID)
table(allspecs$behaviorcat)

######### 1 deer, 2 behaviors
selectthisdeer <- "NW2MF14"
mydeer <- allspecs[allspecs$deerID == selectthisdeer,  ]
mydeerbehav <- mydeer[mydeer$behaviorcat == "RunOnly" | mydeer$behaviorcat == "VigilantOnly", ]

head(mydeerbehav)

spx<-ggplot(mydeerbehav, aes(x=freq, y=pow.x, group=DeerDate)) + 
  geom_line(aes(color=behaviorcat))+
  ggtitle(paste("x-axis", selectthisdeer))
spx

spy<-ggplot(mydeerbehav, aes(x=freq, y=pow.y, group=DeerDate)) + 
  geom_line(aes(color=behaviorcat))+
  ggtitle(paste("y-axis", selectthisdeer))
spy

spz<-ggplot(mydeerbehav, aes(x=freq, y=pow.z, group=DeerDate)) + 
  geom_line(aes(color=behaviorcat))+
  ggtitle(paste("z-axis", selectthisdeer))
spz

######### 1 deer, 2 behaviors
table(allspecs$deerID)

selectthisdeer <- "NW1WF12"

mydeer <- allspecs[allspecs$deerID == selectthisdeer,  ]
mydeerbehav <- mydeer[mydeer$behaviorcat == "RunOnly" | mydeer$behaviorcat == "VigilantOnly", ]

head(mydeerbehav)

spx<-ggplot(mydeerbehav, aes(x=freq, y=pow.x, group=DeerDate)) + 
  geom_line(aes(color=behaviorcat))+
  ggtitle(paste("x-axis", selectthisdeer))
spx

spy<-ggplot(mydeerbehav, aes(x=freq, y=pow.y, group=DeerDate)) + 
  geom_line(aes(color=behaviorcat))+
  ggtitle(paste("y-axis", selectthisdeer))
spy

spz<-ggplot(mydeerbehav, aes(x=freq, y=pow.z, group=DeerDate)) + 
  geom_line(aes(color=behaviorcat))+
  ggtitle(paste("z-axis", selectthisdeer))
spz

######### 4 deer, 1 behavior
table(allspecs$deerID)

mydeer <- allspecs[allspecs$deerID == "NW2MF14" |
                     allspecs$deerID == "NW1WF12" |
                     allspecs$deerID == "NW2MF9" |
                     allspecs$deerID == "WP2MF2", ]

selectthisbehavior <- "RunOnly"
mydeerbehav <- mydeer[mydeer$behaviorcat == selectthisbehavior, ]

head(mydeerbehav)

spx<-ggplot(mydeerbehav, aes(x=freq, y=pow.x, group=DeerDate)) + 
  geom_line(aes(color=deerID))+
  ggtitle(paste("x-axis", selectthisbehavior))
spx

spy<-ggplot(mydeerbehav, aes(x=freq, y=pow.y, group=DeerDate)) + 
  geom_line(aes(color=deerID))+
  ggtitle(paste("y-axis", selectthisbehavior))
spy

spz<-ggplot(mydeerbehav, aes(x=freq, y=pow.z, group=DeerDate)) + 
  geom_line(aes(color=deerID))+
  ggtitle(paste("z-axis", selectthisbehavior))
spz

