library("dplyr")
library("data.table")
library("ggplot2")

# import pure behavior dataset
mywd <- "C:/Users/Apryle D. Craig/Documents/PhD/Video_Analysis/Incubator/deerfear"
setwd(paste0(mywd, "/data/stages_of_processing/forPCA"))
list.files()
purevidACC <- readRDS("purevidACC.dat")

purevidACC2 <- purevidACC

mypsd <- function(obs){
  #obs <- OneDeerDate
  
  specx <- spectrum(obs$accx, method="ar", plot=FALSE)  # , plot=FALSE
  specy <- spectrum(obs$accy, method="ar", plot=FALSE)
  specz <- spectrum(obs$accz, method="ar", plot=FALSE)
  
  f <- specx$freq*10
  
  myx <- specx$spec
  myy <- specy$spec
  myz <- specz$spec
  
  mypower <- as.data.frame(cbind(f, myx, myy, myz))
  
  #N <- length(myz)
  #mypower$DeerID <- rep(obs$deerID[1], N)
  #mypower$DeerDate <- rep(obs$DeerDate[1], N)
  #mypower$behaviorcat <- rep(obs$behaviorcat[1], N)
  
  colnames(mypower) <- c("freq", 
                         "pow.x", "pow.y", "pow.z" #, "DeerID", "myDeerDate", "behaviorcat"
                         )
  
  return(mypower)
}


# For one deer, one 10-second observation at a time

head(purevidACC)
levels(purevidACC$behaviorcat)
runningDeer <- purevidACC[purevidACC$behaviorcat == "RunOnly", ]

runningDeer # view it and choose one DeerDate

myDeerDateDate <- "NW1MF11_20140129_080336"

OneDeerDate <- purevidACC[purevidACC$DeerDate == myDeerDateDate, ]
nrow(OneDeerDate) # make sure this is 101
head(OneDeerDate)

mypower <- mypsd(OneDeerDate)
head(mypower)
nrow(mypower)
################################

# all deerdates in a given behavior

oneBehav <- purevidACC[purevidACC$behaviorcat == "RunOnly", ]
head(oneBehav)
nrow(oneBehav)
oneBehav$DeerDate <- factor(oneBehav$DeerDate) # drop unused levels
table(oneBehav$DeerDate) # view which deerdates to expect in output
str(oneBehav$DeerDate)

oneBehav[500:512, 1:7] # this shows deer NW1WF12_20140209_101101
oneBehav[11500:11512, 1:7] # this shows deer NW1WF2_20130105_102701
oneBehav[22500:22512, 1:7] # this shows deer NW2MF8_20140127_125415



####### try a generic, known function head()
headtib <- oneBehav %>%
              group_by(DeerDate) %>% 
              group_modify(~ head(.x, 1L))

nrow(headtib)
headtib[1:10, 1:5]

#### now with my funtion mypsd, one behavior

psdtib <- oneBehav %>%
  group_by(DeerDate, deerID, behaviorcat) %>% 
  group_modify(~ mypsd(.x))

head(psdtib)

# view it does it look correct?
psdtib[1:10, ]
psdtib[20000:20010, ]

nrow(psdtib)

head(psdtib)
table(psdtib$deerID)
levels(psdtib$deerID)

#### now with my funtion, all behaviors

psdtib2 <- purevidACC %>%
  group_by(DeerDate, deerID, behaviorcat) %>% 
  group_modify(~ mypsd(.x))

head(psdtib2)

# view it does it look correct?
psdtib[1:10, ]
psdtib[20000:20010, ]

nrow(psdtib)

head(psdtib)
table(psdtib$deerID)
levels(psdtib$deerID)


############## plot a few deer 

onedeerpsd <- psdtib[psdtib$deerID == "NW2MF9" | 
                       psdtib$deerID == "NW1WM1" |
                       psdtib$deerID == "NW2MF4" |
                      psdtib$deerID == "WP2MF2" 
                     , ]
onedeerpsd


WP2MF2 <- psdtib[psdtib$deerID == "WP2MF2" , ]

spx<-ggplot(onedeerpsd, aes(x=freq, y=pow.x, group=DeerDate)) + 
  geom_line(aes(color=deerID))+
  ggtitle("x-axis")

spy<-ggplot(onedeerpsd, aes(x=freq, y=pow.y, group=DeerDate)) + 
  geom_line(aes(color=deerID))+
  ggtitle("y-axis") 
################### same deer, 2 behaviors

onedeerpsd <- psdtib[psdtib$deerID == "NW2MF9" , ]
onedeerpsd2behav <- onedeerpsd[onedeerpsd$behaviorcat == "RunOnly" | onedeerpsd$behaviorcat == "VigilantOnly", ]

spx<-ggplot(onedeerpsd2behav, aes(x=freq, y=pow.x, group=DeerDate)) + 
  geom_line(aes(color=behaviorcat))+
  ggtitle("x-axis NW2MF9, 2 behaviors")
