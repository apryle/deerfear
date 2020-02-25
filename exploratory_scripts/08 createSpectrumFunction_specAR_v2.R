# create power spectrum for all pure behaviors

library("dplyr")
library("data.table")
library("ggplot2")

# import pure behavior dataset
mywd <- "C:/Users/Apryle D. Craig/Documents/PhD/Video_Analysis/Incubator/deerfear"
setwd(paste0(mywd, "/data/stages_of_processing/forPCA"))
list.files()
purevidACC <- readRDS("purevidACC.dat")

# create a backup
purevidACC2 <- purevidACC

# function to create power 
mypsd <- function(obs){
  desiredsmoothing <- 200
  
  specx <- spec.ar(obs$accx, method="ols", plot=FALSE, n.freq = desiredsmoothing)
  specy <- spec.ar(obs$accy, method="ols", plot=FALSE, n.freq = desiredsmoothing)
  specz <- spec.ar(obs$accz, method="ols", plot=FALSE, n.freq = desiredsmoothing)
  
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
                         "pow.x", "pow.y", "pow.z" 
                         #, "DeerID", "myDeerDate", "behaviorcat"
                          )
  
  return(mypower)
}

# Test it for one deer, one 10-second observation

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

#######################################

# all deerdates

psdtib <- purevidACC %>%
  group_by(DeerDate, deerID, behaviorcat) %>% 
  group_modify(~ mypsd(.x))

# see if it worked
head(psdtib)
tail(psdtib)
nrow(psdtib) # this should be desiredsmoothing (as defined in mypsd function) * number of levels of deerdate in psdtib
length(levels(psdtib$DeerDate))*200 == nrow(psdtib) # this should be true
table(psdtib$deerID)
table(psdtib$behaviorcat)

# view it does it look correct?
psdtib[1:10, ]
psdtib[120000:120010, ]

purebehaviorspectrums <- psdtib

# save it 
saveRDS(purebehaviorspectrums, file = "purebehaviorspectrums.dat")

