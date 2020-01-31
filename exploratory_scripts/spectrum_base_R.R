# import pure behavior dataset
mywd <- "C:/Users/Apryle D. Craig/Documents/PhD/Video_Analysis/Incubator/deerfear"
setwd(paste0(mywd, "/data/stages_of_processing/forPCA"))
list.files()
purevidACC <- readRDS("purevidACC.dat")

purevidACC2 <- purevidACC


# For one deer at a time

################ just grab 1 deer, for one 10-sec video epoch
################ doing BeddedHeadUpNOTRum

head(purevidACC)
levels(purevidACC$behaviorcat)
table(purevidACC$behaviorcat, purevidACC$deerID)

beddedDeer <- purevidACC[purevidACC$behaviorcat == "BeddedHeadUpNOTRum"
                         & purevidACC$deerID == "NW1MF13", ]

head(beddedDeer)
str(beddedDeer)
beddedDeer$DeerDate <- factor(beddedDeer$DeerDate)
table(beddedDeer$DeerDate)

myDeerDate <- "NW1MF13_20150106_120001"  # choose a deerdate from the list

OneDeerDate <- purevidACC[purevidACC$DeerDate == myDeerDate, ]
nrow(OneDeerDate) # make sure this is 101


#### plot acc x over time
accmax <- max(c(OneDeerDate$accx, OneDeerDate$accy, OneDeerDate$accz))
accmin <- min(c(OneDeerDate$accx, OneDeerDate$accy, OneDeerDate$accz))
plot(OneDeerDate$accx~OneDeerDate$datetimemil, type="l", col="blue", 
     ylim = c(accmin, accmax),
     main = paste(myDeerDate, " doing behavior: ", behav))
points(OneDeerDate$accy~OneDeerDate$datetimemil, type="l", col="red") 
points(OneDeerDate$accz~OneDeerDate$datetimemil, type="l", col="green") 


myspec <- spectrum(OneDeerDate$accx, method="ar")
spectrum(OneDeerDate$accy, method="ar")
spectrum(OneDeerDate$accz, method="ar")

# tease apart the components of spectrum
# use plot=FALSE to turn off plotting and just run function
myspec$freq*10
myspec$spec
summary(myspec)

############### now try a running deer

head(purevidACC2)
levels(purevidACC$behaviorcat)
runningDeer <- purevidACC[purevidACC$behaviorcat == "RunOnly", ]

runningDeer # view it and choose one DeerDate

myDeerDateDate <- "NW1MF11_20140129_080336"

OneDeerDate <- purevidACC[purevidACC$DeerDate == myDeerDateDate, ]
nrow(OneDeerDate) # make sure this is 101

head(OneDeerDate)

#### plot acc x over time
accmax <- max(c(OneDeerDate$accx, OneDeerDate$accy, OneDeerDate$accz))
accmin <- min(c(OneDeerDate$accx, OneDeerDate$accy, OneDeerDate$accz))
plot(OneDeerDate$accx~OneDeerDate$datetimemil, type="l", col="blue", 
     ylim = c(accmin, accmax),
     main = paste(myDeerDate, " doing behavior: ", behav))
points(OneDeerDate$accy~OneDeerDate$datetimemil, type="l", col="red") 
points(OneDeerDate$accz~OneDeerDate$datetimemil, type="l", col="green") 


spectrum(OneDeerDate$accx, method="ar")
spectrum(OneDeerDate$accy, method="ar")
spectrum(OneDeerDate$accz, method="ar")
