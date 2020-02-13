library(psd)


# import pure behavior dataset
mywd <- "C:/Users/Apryle D. Craig/Documents/PhD/Video_Analysis/Incubator/deerfear"
setwd(paste0(mywd, "/data/stages_of_processing/forPCA"))
list.files()
purevidACC <- readRDS("purevidACC.dat")

purevidACC2 <- purevidACC

################ just grab 1 deer, for one 10-sec video epoch
################ doing BeddedHeadUpNOTRum

head(purevidACC2)
levels(purevidACC$behaviorcat)
beddedDeer <- purevidACC[purevidACC$behaviorcat == "BeddedHeadUpNOTRum", ]

head(beddedDeer)

myDeerDate <- "NW1MF13_20150106_120001"  # choose a deerdate from the list

OneDeer <- purevidACC[purevidACC$DeerDate == myDeerDate, ]
nrow(OneDeer) # make sure this is 101

behav <- OneDeer$behaviorcat[1] # just need 1 behavior label

#### plot acc x over time
accmax <- max(c(OneDeer$accx, OneDeer$accy, OneDeer$accz))
accmin <- min(c(OneDeer$accx, OneDeer$accy, OneDeer$accz))
plot(OneDeer$accx~OneDeer$datetimemil, type="l", col="blue", 
     ylim = c(accmin, accmax),
     main = paste(myDeerDate, " doing behavior: ", behav))

#########################

# create a time variable
N <-length(OneDeer$accx) # N is a number. Equivalent to the number samples from one axis (101)
n<- 0:(N-1) # n is a vector representing time. Equivalet to my datetimemil

# dataset with a "hidden" wave form
x <- OneDeer$accx  # x is equivalent to my acc value on one axis

# this should look similar to the one in the previous step, but "zoomed in"
plot(x~n, type="l", 
     main = paste(myDeerDate, " doing behavior: ", behav))

xPer <- (1/N)*abs(fft(x)^2)
f    <- seq(0,1.0-1/N,by=1/N)

plot(xPer[2:length(xPer)]~f[2:length(xPer)], 
     #ylim = c(-5, 15),
     type="l", main = paste(myDeerDate, " doing behavior: ", behav))

##########################################################
################ try another deer, for one 10-sec video epoch, 
################ doing a different behavior... RUNNING
head(purevidACC2)
levels(purevidACC$behaviorcat)
runningDeer <- purevidACC[purevidACC$behaviorcat == "RunOnly", ]


head(runningDeer) # view it and choose one DeerDate

myDeerDate <- "NW1MF11_20140129_080336"

OneDeer <- purevidACC[purevidACC$DeerDate == myDeerDate, ]
nrow(OneDeer) # make sure this is 101

behav <- OneDeer$behaviorcat[1] # just need 1 behavior label

#### plot acc x over time
accmax <- max(c(OneDeer$accx, OneDeer$accy, OneDeer$accz))
accmin <- min(c(OneDeer$accx, OneDeer$accy, OneDeer$accz))
plot(OneDeer$accx~OneDeer$datetimemil, type="l", col="blue", 
     ylim = c(accmin, accmax),
     main = paste(myDeerDate, " doing behavior: ", behav))

########### Now calcuate power spectral density ##############

# create a time variable
N <-length(OneDeer$accx) # N is a number. Equivalent to the number samples from one axis (101)
n<- 0:(N-1) # n is a vector representing time. Equivalet to my datetimemil

# dataset with a "hidden" wave form
x <- OneDeer$accx  # x is equivalent to my acc value on one axis

plot(x~n, type="l",
     main = paste(myDeerDate, " doing behavior: ", behav)) # this should look like the one we made above, but simpler

myfft <- fft(x)
length(myfft)

xPer <- (1/N)*abs(fft(x)^2)
f    <- seq(0,1.0-1/N,by=1/N)

plot(xPer[2:length(xPer)]~f[2:length(xPer)], # start it at 2 to remove the garbage at point 1, which was throwing off the y-axis scale
     type="l",
     #ylim=c(0, 8),
     main = paste(myDeerDate, " doing behavior: ", behav))


##########################################################
################ try another deer, for one 10-sec video epoch, 
################ doing RUNNING again with goal of comparing multiple running deer
head(purevidACC2)

runningDeer[103:108,] # choose the next available, which is from the same deer a little later
# NW1MF11_20140129_080356

#continue to keep the first deerdate to compare
myDeerDate <- "NW1MF11_20140129_080336"
myDeerDate2 <- "NW1MF11_20140129_080356"
  
OneDeer <- purevidACC[purevidACC$DeerDate == myDeerDate, ]
nrow(OneDeer) # make sure this is 101

behav <- OneDeer$behaviorcat[1] # just need 1 behavior label

#### plot acc x over time
accmax <- max(c(OneDeer$accx, OneDeer$accy, OneDeer$accz))
accmin <- min(c(OneDeer$accx, OneDeer$accy, OneDeer$accz))
plot(OneDeer$accx~OneDeer$datetimemil, type="l", col="blue", 
     ylim = c(accmin, accmax),
     main = paste(myDeerDate, " doing behavior: ", behav))

########### Now calcuate power spectral density ##############

# create a time variable
N <-length(OneDeer$accx) # N is a number. Equivalent to the number samples from one axis (101)
n<- 0:(N-1) # n is a vector representing time. Equivalet to my datetimemil

# dataset with a "hidden" wave form
x <- OneDeer$accx  # x is equivalent to my acc value on one axis

plot(x~n, type="l",
     main = paste(myDeerDate, " doing behavior: ", behav)) # this should look like the one we made above, but simpler

myfft <- fft(x)
length(myfft)

xPer <- (1/N)*abs(fft(x)^2)
f    <- seq(0,1.0-1/N,by=1/N)

plot(xPer~f, type="l",
     main = paste(myDeerDate, " doing behavior: ", behav))



################# calculate power for each deerdate
########## sort by behavior


