# PCA with pure behavior epochs

library(dplyr)
library(matlab)
library(ggplot2)
library(data.table)
library(reshape2)
library(reshape)

setwd("C:/Users/Apryle D. Craig/Documents/PhD/Video_Analysis/Incubator/deerfear/data/stages_of_processing/results_of_step_06_countsToLabels")
list.files()
avlabs <- readRDS("accVidLabs.dat")

options(digits.secs = 3)
options(digits = 3)

head(avlabs)
tail(avlabs)
str(avlabs)


#how many acc records per deerdate?
table(avlabs$DeerDate)
set1 <- data.frame(table(avlabs$DeerDate))
summary(set1)
#############################################
# reduce to only look at pure epochs for now

# remove all mixed bheaviors
pure <- avlabs[avlabs$behaviorcat != "MixedBehavior", ]

head(pure)
tail(pure)
str(pure)

# remove all mixed bheaviors
pure <- pure[pure$behaviorcat != "CheckThisVideo" , ]
head(pure)
nrow(pure)
str(pure)

# drop the unused levels to see how many deer we are 
pure$deerID<-factor(pure$deerID)
pure$DeerDate<-factor(pure$DeerDate)
pure$behaviorcat<-factor(pure$behaviorcat)

head(pure)
tail(pure)
str(pure)
nrow(pure)

table(pure$DeerDate)
summary(pure$DeerDate)

summary(pure$behaviorcat)

# sort by DeerID and date time
pure <- pure[order(pure$deerID, pure$datetimemil), ]

################ plot one deer

purevidACC <- pure

mydeer <- "NW1MF11"
stdt <- "2014-01-20 07:30:00"
enddt <- "2014-01-20 07:31:00"

OneDeer <- purevidACC[which(purevidACC$deerID == mydeer &  
                                    purevidACC$datetimemil > stdt &
                                    purevidACC$datetimemil < enddt), ]

accmax <- max(c(OneDeer$accx, OneDeer$accy, OneDeer$accz))
accmin <- min(c(OneDeer$accx, OneDeer$accy, OneDeer$accz))
plot(OneDeer$accx~OneDeer$datetimemil, type="l", col="blue", ylim = c(accmin, accmax))
lines(OneDeer$accy~OneDeer$datetimemil, type="l", col="red")
lines(OneDeer$accz~OneDeer$datetimemil, type="l", col="gray")
title(main = paste("One Deer", OneDeer$deerID, "here"))

######### pca prep########################################

head(purevidACC)

#how many acc records per deerdate?
table(purevidACC$DeerDate)
set <- data.frame(table(purevidACC$DeerDate))
summary(set)

#make all video epochs equal to 101 acc records
purevidACCequal <- by(purevidACC, purevidACC$DeerDate, head, n=101)
str(purevidACCequal)
purevidACCequal2 <- rbindlist(purevidACCequal, fill=TRUE)
head(purevidACCequal2)

#how many acc records per deerdate?
table(purevidACCequal2$DeerDate)
set2 <- data.frame(table(purevidACCequal2$DeerDate))
summary(set2) 
summary(set) # contrast with previous step

nrow(purevidACCequal2)

purevidACC <- purevidACCequal2

######### PCA on raw data #######################
table(purevidACC$DeerDate)
purevidACC$DeerDate <- as.factor(purevidACC$DeerDate)
head(purevidACC)

nrow(purevidACC)

forPCA <- purevidACC[, c("DeerDate", 
                         "ACCSeconds", 
                         "accx", "accy", "accz",
                         "behaviorcat")]

head(forPCA)
str(forPCA)
nrow(forPCA)

# reshape for PCA
melted <- melt(forPCA, id=c(("DeerDate"), "ACCSeconds", "behaviorcat"),
               measure.vars = c("accx", "accy", "accz"))
melted[variable=="accz", ]
purevidACC[purevidACC$DeerDate=="NW1MF11_20140120_073053", c("ACCSeconds", 
                                                             "accx", "accy", "accz")]

dim(melted)
head(melted)
melted$DeerDate <- as.factor(melted$DeerDate )
unique(melted$DeerDate)

widePCA3 <- melted
dim(widePCA3)
head(widePCA3)


#####different way to reshape
head(forPCA)
str(forPCA)
nrow(forPCA)
meltIt <- melt(forPCA, id.vars=c("DeerDate", "ACCSeconds"))
head(meltIt)
dcast(meltIt, DeerDate~variable)
dcast(meltIt, DeerDate~ACCSeconds)

casted <- cast(melted, DeerDate~value)
summary(casted)
dim(casted)
head(casted)

casted2 <- cast(melted, DeerDate~ACCSeconds)
head(casted2)
nrow(casted2)

nrow(melted)/303
seq1 <- rep(1:303)
seq2 <- rep(seq1, times=nrow(melted)/303)
length(seq2)

melted$seq <- seq2

dcast(DeerDate + seq ~ value, data = melted)

castedtry <- cast(melted, DeerDate~seq)  ########## this one works without labels
dim(castedtry)
summary(castedtry)
#################################################

pca <- prcomp(widePCA3[, 1:303],
              center = TRUE, scale = TRUE)

str(pca)

autoplot(pca) # basic plot
str(pca$rotation)
pcarot <- as.data.frame(pca$rotation)
str(pcarot)
pcarot[ , 1]
plot(pcarot[, 1], type="ln")
plot(pcarot[1:101, 1], type="ln")
plot(pcarot[102:202, 1], type="ln")
plot(pcarot[203:303, 1], type="ln")

autoplot(pca, data = widePCA3, colour = "behavior",
         loadings.label = TRUE, loadings.label.size = 3)

#plot portion of explained variance
summary(pca)
eigs <- pca$sdev^2
myeigs <- eigs / sum(eigs)
plot(myeigs, xlim=c(0, 10)) # look for inflection point where adding more PCs does not explain more variation


autoplot(pca, data = widePCA3, colour = "behavior",
         x=2, y=3,
         loadings.label = TRUE, loadings.label.size = 9)

plot(pcarot[, 3], type="ln")
plot(pcarot[1:101, 3], type="ln")
plot(pcarot[102:202, 3], type="ln")
plot(pcarot[203:303, 3], type="ln")

lines(pcarot[, 1], type="ln", col="red")
lines(pcarot[, 2], type="ln", col="green")

autoplot(pca, data = widePCA3, colour = "behavior",
         x=1, y=3,
         loadings.label = TRUE, loadings.label.size = 3)


#############################################

#create some features for each 10-second time epoch
head(purevidACC)
featuresaccx <- do.call(data.frame, aggregate(purevidACC$accx~DeerDate , purevidACC, 
                                              function(x) c(mean = mean(x), 
                                                            max = max(x), 
                                                            min = min(x),
                                                            SD = sd(x),
                                                            mxmindiff = max(x)-min(x))))

featuresaccy <- do.call(data.frame, aggregate(purevidACC$accy~DeerDate , purevidACC, 
                                              function(x) c(mean = mean(x), 
                                                            max = max(x), 
                                                            min = min(x),
                                                            SD = sd(x),
                                                            mxmindiff = max(x)-min(x))))

featuresaccz <- do.call(data.frame, aggregate(purevidACC$accz~DeerDate , purevidACC, 
                                              function(x) c(mean = mean(x), 
                                                            max = max(x), 
                                                            min = min(x),
                                                            SD = sd(x),
                                                            mxmindiff = max(x)-min(x))))

#features that use all three axes, add in pitch, inclination, etc
featuresaccxyz <- do.call(data.frame, aggregate(purevidACC$accz~DeerDate , purevidACC, 
                                              function(x) c(mean = mean(x), 
                                                            max = max(x), 
                                                            min = min(x),
                                                            SD = sd(x),
                                                            mxmindiff = max(x)-min(x))))
str(featuresaccz)
str(featuresaccy)
str(featuresaccz)


#combine all features
allfeatures <- cbind(featuresaccx, featuresaccy, featuresaccz) 
#and remove the repeated dateTime columns
str(allfeatures)
allfeatures <- allfeatures[,-7]
str(allfeatures)
allfeatures <- allfeatures[,-12]
str(allfeatures)

vidACCfeat <- merge(purevideos, allfeatures)
str(vidACCfeat)

#behavior, sex, and species are factors/category, not a character
vidACCfeat$`Pure Behavior in Video` <- as.factor(vidACCfeat$`Pure Behavior in Video`)
vidACCfeat$Sex <- as.factor(vidACCfeat$Sex)
vidACCfeat$`Deer Species` <- as.factor(vidACCfeat$`Deer Species`)

str(vidACCfeat)
nrow(vidACCfeat)

# remove unused deer ID factors
summary(vidACCfeat$'Real ID')
vidACCfeat$'Real ID' <- factor(vidACCfeat$'Real ID')
summary(vidACCfeat$'Real ID')


str(vidACCfeat)
grep("purevidACC.accx.mean", colnames(vidACCfeat))

levels(vidACCfeat$`Real ID`)

nrow(vidACCfeat)
vidACCfeat <- unique(vidACCfeat)

#remove deer with fewer than 2 observations during a dateTIme
vidACCfeat <- vidACCfeat[!(vidACCfeat$dateTime=="NW2MF10_20140212_120134"), ]
vidACCfeat <- vidACCfeat[!(vidACCfeat$dateTime=="NW2MF10_20140212_120154"), ]
vidACCfeat <- vidACCfeat[!(vidACCfeat$dateTime=="NW2MF10_20140212_120204"), ]
vidACCfeat <- vidACCfeat[!(vidACCfeat$dateTime=="NW2MF10_20140212_120255"), ]


#RDA package
library(vegan)
#plotting
library(ggplot2)

############################ RDA for all deer
head(vidACCfeat)
pca <- prcomp(vidACCfeat[, 9:ncol(vidACCfeat)],
              center = TRUE, scale = TRUE)

#plot portion of explained variance
summary(pca)
eigs <- pca$sdev^2
myeigs <- eigs / sum(eigs)
plot(myeigs) # look for inflection point where adding more PCs does not explain more variation

###################################Plot RDA

library(ggfortify)

autoplot(pca)
str(vidACCfeat)
autoplot(pca, data = vidACCfeat, colour = "PureBehavior",
         loadings.label = TRUE, loadings.label.size = 3)

vidACCfeat$PureBehavior <- vidACCfeat$'Pure Behavior in Video'

autoplot(pca, data = vidACCfeat, colour = "PureBehavior",
         x=2, y=3,
         loadings.label = TRUE, loadings.label.size = 3)

autoplot(pca, data = vidACCfeat, colour = "PureBehavior",
         x=1, y=3,
         loadings.label = TRUE, loadings.label.size = 3)


###################### do an RDA for 1 deer, NW1MF11
OneDeer <- vidACCfeat[ which(vidACCfeat$'Real ID'=="NW1MF11"), ]
OneDeer$'Real ID' <- factor(OneDeer$'Real ID')
str(OneDeer)
head(OneDeer)

str(OneDeer)
my.rda <- rda(OneDeer[, 22:ncol(OneDeer)]) 
biplot(my.rda,
       main = "RDA pure epochs NW1MF11")
ordihull(my.rda,
         group = OneDeer$`Pure Behavior in Video`,
         col = c(1,2,3, 4, 5, 6, 7, 8, 9))

KnownBehavior <- levels(OneDeer$`Pure Behavior in Video`)

legend("bottomleft",
       col = c(1,2,3, 4, 5, 6, 7, 8, 9), 
       lty = 1,
       legend = KnownBehavior)


###################### do an RDA for 1 deer, NW1MF13
OneDeer <- vidACCfeat[ which(vidACCfeat$'Real ID'=="NW1MF13"), ]
OneDeer$'Real ID' <- factor(OneDeer$'Real ID')
str(OneDeer)

my.rda <- rda(OneDeer[, 22:ncol(OneDeer)]) 
biplot(my.rda,
       main = "RDA pure epochs NW1MF13")
ordihull(my.rda,
         group = OneDeer$`Pure Behavior in Video`,
         col = c(1,2,3, 4, 5, 6, 7, 8, 9))

KnownBehavior <- levels(OneDeer$`Pure Behavior in Video`)

legend("bottomleft",
       col = c(1,2,3, 4, 5, 6, 7, 8, 9), 
       lty = 1,
       legend = KnownBehavior)


###################### do an RDA for 1 deer, NW1MF4
OneDeer <- vidACCfeat[ which(vidACCfeat$'Real ID'=="NW1MF4"), ]
OneDeer$'Real ID' <- factor(OneDeer$'Real ID')
str(OneDeer)

my.rda <- rda(OneDeer[, 22:ncol(OneDeer)]) 
biplot(my.rda,
       main = "RDA pure epochs NW1MF4")
ordihull(my.rda,
         group = OneDeer$`Pure Behavior in Video`,
         col = c(1,2,3, 4, 5, 6, 7, 8, 9))

KnownBehavior <- levels(OneDeer$`Pure Behavior in Video`)

legend("topright",
       col = c(1,2,3, 4, 5, 6, 7, 8, 9), 
       lty = 1,
       legend = KnownBehavior)


##### Let's simplify BEHAVIORS to Bedded Head Down and Walk_Pure. All Deer
str(vidACCfeat)
TwoBehavs <- vidACCfeat[ which(vidACCfeat$`Pure Behavior in Video`=="Bedded_HeadDown"
                             | vidACCfeat$`Pure Behavior in Video`=="Walk_Pure" ), ]

TwoBehavs$`Pure Behavior in Video` <- factor(TwoBehavs$`Pure Behavior in Video`)
str(TwoBehavs) #should have 2 levels now

TwoBehavs$'Real ID' <- factor(TwoBehavs$'Real ID')
head(TwoBehavs)

str(TwoBehavs)
head(TwoBehavs[, 9:ncol(TwoBehavs)])
my.rda <- rda(TwoBehavs[, 22:ncol(TwoBehavs)]) 
biplot(my.rda,
       main = "RDA 2 distinct behaviors Bedded_HeadDown and Walk_Pure")
ordihull(my.rda,
         group = TwoBehavs$`Pure Behavior in Video`,
         col = c(1,2))

KnownBehavior <- levels(TwoBehavs$`Pure Behavior in Video`)

legend("topright",
       col = c(1,2), 
       lty = 1,
       legend = KnownBehavior)


############## NW1WF12 with just 2 behaviors
table(vidACCfeat$'Real ID', vidACCfeat$`Pure Behavior in Video`)
        
OneDeer <- vidACCfeat[ which(vidACCfeat$'Real ID'=="NW1WF12"), ]
nrow(OneDeer)

OneDeerTwoBehavs <- OneDeer[ which(OneDeer$`Pure Behavior in Video`=="Bedded_HeadDown"
                               | OneDeer$`Pure Behavior in Video`=="Walk_Pure" ), ]

OneDeerTwoBehavs$'Real ID' <- factor(OneDeerTwoBehavs$'Real ID')
OneDeerTwoBehavs$'Pure Behavior in Video' <- factor(OneDeerTwoBehavs$'Pure Behavior in Video')

head(OneDeerTwoBehavs)

str(OneDeerTwoBehavs)
head(OneDeerTwoBehavs[, 22:ncol(TwoBehavs)])
my.rda <- rda(OneDeerTwoBehavs[, 22:ncol(TwoBehavs)]) 
biplot(my.rda,
       main = "RDA 1 deer two distinct behavs bedded head down and walking")
ordihull(my.rda,
         group = OneDeerTwoBehavs$`Pure Behavior in Video`,
         col = c(1,2))

KnownBehavior <- levels(OneDeerTwoBehavs$`Pure Behavior in Video`)

legend("bottomleft",
       col = c(1,2), 
       lty = 1,
       legend = KnownBehavior)



############## All deer with just 2 behaviors
table(vidACCfeat$'Real ID', vidACCfeat$`Pure Behavior in Video`)

OneDeer <- vidACCfeat[ which(vidACCfeat$'Real ID'=="NW2MF14"), ]
nrow(OneDeer)

OneDeerTwoBehavs <- OneDeer[ which(OneDeer$`Pure Behavior in Video`=="Bedded_HeadDown"
                                   | OneDeer$`Pure Behavior in Video`=="Walk_Pure" ), ]

OneDeerTwoBehavs$'Real ID' <- factor(OneDeerTwoBehavs$'Real ID')
OneDeerTwoBehavs$'Pure Behavior in Video' <- factor(OneDeerTwoBehavs$'Pure Behavior in Video')

head(OneDeerTwoBehavs)

str(OneDeerTwoBehavs)
head(OneDeerTwoBehavs[, 22:ncol(TwoBehavs)])
my.rda <- rda(OneDeerTwoBehavs[, 22:ncol(TwoBehavs)]) 
biplot(my.rda,
       main = "RDA NW2MF14 two distinct behavs bedded head down and walking")
ordihull(my.rda,
         group = OneDeerTwoBehavs$`Pure Behavior in Video`,
         col = c(1,2))

KnownBehavior <- levels(OneDeerTwoBehavs$`Pure Behavior in Video`)

legend("topright",
       col = c(1,2), 
       lty = 1,
       legend = KnownBehavior)



############## One deer NW1MF11 with just 2 behaviors, compare it to first plot
table(vidACCfeat$'Real ID', vidACCfeat$`Pure Behavior in Video`)

OneDeer <- vidACCfeat[ which(vidACCfeat$'Real ID'=="NW1MF11"), ]
nrow(OneDeer)

table(OneDeer$`Pure Behavior in Video`)
levels(OneDeer$`Pure Behavior in Video`)

OneDeerTwoBehavs <- OneDeer[ which(OneDeer$`Pure Behavior in Video`=="NoWalkForage"
                                   | OneDeer$`Pure Behavior in Video`=="Bedded-Vigilant" ), ]

OneDeerTwoBehavs$'Real ID' <- factor(OneDeerTwoBehavs$'Real ID')
OneDeerTwoBehavs$'Pure Behavior in Video' <- factor(OneDeerTwoBehavs$'Pure Behavior in Video')

head(OneDeerTwoBehavs)

str(OneDeerTwoBehavs)
head(OneDeerTwoBehavs[, 22:ncol(TwoBehavs)])
my.rda <- rda(OneDeerTwoBehavs[, 22:ncol(TwoBehavs)]) 
biplot(my.rda,
       main = "RDA NW2MF14 two distinct behavs bedded head down and walking")
ordihull(my.rda,
         group = OneDeerTwoBehavs$`Pure Behavior in Video`,
         col = c(1,2))

KnownBehavior <- levels(OneDeerTwoBehavs$`Pure Behavior in Video`)

legend("bottomleft",
       col = c(1,2), 
       lty = 1,
       legend = KnownBehavior)

obsPureBehav <- as.data.frame(table(vidACCfeat$`Pure Behavior in Video`))
plot(obsPureBehav)
