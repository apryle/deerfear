
mywd <- "C:/Users/Apryle D. Craig/Documents/PhD/Video_Analysis/Incubator/deerfear"
setwd(paste0(mywd, "/data/stages_of_processing/forPCA"))
list.files()
purevidACC <- readRDS("purevidACC.dat")

#############################################

#create some features for each 10-second time epoch
head(purevidACC)
featuresaccx <- do.call(data.frame, aggregate(purevidACC$accx~DeerDate , purevidACC, 
                                              function(x) c(mean = mean(x), 
                                                            max = max(x), 
                                                            min = min(x),
                                                            SD = sd(x),
                                                            mxmindiff = max(x)-min(x))))

head(featuresaccx) # check to make sure it looks correct

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


featuresxyz$totvar <- var(purevidACC$accx) + var(purevidACC$accy) + var(purevidACC$accz)
head(featuresxyz)

#### features that use all three axes, add in pitch, inclination, etc
                                                     
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
