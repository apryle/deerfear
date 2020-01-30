# PCA with pure behavior epochs

library(dplyr)
library(matlab)
library(ggplot2)
library(data.table)
library(reshape2)
library(reshape)
library(stringr)

mywd <- "C:/Users/Apryle D. Craig/Documents/PhD/Video_Analysis/Incubator/deerfear"
setwd(paste0(mywd, "/data/stages_of_processing/results_of_step_06_countsToLabels"))
list.files()
avlabs <- readRDS("accVidLabs.dat")

options(digits.secs = 3)
options(digits = 3)

head(avlabs)
tail(avlabs)
str(avlabs)
summary(avlabs)

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

# remove all uncertain, possibly incorrect bheaviors
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

behav <- OneDeer$behaviorcat[1] # just need 1 behavior label

accmax <- max(c(OneDeer$accx, OneDeer$accy, OneDeer$accz))
accmin <- min(c(OneDeer$accx, OneDeer$accy, OneDeer$accz))
plot(OneDeer$accx~OneDeer$datetimemil, type="l", col="blue", 
     ylim = c(accmin, accmax),
     main = paste(mydeer, " doing behavior: ", behav, "\n from", stdt, "to", enddt))
lines(OneDeer$accy~OneDeer$datetimemil, type="l", col="red")
lines(OneDeer$accz~OneDeer$datetimemil, type="l", col="gray")

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
summary(set2) # this should be 101 for all Freq 
summary(set) # contrast with previous step, which shows 101-103

nrow(purevidACCequal2)

purevidACC <- purevidACCequal2

table(purevidACC$DeerDate)
purevidACC$DeerDate <- as.factor(purevidACC$DeerDate)
head(purevidACC)

nrow(purevidACC)

# this file has pure behavior epochs only
# and equal sample size for all video epochs
list.files()
saveRDS(purevidACC, file = paste0(mywd, "/data/stages_of_processing/forPCA/purevidACC.dat"))

################### reshape for PCA in 2 steps: (1) melt and (2) cast

########## (1) subset and melt, for PCA format


forPCA <- purevidACC[, c("DeerDate", 
                         "ACCSeconds", 
                         "accx", "accy", "accz",
                         "behaviorcat")]

head(forPCA)
str(forPCA)
nrow(forPCA)

melted <- melt(forPCA, id=c(("DeerDate"), "ACCSeconds", "behaviorcat"),
               measure.vars = c("accx", "accy", "accz"))

head(melted)

# compare original data with reshaped data
melted[variable=="accz", ]
purevidACC[purevidACC$DeerDate=="NW1MF11_20140120_073053", c("ACCSeconds", 
                                                             "accx", "accy", "accz",
                                                             )]
# done comparing original data with reshaped data
dim(melted)
head(melted)
melted$DeerDate <- as.factor(melted$DeerDate )
unique(melted$DeerDate)

########## (2) cast the melted df, for PCA format

# create a sequencing vector to hold the group of 303 acc records per deerdate
nrow(melted)/303
seq1 <- rep(1:303)
seq2 <- rep(seq1, times=nrow(melted)/303)
length(seq2)
melted$seq <- seq2

# create a field to hold the hacked deerdate plus associated behavior label
# we will pivot on this concatenated hacked field
melted$DeerDateBehav <- paste0(melted$DeerDate, "_", melted$behaviorcat)

# reshape the data
castedtry <- cast(melted, DeerDateBehav~seq)  
dim(castedtry)
castedtry[1:10, 10:20] # look at some rows - does it make sense?
summary(castedtry)

widePCA3 <- castedtry

###########################################

# recreate a behavior label column for each deer-date
widePCA3[1:5, 1:5] # look at what is currenty in nae col
spltnames <- str_split_fixed(widePCA3$DeerDateBehav, "_", 4)
colnames(spltnames) <- c("DeerID", "VidDate", "VidTime", "behaviorcat")
head(spltnames)

nrow(widePCA3)
nrow(spltnames)

widePCA3[1:5, 1:5] 
head(spltnames)

widePCA4 <- cbind(spltnames, widePCA3)
widePCA4[1:7, 1:8] 

############ PCA on all 3 axes #####################################

pca.obj <- prcomp(widePCA4[, 6:ncol(widePCA3)],
              center = TRUE, scale = TRUE)

str(pca.obj)

dtp <- data.frame('behaviorcat' = widePCA4$behaviorcat, pca.obj$x[,1:2]) # the first two componets are selected (NB: you can also select 3 for 3D plottings or 3+)
ggplot(data = dtp) + 
        geom_point(aes(x = PC1, y = PC2, col = behaviorcat)) + 
        theme_minimal() +
        ggtitle("PCA of Acceleration in x, y, and z")


str(pca.obj$rotation)
pcarot <- as.data.frame(pca.obj$rotation)
str(pcarot)
pcarot[ , 1]
plot(pcarot[, 1], type="ln")
plot(pcarot[1:101, 1], type="ln")
plot(pcarot[102:202, 1], type="ln")
plot(pcarot[203:303, 1], type="ln")

#plot portion of explained variance
summary(pca.obj)
eigs <- pca.obj$sdev^2
myeigs <- eigs / sum(eigs)
plot(myeigs, xlim=c(0, 10)) # look for inflection point where adding more PCs does not explain more variation

############ PCA on all x axis #####################################

widePCA4.x <- widePCA4[, 6:106] # for x values only

pca.x <- prcomp(widePCA4.x,  
                  center = TRUE, scale = TRUE)

str(pca.x)

dtp <- data.frame('behaviorcat' = widePCA4$behaviorcat, pca.x$x[,1:2]) # the first two componets are selected (NB: you can also select 3 for 3D plottings or 3+)
ggplot(data = dtp) + 
        geom_point(aes(x = PC1, y = PC2, col = behaviorcat)) + 
        theme_minimal() +
        ggtitle("PCA of Acceleration in x")


str(pca.x$rotation)
pcarot <- as.data.frame(pca.x$rotation)
str(pcarot)
pcarot[ , 1]
plot(pcarot[, 1], type="ln")
plot(pcarot[1:101, 1], type="ln")
plot(pcarot[102:202, 1], type="ln")
plot(pcarot[203:303, 1], type="ln")

#plot portion of explained variance
summary(pca.x)
eigs <- pca.x$sdev^2
myeigs <- eigs / sum(eigs)
plot(myeigs, xlim=c(0, 10)) # look for inflection point where adding more PCs does not explain more variation

############ PCA on all y axis #####################################

widePCA4.y <- widePCA4[, 107:207] # for y values only

pca.y <- prcomp(widePCA4.y,  
                center = TRUE, scale = TRUE)

str(pca.y)

dtp <- data.frame('behaviorcat' = widePCA4$behaviorcat, pca.y$x[,1:2]) # the first two componets are selected (NB: you can also select 3 for 3D plottings or 3+)
ggplot(data = dtp) + 
        geom_point(aes(x = PC1, y = PC2, col = behaviorcat)) + 
        theme_minimal() +
        ggtitle("PCA of Acceleration in y")


str(pca.y$rotation)
pcarot <- as.data.frame(pca.y$rotation)
str(pcarot)
pcarot[ , 1]
plot(pcarot[, 1], type="ln")
plot(pcarot[1:101, 1], type="ln")
plot(pcarot[102:202, 1], type="ln")
plot(pcarot[203:303, 1], type="ln")

#plot portion of explained variance
summary(pca.y)
eigs <- pca.y$sdev^2
myeigs <- eigs / sum(eigs)
plot(myeigs, xlim=c(0, 10)) # look for inflection point where adding more PCs does not explain more variation

############ PCA on all z axis #####################################

widePCA4.z <- widePCA4[ , 208:ncol(widePCA4)] # for z values only

pca.z <- prcomp(widePCA4.z,  
                center = TRUE, scale = TRUE)

str(pca.z)

dtp <- data.frame('behaviorcat' = widePCA4$behaviorcat, pca.z$x[,1:2]) # the first two componets are selected (NB: you can also select 3 for 3D plottings or 3+)
ggplot(data = dtp) + 
        geom_point(aes(x = PC1, y = PC2, col = behaviorcat)) + 
        theme_minimal() +
        ggtitle("PCA of Acceleration in z")


str(pca.z$rotation)
pcarot <- as.data.frame(pca.z$rotation)
str(pcarot)
pcarot[ , 1]
plot(pcarot[, 1], type="ln")
plot(pcarot[1:101, 1], type="ln")
plot(pcarot[102:202, 1], type="ln")
plot(pcarot[203:303, 1], type="ln")

#plot portion of explained variance
summary(pca.z)
eigs <- pca.z$sdev^2
myeigs <- eigs / sum(eigs)
plot(myeigs, xlim=c(0, 10)) # look for inflection point where adding more PCs does not explain more variation


