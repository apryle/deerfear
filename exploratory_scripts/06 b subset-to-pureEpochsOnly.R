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
subsetofpureEpochsOnly <- pure[order(pure$deerID, pure$datetimemil), ]

# this file has pure behavior epochs only
# and equal sample size for all video epochs
list.files()
saveRDS(subsetofpureEpochsOnly, file = paste0(mywd, "/data/stages_of_processing/forPCA/subsetofpureEpochsOnly.dat"))

