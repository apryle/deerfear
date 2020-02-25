library(dplyr)

setwd("~/Documents/PhD/Video_Analysis/Incubator/deerfear/data/stages_of_processing/forPCA")
list.files()
logtransformed <- readRDS("logtransformed.dat")

head(logtransformed)

widePCA4 <- logtransformed

table(widePCA4$DeerID, widePCA4$collapsedBehaviors)

nlevels(widePCA4$DeerID) # should be 29

numberOfDraws <- 10
randomDeer <- sample(unique(widePCA4$DeerID), numberOfDraws, replace=F)
randomDeer <- factor(unlist(randomDeer))
nlevels(randomDeer) # should match numberOfDraws above

# now stratify by sex and species
# we'd like to hold these deer for testing at the end
testdeerlist <- c("WP2MM6",
              "NW2WM6",
              "NW1WF11",
              "NW1MF9")

# create training and test datasets based on testdeer list
testdeer <- widePCA4[widePCA4$DeerID %in% testdeerlist ,]
testdeer$DeerID <- factor(testdeer$DeerID)
head(testdeer)
summary(testdeer[ , 1:6])
table(testdeer$DeerID)
table(testdeer$collapsedBehaviors)

trainingdeer <- widePCA4[!(widePCA4$DeerID %in% testdeerlist) ,]
table(trainingdeer$collapsedBehaviors)

nrow(widePCA4)
nrow(testdeer) + nrow(trainingdeer) == nrow(widePCA4) # this should be true

# save it 
setwd("~/Documents/PhD/Video_Analysis/Incubator/deerfear/data/stages_of_processing/forPCA")
saveRDS(trainingdeer, file = "trainingdeer.dat")
saveRDS(testdeer, file = "testdeer.dat")
