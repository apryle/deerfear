# transform data for PCA 

library(ggplot2)

widePCA4 # this is creaed in file 09 convertLongtoWide.R
table(widePCA4$behaviorcat) 

# start with running and vigilant only
twoBehavs <- widePCA4[widePCA4$behaviorcat=="VigilantOnly" | widePCA4$behaviorcat=="RunOnly", ]
widePCA4 <-twoBehavs
widePCA4$behaviorcat <- factor(widePCA4$behaviorcat)
widePCA4$DeerID <- factor(widePCA4$DeerID)

# select 1 training deer and 1 testing deer
table(widePCA4$DeerID)
table(widePCA4$behaviorcat) # this should have only run and vigilant

traindeer <- "NW1WF12" # 126 observations 
testdeer <- "WP2MM6" # 93 observations

# subset to do pca on just training deer
training <-widePCA4[widePCA4$DeerID==traindeer, ]
dim(training)
training$behaviorcat

# subset for testing deer
testing <-widePCA4[widePCA4$DeerID==testdeer, ]
dim(testing)
testing$behaviorcat
table(testing$behaviorcat)

# transform training data for PCA 
min(training[, 6:ncol(training)]) # check to see if there are zeros
trainingplus1 <- training[, 6:ncol(training)] + .01 # add 1 because cannot take log of 0
min(trainingplus1)

logTrainDataX <- log(trainingplus1) # the predictors only. Power spectrum
TrainDataY <- testing$behaviorcat