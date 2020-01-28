library(reshape2)
library(ggplot2)

setwd("C:/Users/Apryle D. Craig/Documents/PhD/Video_Analysis/Incubator/deerfear/data/stages_of_processing/results_of_step_06_countsToLabels")
avlabs <- readRDS("accVidLabs.dat")     

options(digits.secs = 3)
options(digits = 3)

head(avlabs)
tail(avlabs)
str(avlabs)

totperdeer <- aggregate(avlabs[ ,c("DurationVigilance", 
                                   "DurationForaging",
                                   "DurationBedded",
                                   "DurationTravel")], by=list(Category=avlabs$deerID), FUN=sum)
str(totperdeer)
totperdeer$tottime <- rowSums(totperdeer[ , -1])
proptimeperdeer <- cbind(deerid = totperdeer$Category, totperdeer[2:6]/totperdeer$tottime)
head(proptimeperdeer)

melted <- melt(proptimeperdeer)
head(melted)
ggplot(melted, aes(variable, value)) + geom_boxplot()

#split boxplots by wolf presence/absence
melted$wolfpresence <- as.factor(substr(melted$deerid, 1, 2))
melted
ggplot(melted, aes(variable, value)) + geom_boxplot()

p2 <- ggplot(melted, aes(x=wolfpresence,y=value,fill=wolfpresence))+
  geom_boxplot() + labs(title="CMP") +facet_wrap(~variable)
p2
