# prep fot pipeline pure behavior epochs

library(dplyr)
library(matlab)
library(ggplot2)
library(data.table)
library(reshape2)
library(reshape)
library(stringr)

mywd <- "C:/Users/Apryle D. Craig/Documents/PhD/Video_Analysis/Incubator/deerfear"
setwd(paste0(mywd, "/data/stages_of_processing/forPCA"))
list.files()
avlabs <- readRDS("purebehaviorspectrums.dat")

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

table(avlabs$behaviorcat)
avlabs[avlabs$behaviorcat=="BeddedHeadUpNOTRum", ] <-"BeddedHeadNOTRum"
avlabs$behaviorcat <- factor(avlabs$behaviorcat)

myspec <- avlabs
str(myspec)
summary(myspec)
######### prep########################################

################### reshape for PCA in 2 steps: (1) melt and (2) cast

########## (1) subset and melt, for PCA format

head(myspec)
prep <- myspec[, c("DeerDate", 
                     "freq", 
                     "pow.x", "pow.y", "pow.z",
                     "behaviorcat", "deerID")]

head(prep)
str(prep)
nrow(prep)

forPCA <- as.data.frame(prep)
melted <- melt(forPCA, id=c("DeerDate", "freq", "behaviorcat", "deerID"),
               measure.vars = c("pow.x", "pow.y", "pow.z"))

head(melted)

# compare original data with reshaped data
melted[melted$variable=="pow.x", ]
myspec[myspec$DeerDate=="NW1MF11_20140120_073053", c("freq", 
                                                     "pow.x", "pow.y", "pow.z")]
# done comparing original data with reshaped data
dim(melted)
head(melted)
melted$DeerDate <- as.factor(melted$DeerDate )
unique(melted$DeerDate)

########## (2) cast the melted df, for PCA format

# create a sequencing vector to hold the group of power records per deerdate
table(melted$DeerDate)
powerfreq <- 600 # this can change based on frequency set in mypsd function 
nrow(melted)/powerfreq
seq1 <- rep(1:powerfreq)
seq2 <- rep(seq1, times=nrow(melted)/powerfreq)
length(seq2)
melted$seq <- seq2

# create a field to hold a hacked deerdate plus associated behavior label
# we will pivot on this concatenated hacked field
melted$DeerDateBehav <- paste0(melted$DeerDate, "_", melted$behaviorcat)

head(melted)
melted$freq

# reshape the data
castedtry <- cast(melted, DeerDateBehav~seq)  
dim(castedtry) # this should be the number of deerdates
head(castedtry)
castedtry[1:10, 10:20] # look at some rows - does it make sense?

widePCA3 <- castedtry

###########################################

# recreate a behavior label column for each deer-date
head(widePCA3)
widePCA3[1:5, 1:5] # look at what is currenty in cols
spltnames <- str_split_fixed(widePCA3$DeerDateBehav, "_", 4)
colnames(spltnames) <- c("DeerID", "VidDate", "VidTime", "behaviorcat")
head(spltnames)

nrow(widePCA3)
nrow(spltnames)

widePCA3[1:5, 1:5] 
head(spltnames)

widePCA4 <- cbind(spltnames, widePCA3)
widePCA4[1:7, 1:8] 

table(widePCA4$behaviorcat)
widePCA4[widePCA4$behaviorcat=="", ] 
