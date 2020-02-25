setwd("~/Documents/PhD/Video_Analysis/Incubator/deerfear/data/stages_of_processing/forPCA")
list.files()
subsetofpureEpochs <- readRDS("subsetofpureEpochsOnly.dat")

head(subsetofpureEpochs)
purevidACC <- subsetofpureEpochs
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
