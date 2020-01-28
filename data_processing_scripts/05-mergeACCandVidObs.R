library(dplyr)

setwd("C:/Users/Apryle D. Craig/Documents/PhD/Video_Analysis/Incubator/deerfear/data/stages_of_processing/results_of_step_04_CleanVidObs")
list.files()
labeledvids <- readRDS("CleanVideoObs.dat")
head(labeledvids)
str(labeledvids)
#labeledvids$DeerDate <- as.factor(labeledvids$DeerDate)

setwd("C:/Users/Apryle D. Craig/Documents/PhD/Video_Analysis/Incubator/deerfear/data/stages_of_processing/results_of_step_03_AllWorkingACCs")
list.files()
acc <- readRDS("allDeerACC.dat")
head(acc)
str(acc)
#acc$DeerDate <- as.factor(acc$DeerDate)

# how many acc values are there for each deer-date?
table(labeledvids$DeerDate) # this should have 1 per DeerDate
table(acc$DeerDate) # this should have 99-103 per video
sum(100 > summary(acc$DeerDate)) # this should be 0

# below shows that all deer have at least 100 acc records
df <- data.frame(table(acc$DeerDate))
df.toofew <- df[df$Freq<100, ] # need to take these results and look up a few deer. 
df.toofew # should be 0 

#################################################
# merge the videos and the accs
vamerge <- acc %>% inner_join(labeledvids, by = "DeerDate")

head(vamerge)
nrow(labeledvids)
nrow(acc)
nrow(vamerge) 
str(vamerge)
summary(vamerge)
vamerge$DeerDate <- as.factor(vamerge$DeerDate)
str(vamerge)

table(vamerge$DeerDate)
sum(100 > summary(vamerge$DeerDate)) # this should be 0, indicates all deer have at least 100

vidacc <- vamerge

# drop unneeded columns, then reorder
colnames(vidacc)
vidacc <- vidacc[ , -which(names(vidacc) %in% c("viddate",
                                                "vidtime",
                                                "datetime",
                                                "DeerID"
                                                )
                           )]

head(vidacc)
# ncol(vidacc)
# col_order <- c("DeerID", 
#                "DeerDate", "Date", "Time", "ACCSecond","datetimemil", 
#                "accx", "accy", "accz",
#                "DurationForaging", "DurationBedded", "DurationGrooming", 
#                "DurationVigilance", 
#                "DurationTravel", "Descriptiontravel", "Speedtravel",
#                "Species", "Sex", "ForageGroup",
#                "LessThan10secAccountedFor?"
#                )
# 
# vidacc <- vidacc[, col_order]

# ensure structure is correct
str(vidacc)
vidacc$DeerDate <- as.factor(vidacc$DeerDate)
vidacc$Species <- as.factor(vidacc$Species)
vidacc$Sex <- as.factor(vidacc$Sex)
vidacc$ForageGroup <- as.factor(vidacc$ForageGroup)

vidacc$DurationForaging <- as.numeric(vidacc$DurationForaging)
vidacc$DurationBedded <- as.numeric(vidacc$DurationBedded)
vidacc$DurationGrooming <- as.numeric(vidacc$DurationGrooming)
vidacc$DurationVigilance <- as.numeric(vidacc$DurationVigilance)
vidacc$DurationTravel <- as.numeric(vidacc$DurationTravel)


######################################################

# all time in each video must be categorized, 
# so video activites must add up to 9, 10, or 11

head(vidacc)

vidacc$LessThan10secAccountedFor <- vidacc$DurationForaging + 
  vidacc$DurationBedded + 
  vidacc$DurationGrooming + 
  vidacc$DurationVigilance + 
  vidacc$DurationTravel

vidacc2 <- vidacc[!vidacc$LessThan10secAccountedFor<=9, ]
nrow(vidacc2)

str(vidacc2)

# save output 
setwd("C:/Users/Apryle D. Craig/Documents/PhD/Video_Analysis/Incubator/deerfear/data/stages_of_processing/results_of_step_05_mergeACCandObs")
list.files()

saveRDS(vidacc, file = "accObsmerged.dat")
