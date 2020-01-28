# 06 convert duration counts to behavior labels/categories

setwd("C:/Users/Apryle D. Craig/Documents/PhD/Video_Analysis/Incubator/deerfear/data/stages_of_processing/results_of_step_05_mergeACCandObs")
list.files()

accvid <- readRDS("accObsmerged.dat")

head(accvid)
tail(accvid)
str(accvid)

accvid$behaviorcat <- "TBD"
head(accvid)

head(accvid)
tail(accvid)
str(accvid)

# Assign behavior labels including
  # ForageOnly
  # BeddedGeneral
  # BeddedHeadUpKnown
  # BeddedRuminateKnown
  # Grooming
  # RunOnly
  # WalkOnly

# ForageOnly
accvid[accvid$DurationForaging >= 1 &
         accvid$DurationBedded == 0 &
         accvid$DurationGrooming == 0 &
         accvid$DurationTravel == 0 &
         accvid$DurationVigilance == 0, c("behaviorcat")] <- "ForageOnly"

head(accvid)
tail(accvid)

# BeddedGeneral
accvid[which(accvid$DurationForaging == 0 &
               accvid$DurationBedded >= 1 &      # this could be bedded headup, or bedded head down
               accvid$DurationGrooming == 0 &
               accvid$DurationTravel == 0 &
               accvid$DurationVigilance == 0), c("behaviorcat")] <- "BeddedGeneral"

# BeddedHeadUpKnown
accvid[which(accvid$DurationForaging == 0 &
               accvid$DurationBedded == 10 &
               accvid$DurationGrooming == 0 &
               accvid$DurationTravel == 0 &
               accvid$DurationVigilance == 10), c("behaviorcat")] <- "BeddedHeadNOTRum" # head up could be vig, sleeping, swallow
# BeddedHeadUpKnown
accvid[which(accvid$DurationForaging == 0 &
               accvid$DurationBedded == 9 &
               accvid$DurationGrooming == 0 &
               accvid$DurationTravel == 0 &
               accvid$DurationVigilance == 9), c("behaviorcat")] <- "BeddedHeadUpNOTRum"  # head up could be vig, sleeping, swallow


# BeddedHeadUpKnown... CHECK THESE - vig or foraging?
accvid[which(accvid$DurationForaging == 10 & 
               accvid$DurationBedded == 10 &
               accvid$DurationVigilance == 10 ), c("behaviorcat")] <- "checkThese"

# BeddedRuminateKnown
accvid[which(accvid$DurationForaging == 10 &
               accvid$ForageGroup == "CUD" &
               accvid$DurationBedded == 10 &
               accvid$DurationGrooming == 0 &
               accvid$DurationTravel == 0 &
               accvid$DurationVigilance == 0), c("behaviorcat")] <- "BeddedRuminateKnown"  # head up chewing
# BeddedRuminateKnown
accvid[which(accvid$DurationForaging == 9 &
               accvid$ForageGroup == "CUD" &
               accvid$DurationBedded == 9 &
               accvid$DurationGrooming == 0 &
               accvid$DurationTravel == 0 &
               accvid$DurationVigilance == 0), c("behaviorcat")] <- "BeddedRuminateKnown"  # head up chewing

# Grooming
accvid[which(accvid$DurationForaging == 0 &
               accvid$DurationBedded == 0 &
               accvid$DurationGrooming >= 1 &
               accvid$DurationTravel == 0 &
               accvid$DurationVigilance == 0), c("behaviorcat")] <- "Grooming"

# WalkOnly
accvid[which(accvid$DurationForaging == 0 &
               accvid$DurationBedded == 0 &
               accvid$DurationGrooming == 0 &
               accvid$DurationTravel >= 1 &
               accvid$Descriptiontravel == 1 &
               accvid$DurationVigilance == 0), c("behaviorcat")] <- "WalkOnly"

# RunOnly
accvid[which(accvid$DurationForaging == 0 &
               accvid$DurationBedded == 0 &
               accvid$DurationGrooming == 0 &
               accvid$DurationTravel >= 1 &
               accvid$Descriptiontravel == 2 &
               accvid$DurationVigilance == 0), c("behaviorcat")] <- "RunOnly"

# VigilantOnly
accvid[which(accvid$DurationForaging == 0 &
               accvid$DurationBedded == 0 &
               accvid$DurationGrooming == 0 &
               accvid$DurationTravel == 0 &
               accvid$DurationVigilance >= 1), c("behaviorcat")] <- "VigilantOnly"

# ForageTravel
accvid[which(accvid$DurationForaging == 10 &
               accvid$DurationBedded == 0 &
               accvid$DurationGrooming == 0 &
               accvid$DurationTravel == 10 &
               accvid$DurationVigilance == 0), c("behaviorcat")] <- "ForageTravel"
# ForageTravel
accvid[which(accvid$DurationForaging == 9 &
               accvid$DurationBedded == 0 &
               accvid$DurationGrooming == 0 &
               accvid$DurationTravel == 9 &
               accvid$DurationVigilance == 0), c("behaviorcat")] <- "ForageTravel"

# label all remaining as mixed
accvid[which(accvid$behaviorcat =="TBD"), c("behaviorcat")] <- "MixedBehavior"

head(accvid)
tail(accvid)

######## Are there videos that are bedded, but forage group is not CUD? That doesn't really occur
# check these videos
BeddedForageNotCUD <- accvid[which(accvid$DurationBedded >= 1 & 
                                     accvid$DurationForaging >= 1 &
                                     accvid$ForageGroup != "CUD"), ] 
nrow(BeddedForageNotCUD)
head(BeddedForageNotCUD)

# label them as CheckThisVideo for now 
accvid[which(accvid$DurationBedded >= 1 & 
               accvid$DurationForaging >= 1 &
               accvid$ForageGroup != "CUD"), c("behaviorcat")] <- "CheckThisVideo"

######## Are there videos that are 100% vigilant and 100% traveling? 100% vigilant and 100% foraging? 
# That doesn't really occur in nature
accvid[which(accvid$DurationTravel == 10 & accvid$DurationVigilance == 10 ), ] 
accvid[which(accvid$DurationTravel == 9 & accvid$DurationVigilance == 9 ), ] 

accvid[which(accvid$DurationBedded < 10 & accvid$DurationForaging == 10 & accvid$DurationVigilance == 10 ), ] 
accvid[which(accvid$DurationBedded < 9 & accvid$DurationForaging == 9 & accvid$DurationVigilance == 9 ), ] 

accvid[which(accvid$DurationBedded ==10 & accvid$DurationTravel == 10 ), ] 
accvid[which(accvid$DurationBedded ==9 & accvid$DurationTravel == 9 ), ] 


########## some checks prior to saving
head(accvid)
tail(accvid)
str(accvid)

sum(table(accvid$DeerDate) < 105) # All videos should have fewer than 105 acc records
sum(table(accvid$DeerDate) > 99) # All videos should have more than 99 acc records

accvid$behaviorcat <- as.factor(accvid$behaviorcat)
accvid$Descriptiontravel <- as.factor(accvid$Descriptiontravel)
accvid$Speedtravel <- as.factor(accvid$Speedtravel)

table(accvid$behaviorcat)
table(accvid$deerID)

########## this file contains only deer where the full 9 or 10 seconds was accounted for
############ also adds labels for each behavior
############ also removed behaviors that are mutually exclusive
setwd("C:/Users/Apryle D. Craig/Documents/PhD/Video_Analysis/Incubator/deerfear/data/stages_of_processing/results_of_step_06_countsToLabels")
list.files()
saveRDS(accvid, file = "accVidLabs.dat")


