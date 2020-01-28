#FOR SEASON 2-3 harddrive, clean acc only

setwd("~/Documents/PhD/Video_Analysis/Incubator/deerfear/data/extracted_from_harddrives/Results_from_step_01b")
deerdata23 <- readRDS("masterallseason23.dat")
head(deerdata23)
str(deerdata23)

deerdata <- deerdata23

#separate acc data
accMessy1 <- lapply(deerdata, function (x) {
  x$acc })

# put into dataframe
require(data.table)

accMessy <- rbindlist(accMessy1, fill=TRUE)

head(accMessy)
accMessy$deerdate <- paste0(accMessy$deerID, "_", accMessy$viddate, "_", accMessy$vidtime)
table(accMessy$deerdate)
df <- data.frame(table(accMessy$deerdate))
df.toofew <- df[df$Freq<100, ]
df.toofew 

#this should remove all that have too few
main_data2 <- accMessy[ ! accMessy$deerdate %in% df.toofew$Var1, ]

# check that it worked
table(main_data2$deerdate)
df <- data.frame(table(main_data2$deerdate)) # create a df of frequencies
df.toofew <- df[df$Freq<100, ] # find small counts
df.toofew # this should be empty

head(main_data2)

# but these, which have missing ACC values in future step are fine at this step
main_data2[main_data2$deerdate=="NW1MF13_20141230_161854", ] # this was were missing ACC values in a future step
main_data2[main_data2$deerdate=="NW1MF13_20141231_170851", ] # this was were missing ACC values in a future step
main_data2[main_data2$deerdate=="NW1MF13_20141230_161854", ] # this was were missing ACC values in a future step
main_data2[main_data2$deerdate=="WP2MF10_20150203_104823", ] # this was were missing ACC values in a future step
# they look fine at this step

accMessy2 <- main_data2
nrow(accMessy2)
head(accMessy2)

#############################

# reorder columns
setcolorder(accMessy2, c("deerID", "viddate", "vidtime", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V1"))

# drop un needed columns
accMessy3 <- accMessy2[,-c("V6", "V7", "V8", "V1")]
colnames(accMessy3) <- c("deerID", "viddate", "vidtime", "ACCSeconds", "accx", "accy", "accz", "deerdate")
head(accMessy3)

accMessy3
# everything is intact til this step
################################################################

#in some cases, the acc malfunctioned and recorded all zeros
#only use the files where acc does not equal 0 0 0

# how many files have all zeros where it didn't work? 0 0 0
accDidntWork <- accMessy3[which(accMessy3$accx ==0
                               & accMessy3$accy ==0
                               & accMessy3$accz ==0
), ]

head(accDidntWork)
nrow(accDidntWork)
accDidntWork[200000:200010] # inspect a few random rows
accDidntWork[900:920] # inspect a few random rows

#this should remove all that have all zeros
accWorked <- accMessy3[ ! accMessy3$deerdate %in% accDidntWork$deerdate, ]

nrow(accMessy3) == nrow(accDidntWork) + nrow(accWorked) # this should be true. 

accWorked

###########################
#### Are ACCs still in tact at this step? 
### It was incorrect, but now seems to be working. 
accFinalOneDeer <- accWorked[which(accWorked$deerdate =="WP2MF10_20150204_080103"), ]

nrow(accFinalOneDeer) # yes, looks like it worked til here

#######################################

# compare this to the full table accFinal: how many acc records per deer?
table(accMessy3$deerID)
table(accWorked$deerID)
table(accDidntWork$deerID) # if a deerID had 0 in the previous line, it should have a number in this line and vice versa 

#######################################