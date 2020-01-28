#clean acc only

require(data.table)

mywd <- "C:/Users/Apryle D. Craig/Documents/PhD/Video_Analysis/Incubator/deerfear/data/extracted_from_harddrives/Results_from_step_01b/"
setwd(mywd)

list.files()


###### from harddrive 1, chunk 1 ###############################

deerdata1_100000 <- readRDS("masterall1_100000.dat")
head(deerdata1_100000) # deer shows as NW1MF11
str(deerdata1_100000)
deerdata <- deerdata1_100000

####### from harddrive 1, chunk 2 #############################

deerdata100001_200000 <- readRDS("masterall100001_200000.dat")
head(deerdata100001_200000) # deer shows as NW1WF6
str(deerdata100001_200000)
deerdata <- deerdata100001_200000

######## from hardddrive 1, chunk 3 #############################

deerdata200001_end <- readRDS("masterall200001_500547.dat")
head(deerdata200001_end)
str(deerdata200001_end)
deerdata <- deerdata200001_end

######### from harddrive 2, chunk 1 ############################

deerdata23 <- readRDS("masterallseason23.dat")
head(deerdata23)
str(deerdata23)

#####################################


#separate acc data
accMessy1 <- lapply(deerdata, function (x) {
  x$acc })

# put into dataframe
accMessy <- rbindlist(accMessy1, fill=TRUE)

head(accMessy)
accMessy$deerdate <- paste0(accMessy$deerID, "_", accMessy$viddate, "_", accMessy$vidtime)
table(accMessy$deerdate)
df <- data.frame(table(accMessy$deerdate))
df.toofew <- df[df$Freq<100, ]
df.toofew # these acc dat files were empty when I checked the hard drive

accMessy[accMessy$deerdate == "NW2WM6_20140302_100640", ]

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

accMessy4 <- accMessy3
# everything is intact til this step

################################################################
# NAs
# in one case (NW2MF2 20130131  154013) in drive 1 chunk 3, 
# it recorded jibberish which is causing an NA 
# looks like it is a single row - just remove that row

withNA <- accMessy3[is.na(accMessy3$accx) & is.na(accMessy3$accy) & is.na(accMessy3$accz), ]
withoutNA <- accMessy3[!(is.na(accMessy3$accx) & is.na(accMessy3$accy) & is.na(accMessy3$accz)), ]
nrow(withNA)
nrow(accMessy3)
nrow(withoutNA)

# make sure all of the other rows are still there for NW2MF2_20130131_154013
withoutNA[withoutNA$deerdate == "NW2MF2_20130131_154013", ]

accMessy3 <- withoutNA

#########################
################################################################

#in some cases, the acc malfunctioned and recorded all zeros
#only use the files where acc does not equal 0 0 0

accMessy3$deerdate <- as.factor(accMessy3$deerdate)

str(accMessy3)

# how many files have all zeros where it didn't work? 0 0 0
sumbydeerdate <- aggregate(accMessy3[ , c("accx", "accy", "accz")], by=list(accMessy3$deerdate), FUN=sum)
accDidntWork <- sumbydeerdate[which(sumbydeerdate$accx == 0 & sumbydeerdate$accy == 0 & sumbydeerdate$accx == 0), ]

head(accDidntWork)
nrow(accDidntWork)
length(accDidntWork$Group.1)

#this should remove all 
accWorked <- accMessy3[ ! accMessy3$deerdate %in% accDidntWork$Group.1, ]
op <- accMessy3$deerdate %in% accDidntWork$Group.1 # opposite
op
sum(op)

summary(accDidntWork)
str(accDidntWork)

accdidntworkfull <- accMessy3[ accMessy3$deerdate %in% accDidntWork$Group.1, ] # find the cases where it didn't work

nrow(accMessy3) == nrow(accdidntworkfull) + nrow(accWorked) # this should be true. 

accWorked

#########################
# check accx + accy + accz == 0... this could also be a short term ACC error
acconly <- accWorked[ , c("deerID", "accx", "accy", "accz") ]
head(acconly)
accdidnwork2 <- acconly[acconly$accx ==0 & acconly$accy == 0 & acconly$accz == 0 , ]

###########################
#### Are ACCs still in tact at this step? 
### It was incorrect, but now seems to be working. 
accFinalOneDeer <- accWorked[which(accWorked$deerdate =="WP2MF10_20150203_111924"), ]

nrow(accFinalOneDeer) # yes, looks like it worked til here

#######################################
accWorked$deerdate <- factor(accWorked$deerdate) # drop unused factors of deerdate

# check that each remaining deerdate has at least 100 acc records
df <- data.frame(table(accWorked$deerdate)) # create a df of frequencies
df.toofew <- df[df$Freq<100, ] # find small counts
df.toofew # this should be empty

setwd("~/Documents/PhD/Video_Analysis/Incubator/deerfear/data/stages_of_processing/results_of_step_02")
list.files()
#######################################
# 3940986 rows in chunk 1
saveRDS(accWorked, file = "drive1chunk1ACC.dat")

#######################################

saveRDS(accWorked, file = "drive1chunk2ACC.dat")

#######################################

saveRDS(accWorked, file = "drive1chunk3ACC.dat")

#######################################

saveRDS(accWorked, file = "ACCWorked23.dat")
