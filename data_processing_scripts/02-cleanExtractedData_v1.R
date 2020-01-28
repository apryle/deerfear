# now set it to pull from data folder

mywd <- "C:/Users/Apryle D. Craig/Documents/PhD/Video_Analysis/Incubator/deerfear/data/extracted_from_harddrives/Results_from_step_01b/"
setwd(mywd)

list.files()

#load a segmented list file containing gps and acc data
#FOR SEASON 1-2 harddrive: 
deerdata <- readRDS("masterall1_100000.dat")      # group a
deerdata <- readRDS("masterall100001_200000.dat") # group b 
deerdata <- readRDS("masterall200001_500547.dat") # group c

length(deerdata)

#FOR SEASON 2-3 harddrive
setwd("E:/")
deerdata <- readRDS("masterall1_200000.dat")
deerdata <- readRDS("masterall200001_400000.dat")

length(deerdata)
head(deerdata)
tail(deerdata)

#separate gps and acc data
gpsMessy1 <- lapply(deerdata, function (x) {
  x$gps })

accMessy1 <- lapply(deerdata, function (x) {
  x$acc })

# put into dataframe
require(data.table)
str(gpsMessy1) # before
gpsMessy1[1]
gpsMessy1[71825]

gpsMessy <- rbindlist(gpsMessy1, fill=TRUE)
accMessy <- rbindlist(accMessy1, fill=TRUE)

head(accMessy)

##############################
######### records are intact up to this point

OneDeer2ndhalf <- accMessy[which(accMessy$deerID =="NW1MF4"
                             & accMessy$viddate =="20150118"
                             & accMessy$vidtime =="120336"
                             ), ]

#######################################

head(gpsMessy)
tail(gpsMessy)
nrow(gpsMessy) # when running alldats[1:5000], I got 50898 rows. WHen running alldats[1:10000], I got 101807 rows. This seems to be working.

str(gpsMessy) # after
class(gpsMessy)
gpsMessy$deerID <- as.factor(gpsMessy$deerID) #change deerID to a factor
str(gpsMessy)
head(gpsMessy)
gpsMessy[5:15]
# clean up acc df

# reorder columns
setcolorder(accMessy, c("deerID", "viddate", "vidtime", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V1"))

# drop un needed columns
accFinal <- accMessy[,c("V6", "V7", "V8", "V1"):=NULL]

###########################
#### records are in tact at this step
head(accFinal)
OneDeer <- accFinal[which(accFinal$deerID =="NW1MF4"
                                 & accFinal$viddate =="20150118"
                                 & accFinal$vidtime =="120336"
), ]
nrow(OneDeer)
#######################################


names(accFinal) <- c("DeerID", "VideoDate", "VideoTime", "ACCSecond", "accx", "accy", "accz")
str(accFinal)
accFinal$DeerID <- as.factor(accFinal$DeerID) #change deerID to a factor
str(accFinal)
head(accFinal)

# how many acc records per deer?
summary(accFinal$DeerID)

###########################
#### Is it still in tact at this step?  YES!!!
accFinalOneDeer <- accFinal[which(accFinal$DeerID =="NW1MF4"
                                 & accFinal$VideoDate =="20150118"
                                 & accFinal$VideoTime =="120336"
), ]

nrow(accFinalOneDeer)

#######################################

# Now the GPS 
gpsMessy$V3<-as.character(gpsMessy$V3)

# separate col 3 text into separate columns
str(gpsMessy$V3)
max(nchar(gpsMessy$V3))
min(nchar(gpsMessy$V3))

names(gpsMessy) <- c("Junk1", "GPSseconds", "LatLongStr", "DeerID", "vidDate", "VidTime")


library(stringr)
colsplit <- str_split_fixed(gpsMessy$LatLongStr, ",",8)
gpsinfo <- cbind(gpsMessy, colsplit)
head(gpsinfo)
gpsFinal <- gpsinfo[,c("V1", "V2", "V3", "V8"):=NULL]
gpsFinal <- gpsinfo[,c("Junk1", "LatLongStr"):=NULL]

head(gpsFinal)

names(gpsFinal) <- c("GPSsecond", "DeerID", "VideoDate", "VideoTime", "Latitude", "LatUnits", "Longitude", "LongUnits")

setcolorder(gpsFinal, c("DeerID", "VideoDate", "VideoTime", "GPSsecond", "Latitude", "LatUnits", "Longitude", "LongUnits"))

head(gpsFinal)


################################################################

#in some cases, the acc malfunctioned and recorded all zeros
#only use the files where acc does not equal 0 0 0

# how many files have all zeros where it didn't work? 0 0 0
accDidntWork <- accFinal[which(accFinal$accx ==0
                               & accFinal$accy ==0
                               & accFinal$accz ==0
                               ), ]

nrow(accDidntWork)
accDidntWork[200000:200010] # inspect a few random rows
accDidntWork[900:920] # inspect a few random rows

accWorked <- subset(accFinal, accFinal$accx != 0 | accFinal$accy != 0 | accFinal$accz != 0)
nrow(accFinal) == nrow(accDidntWork) + nrow(accWorked) # this should be true. 
#it is for group a. It is FALSE for b. it is FALSE for c

nrow(accFinal) - nrow(accDidntWork) - nrow(accWorked) # for group b, I am 97 off

###########################
#### Is it still in tact at this step? 
### It was incorrect, but now seems to be working. 
accFinalOneDeer <- accWorked[which(accWorked$DeerID =="NW1MF4"
                                  & accWorked$VideoDate =="20150118"
                                  & accWorked$VideoTime =="120336"
), ]

nrow(accFinalOneDeer)

#######################################

# compare this to the full table accFinal: how many acc records per deer?
summary(accFinal$DeerID)
summary(accWorked$DeerID)
summary(accDidntWork$DeerID) # if a deerID had 0 in the previous line, it should have a number in this line and vice versa 

#######################################

#drop levels
levels(accWorked$DeerID)
accWorked$DeerID <- droplevels(accWorked$DeerID)
levels(accWorked$DeerID) # should match the summary produced above

#compare accWorked and accFinal
#accWorked should be smaller than accFinal
nrow(accFinal)
nrow(accWorked)

accFinal[1:40]
accWorked[1:40] 

tail(accFinal) # the acc values are all zeros! This should not appear in accWorked for 1-100000
tail(accWorked) 

levels(accWorked$DeerID) 



#save the full files with working acc 

setwd("C:/Users/Apryle D. Craig/Documents/PhD/Video_Analysis/Incubator/deerfear/data/stages_of_processing/results_of_step_02")
list.files()

# SEASON 1-2
season12a <- accWorked
season12b <- accWorked
season12c <- accWorked
season12all <- rbind(season12a, season12b, season12c)
saveRDS(season12all, file = "season12accWorked.dat")

# SEASON 2-3 
Season23deerwithACCdata1_200000 <- readRDS("Season23deerwithACCdata1_200000.dat")
saveRDS(accWorked, file = "Season23deerwithACCdata200000_end.dat")
Season23all <- rbind(Season23deerwithACCdata1_200000, accWorked)
nrow(Season23all)
saveRDS(accWorked, file = "Season23all.dat")
