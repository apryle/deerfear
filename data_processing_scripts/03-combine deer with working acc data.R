setwd("C:/Users/Apryle D. Craig/Documents/PhD/Video_Analysis/Incubator/deerfear/data/stages_of_processing/results_of_step_02")
list.files()

drive1chunk1 <- readRDS("drive1chunk1ACC.dat")
drive1chunk2 <- readRDS("drive1chunk2ACC.dat")
drive1chunk3 <- readRDS("drive1chunk3ACC.dat")
Season23ACC <- readRDS("ACCWorked23.dat")

head(drive1chunk1)
nrow(drive1chunk1)
summary(drive1chunk1[,c("accx", "accy", "accz")])
sum(is.na(drive1chunk1$accx)) + sum(is.na(drive1chunk1$accy)) + sum(is.na(drive1chunk1$accz))
drive1chunk1[ drive1chunk1$accx==0 & drive1chunk1$accy==0 & drive1chunk1$accz==0, ]

head(drive1chunk2)
nrow(drive1chunk2)
summary(drive1chunk2[,c("accx", "accy", "accz")])
sum(is.na(drive1chunk2$accx)) + sum(is.na(drive1chunk2$accy)) + sum(is.na(drive1chunk2$accz))
drive1chunk2[ drive1chunk2$accx==0 & drive1chunk2$accy==0 & drive1chunk2$accz==0, ]

head(drive1chunk3)
nrow(drive1chunk3)
summary(drive1chunk3[,c("accx", "accy", "accz")])
sum(is.na(drive1chunk3$accx)) + sum(is.na(drive1chunk3$accy)) + sum(is.na(drive1chunk3$accz))
drive1chunk3[ drive1chunk3$accx==0 & drive1chunk3$accy==0 & drive1chunk3$accz==0, ]

head(Season23ACC)
nrow(Season23ACC)
summary(Season23ACC[,c("accx", "accy", "accz")])
sum(is.na(Season23ACC$accx)) + sum(is.na(Season23ACC$accy)) + sum(is.na(Season23ACC$accz))
Season23ACC[ Season23ACC$accx==0 & Season23ACC$accy==0 & Season23ACC$accz==0, ]

## merge them
Season123ACC <- rbind(drive1chunk1, drive1chunk2, drive1chunk3, Season23ACC) 
sum(is.na(Season123ACC$accx)) + sum(is.na(Season123ACC$accy)) + sum(is.na(Season123ACC$accz))
summary(Season123ACC[,c("accx", "accy", "accz")])

# save to 02b data folder
saveRDS(Season123ACC, file = "Season123ACC.dat")
#Season123ACC <- readRDS("Season123ACC.dat")

table(Season123ACC$deerID)

head(Season123ACC)
nrow(Season123ACC)
head(Season123ACC)
summary(Season123ACC)

allDeer <- Season123ACC

# convert dates and add a new column to hold
allDeer$datetime <- paste(allDeer$viddate, allDeer$vidtime, sep=" ")
allDeer$datetime <- strptime(allDeer$datetime, "%Y%m%d %H%M%OS")
allDeer$datetimemil <- allDeer$datetime + allDeer$ACCSecond

head(allDeer)

#see how many videos per deer-date-time. should be at least 100
str(allDeer$deerdate)
sum(100 > summary(allDeer$deerdate)) # this should be 0

#remove unneeded columns 
head(allDeer)
colnames(allDeer)
allDeer <- allDeer[ , -which(names(allDeer) %in% c("datetime"))]
head(allDeer)

# rename deerdate column to DeerDate to match VidObs spreadsheet
names(allDeer)[names(allDeer) == "deerdate"] <- "DeerDate"

setwd("C:/Users/Apryle D. Craig/Documents/PhD/Video_Analysis/Incubator/deerfear/data/stages_of_processing/results_of_step_03_AllWorkingACCs")
saveRDS(allDeer, file = "allDeerACC.dat")

