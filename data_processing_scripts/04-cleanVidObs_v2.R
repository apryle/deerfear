#import video labeled dataset
library(readxl)

setwd("C:/Users/Apryle D. Craig/Documents/PhD/Video_Analysis/Incubator/deerfear/data/observed_from_videos/")
list.files()
purevideos <- as.data.frame(read_excel("ObservedVideoBehaviors.xlsx", 
                                       sheet = "Follows"))
head(purevideos)
str(purevideos)
purevideos$DeerID <- as.factor(purevideos$DeerID)

# Remove all exact duplicates
purevideos2 <- purevideos[!duplicated(purevideos[,c("DeerID","Date", "Time")]),]
nrow(purevideos2)
nrow(purevideos) # this process rmvd about 1200 duplicates

# Remove all observations with SNOW or text in behavior column
# because we cannot use it as a behavior label if we cannot see
nrow(purevideos2)
colnames(purevideos2)
purevideos3 <- purevideos2[-which(purevideos2$DurationVigilance=="SNOW" |
                     purevideos2$DurationForaging=="SNOW" |
                     purevideos2$DurationGrooming=="SNOW" |
                     purevideos2$DurationTravel=="SNOW" |
                     purevideos2$DurationBedded=="SNOW"), ]
nrow(purevideos3) # removed about 500 rows

# Check to see if there are blanks in Time or Date field 
purevideos3[which(is.null(purevideos3$Time)), ]
purevideos3[which(is.null(purevideos3$Date)), ]

# Check for NAs
sum(is.na(purevideos2$DurationVigilance))
sum(is.na(purevideos2$DurationForaging))
sum(is.na(purevideos2$DurationBedded))
sum(is.na(purevideos2$DurationTravel))
sum(is.na(purevideos2$DurationGrooming))

# Check to see if there are dates or times that do not make sense
purevideos3[which(nchar(purevideos3$Time)>6), ]
head(purevideos3)

###### Date coersion
# add leading 0 to times
i<-1
while (i<length(purevideos3$Time)) {
  if (nchar(purevideos3$Time[i])<6) { 
    purevideos3$Time[i] <- paste0("0", purevideos3$Time[i])
  print(purevideos3$Time[i])
  } 
  i <- i +1
}

# Check to see if it worked
purevideos3[which(nchar(purevideos3$Time)<6), ] # should be zero
purevideos3[which(nchar(purevideos3$Time)==6), ] # should be full dataset
purevideos3[which(nchar(purevideos3$Time)>6), ] # should be zero

# convert dates and add a new column to hold
purevideos3$datetime <- paste(purevideos3$Date, purevideos3$Time, sep=" ")
purevideos3$datetime <- strptime(purevideos3$datetime, "%Y%m%d %H%M%OS")
purevideos3$datetime <- as.POSIXct(purevideos3$datetime)

head(purevideos3)
str(purevideos3)

#remove all unneeded columns
colnames(purevideos3)
purevideos3 <- purevideos3[ , -which(names(purevideos3) %in% c("Coded Deer ID",
                                                            "Viewer",
                                                            "Notes",
                                                            "Vigilant",
                                                            "NumberBites",
                                                            "NumberOtherDeer",  
                                                            "SpeciesofDeerPresent",
                                                            "InteractionType",
                                                            "Landscapetype" ,             
                                                            "Terraintype" ,              
                                                            "Coverwithin10mofDeer",
                                                            "CoverBEYOND10mofdeer",
                                                            "Snow",
                                                            "Northing", "Westing", 
                                                            "OnBoardGPSworked?", 
                                                            "Time of Day",
                                                            "DeerDay",
                                                            "DaysSinceRelease"
                                                            ))]


head(purevideos3)

#create a column with Deer_Date_Time in both dataframes 
purevideos3$DeerDate <- paste(purevideos3$DeerID, purevideos3$Date, purevideos3$Time, sep="_")
nrow(purevideos3)

setwd("C:/Users/Apryle D. Craig/Documents/PhD/Video_Analysis/Incubator/deerfear/data/stages_of_processing/results_of_step_04_CleanVidObs")
list.files()

saveRDS(purevideos3, file = "CleanVideoObs.dat")
