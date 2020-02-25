# Apryle Brain Algo

library(tidyverse)

mywd <- "C:/Users/Apryle D. Craig/Documents/PhD/Video_Analysis/Incubator/deerfear"
setwd(paste0(mywd, "/data/stages_of_processing/results_of_step_06_countsToLabels"))
list.files()
avlabs <- readRDS("accVidLabs.dat")

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

#create a new column so I can summarize over each second of video
avlabs$SecondsInteger <- "ToBeCalculated"

avlabs[avlabs$ACCSeconds < 1, c("SecondsInteger")] <- 0
avlabs[(avlabs$ACCSeconds >= 1 & avlabs$ACCSeconds < 2)  , c("SecondsInteger")] <- 1
avlabs[(avlabs$ACCSeconds >= 2 & avlabs$ACCSeconds < 3)  , c("SecondsInteger")] <- 2
avlabs[(avlabs$ACCSeconds >= 3 & avlabs$ACCSeconds < 4)  , c("SecondsInteger")] <- 3
avlabs[(avlabs$ACCSeconds >= 4 & avlabs$ACCSeconds < 5)  , c("SecondsInteger")] <- 4
avlabs[(avlabs$ACCSeconds >= 5 & avlabs$ACCSeconds < 6)  , c("SecondsInteger")] <- 5
avlabs[(avlabs$ACCSeconds >= 6 & avlabs$ACCSeconds < 7)  , c("SecondsInteger")] <- 6
avlabs[(avlabs$ACCSeconds >= 7 & avlabs$ACCSeconds < 8)  , c("SecondsInteger")] <- 7
avlabs[(avlabs$ACCSeconds >= 8 & avlabs$ACCSeconds < 9)  , c("SecondsInteger")] <- 8
avlabs[(avlabs$ACCSeconds >= 9)  , c("SecondsInteger")] <- 9

avlabs$SecondsInteger

#add a column for behavior category for that second (response/predicted behavior)
avlabs$predictedBehavior <- "TBD"

head(avlabs)

avlabs$DeerDateSec <- paste0(avlabs$DeerDate, "_", avlabs$SecondsInteger)

features <- avlabs %>%
  group_by(DeerDateSec, behaviorcat)  %>%
  summarize(mean.x = mean(accx),
            mean.y = mean(accy),
            mean.z = mean(accz),
            var.x = var(accx),
            var.y = var(accy),
            var.z = var(accz))

features$predictedbehavior <- "TBD"

#red == x axis
#gray == 
#blue ==

levels(avlabs$behaviorcat)
BeddedGeneral <- avlabs[(avlabs$behaviorcat == "BeddedGeneral")  , ]
ForageOnly <- avlabs[(avlabs$behaviorcat == "ForageOnly")  , ]
ForageOnly[11600:11620, 1:5]
BeddedRuminateKnown
BeddedGeneral[1600:1620, 1:5]

VigilantOnly <- features[(features$behaviorcat == "VigilantOnly")  , ]
RunOnly <- features[(features$behaviorcat == "RunOnly")  , ]
ForageOnly <- features[(features$behaviorcat == "ForageOnly")  , ]
VigilantOnly[1600:1620, ]
RunOnly[1600:1620, ]
ForageOnly[1600:1620, ]

features[(features$mean.x > features$mean.y 
        & features$mean.x > features$mean.z )  , c("predictedbehavior")] <- "Bedded"

features[(features$mean.x < features$mean.y 
          & features$mean.x < features$mean.z )  , c("predictedbehavior")] <- "Foraging"

features[(features$mean.x < features$mean.y
          & features$mean.x < features$mean.z
          & features$var.x < 4 
          & features$var.y < 4
          & features$var.z < 4)  , c("predictedbehavior")] <- "Vigilant"

features[500:510, ]
