# fix date field - just a handy script in case I have to fix dates at some point

# import file from last step
setwd("C:/Users/Apryle D. Craig/Documents/PhD/Video_Analysis/Incubator/deerfear/data/stages_of_processing/results_of_step_06_countsToLabels")

accVidLabs <- readRDS("accVidLabs.dat")
head(accVidLabs)
str(accVidLabs)

# we want to view microseconds throughout
options(digits.secs = 3)
options(digits = 3)

# convert dates and add a new column to hold
allDeer$datetime <- paste(allDeer$VideoDate, allDeer$VideoTime, sep=" ")
allDeer$datetime <- strptime(allDeer$datetime, "%Y%m%d %H%M%OS")
allDeer$datetimemil <- allDeer$datetime + allDeer$ACCSecond