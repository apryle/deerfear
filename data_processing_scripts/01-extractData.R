require(parallel)

mywd <- "C:/Users/Apryle D. Craig/Documents/PhD/Video_Analysis/Incubator/deerfear"
setwd(mywd)
list.files()

# create a function to extra data from each .dat file
extraction <- function(f){
  
  #f=1
  file = alldats10[f]
  fnlist <- strsplit(file, split = "[/]")[[1]]
  
  #get deer id, date, time, seconds for each file
  idList <- strsplit(fnlist, split = "[_]")[[1]]
  id <- idList[1]
  
  fulldate <- fnlist[6]
  datelist <- strsplit(fulldate, split = "[-]")[[1]][3]
  viddate <- substr(datelist, start = 1, stop = 8)
  vidtime <- substr(datelist, start = 9, stop = 14)
  
  #extract data from dat file
  wholeTable <- tryCatch(read.table(file, sep = "X"), error=function(e) NULL)
  
  
  #separate gps data from activity data
  gpsrows <- max(grep("GPS", wholeTable[,1])) # how many rows contain GPS data?
  
  #pull gps rows
  myGPSdataMess <- tryCatch(read.table(file, sep = " ", nrows = gpsrows), error=function(e) NULL)
  
  #append gps table with deer id, date, etc
  myGPSdataMess$deerID <- id
  myGPSdataMess$viddate <- viddate
  myGPSdataMess$vidtime <- vidtime
  
  #pull activity rows, which comes after the gps rows in the datafile
  myActivityMess <- tryCatch(read.table(file, skip=gpsrows, sep=" "), error=function(e) NULL)
  
  #append acc table with deer id, date, etc
  myActivityMess$deerID <- id
  myActivityMess$viddate <- viddate
  myActivityMess$vidtime <- vidtime
  
  out <- list() # right this into a csv instead
  out$gps <- myGPSdataMess
  out$acc <- myActivityMess
  return(out)
  
}

#######################################################
# START extraction ####################################
# this step should only have to happen once PER Harddrive ###########

# #set folder to directory level (external hard drive root), so it must go into each deer folder, then datefolder, and then zerofolder to fetch dat files
# # now set it to pull from the external hd
# setwd("E:/")
# list.files()
# 
# # create the alldats.dat file
# alldats <- list.files("./", ".dat$", recursive = TRUE)
# head(alldats)
# tail(alldats)
# length(alldats) # should be 500547; 314527 for season 23
# 
# #save output to appropriate data file
# setwd("C:/Users/Apryle D. Craig/Documents/PhD/Video_Analysis/Incubator/deerfear/data/extracted_from_harddrives/Results_from_step_01a/")
# list.files()
# save(alldats, file = "alldats23.dat") # save it so I don't have to run the list.files function each time.

# END extraction  ####################################
######################################################

# open the file containing a list of all dat files within the external harddrive 
setwd("C:/Users/Apryle D. Craig/Documents/PhD/Video_Analysis/Incubator/deerfear/data/extracted_from_harddrives/Results_from_step_01a/")
list.files()
load("alldats.dat") # this is a file that was created using the code directly below

# now switch directories so that it's looking for each file on the ext harddrive
setwd("E:/")

head(alldats)
length(alldats)

numcores <- detectCores()-1
cl <- makeCluster(numcores)


# cannot extract all data at once; crashes R. Extract in chunks...

######## CHUNK 1: harddrive 1, 1:100000 ################################
# split the dataset. run the first chunk of dats
alldats10 <- alldats[1:100000] 
clusterExport(cl, "alldats10")

# it doesn't want to run the first file, alldats.dat, so start it at element 2
masterall <- parLapply(cl, 2:length(alldats10), extraction)

#save chunk 1 into a dat file
saveRDS(masterall, file = paste0(mywd, "/data/extracted_from_harddrives/Results_from_step_01b/drive1masterall1_100000.dat"))
stopCluster(cl)

######## CHUNK 2: harddrive 1 #############################
# next chunk of dat files
alldats10 <- alldats[100001:200000] 
clusterExport(cl, "alldats10")

# it doesn't want to run the first file, alldats.dat, so start it at element 2
masterall <- parLapply(cl, 2:length(alldats10), extraction)

#save chunk 2 into a dat file
saveRDS(masterall, file = paste0(mywd, "/data/extracted_from_harddrives/Results_from_step_01b/drive1masterall100001_200000.dat"))

stopCluster(cl)

######## CHUNK 3: harddrive 1 #############################
alldats10 <- alldats[200001:length(alldats)]  
clusterExport(cl, "alldats10")

# it doesn't want to run the first file, alldats.dat, so start it at element 2
masterall <- parLapply(cl, 2:length(alldats10), extraction)

#save chunk 3 into a dat file
saveRDS(masterall, file = paste0(mywd, "/data/extracted_from_harddrives/Results_from_step_01b/drive1masterall200001_end.dat"))

stopCluster(cl)

######## CHUNK 4: harddrive 2 #############################
alldats10 <- alldats[1:length(alldats)]  
clusterExport(cl, "alldats10")

# it doesn't want to run the first file, alldats.dat, so start it at element 2
masterall <- parLapply(cl, 2:length(alldats10), extraction)

#save chunk 4 into a dat file
saveRDS(masterall, paste0(mywd, "/data/extracted_from_harddrives/Results_from_step_01b/masterallseason23.dat"))

stopCluster(cl)
