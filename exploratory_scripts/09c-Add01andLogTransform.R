



widePCA4

# log transform for PCA. (1) add 0.01 (2) do log transform
  
# (1) Add 0.01 data for PCA 
min(widePCA4[, 6:(ncol(widePCA4)-1)]) # check to see if there are zeros in the spectrum columns
widePCA4[, 6:(ncol(widePCA4)-1)] <- widePCA4[, 6:(ncol(widePCA4)-1)] + .01 # add 1 because cannot take log of 0
min(widePCA4[, 6:(ncol(widePCA4)-1)]) # check to see there are no zeros in the spectrum columns

# (2) log transform the values now
widePCA4[, 6:(ncol(widePCA4)-1)] <- log(widePCA4[, 6:(ncol(widePCA4)-1)]) 

logtransformed <- widePCA4

# save it 
setwd("~/Documents/PhD/Video_Analysis/Incubator/deerfear/data/stages_of_processing/forPCA")
saveRDS(logtransformed, file = "logtransformed.dat")

