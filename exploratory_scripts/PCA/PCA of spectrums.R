# PCA with pure behavior epochs

library(dplyr)
library(matlab)
library(ggplot2)
library(data.table)
library(reshape2)
library(reshape)
library(stringr)


############ PCA of powers on all 3 axis, all behaviors #####################################

pca.obj <- prcomp(widePCA4[, 6:ncol(widePCA4)],
              center = TRUE, scale = TRUE)

str(pca.obj)

dtp <- data.frame('behaviorcat' = widePCA4$behaviorcat, pca.obj$x[,1:2]) # the first two componets are selected (NB: you can also select 3 for 3D plottings or 3+)
ggplot(data = dtp) + 
        geom_point(aes(x = PC1, y = PC2, col = behaviorcat)) + 
        theme_minimal() +
        ggtitle("PCA of Power Spectrum in x, y, and z")


str(pca.obj$rotation)
pcarot <- as.data.frame(pca.obj$rotation)
str(pcarot)
pcarot[ , 1]
plot(pcarot[, 1], type="ln")
plot(pcarot[1:101, 1], type="ln")
plot(pcarot[102:202, 1], type="ln")
plot(pcarot[203:303, 1], type="ln")

#plot portion of explained variance
summary(pca.obj)
eigs <- pca.obj$sdev^2
myeigs <- eigs / sum(eigs)
plot(myeigs, xlim=c(0, 10)) # look for inflection point where adding more PCs does not explain more variation

############ PCA on all x axis #####################################
ncol(widePCA3)
widePCA4.x <- widePCA4[, 6:206] # for x values only

pca.x <- prcomp(widePCA4.x,  
                  center = TRUE, scale = TRUE)

str(pca.x)

dtp <- data.frame('behaviorcat' = widePCA4$behaviorcat, pca.x$x[,1:2]) # the first two componets are selected (NB: you can also select 3 for 3D plottings or 3+)
ggplot(data = dtp) + 
        geom_point(aes(x = PC1, y = PC2, col = behaviorcat)) + 
        theme_minimal() +
        ggtitle("PCA of Power Spectrum in x")


str(pca.x$rotation)
pcarot <- as.data.frame(pca.x$rotation)
str(pcarot)
pcarot[ , 1]
plot(pcarot[, 1], type="ln")
plot(pcarot[1:101, 1], type="ln")
plot(pcarot[102:202, 1], type="ln")
plot(pcarot[203:303, 1], type="ln")

#plot portion of explained variance
summary(pca.x)
eigs <- pca.x$sdev^2
myeigs <- eigs / sum(eigs)
plot(myeigs, xlim=c(0, 10)) # look for inflection point where adding more PCs does not explain more variation

############ PCA on all y axis #####################################

widePCA4.y <- widePCA4[, 107:207] # for y values only

pca.y <- prcomp(widePCA4.y,  
                center = TRUE, scale = TRUE)

str(pca.y)

dtp <- data.frame('behaviorcat' = widePCA4$behaviorcat, pca.y$x[,1:2]) # the first two componets are selected (NB: you can also select 3 for 3D plottings or 3+)
ggplot(data = dtp) + 
        geom_point(aes(x = PC1, y = PC2, col = behaviorcat)) + 
        theme_minimal() +
        ggtitle("PCA of Power Spectrum in y")


str(pca.y$rotation)
pcarot <- as.data.frame(pca.y$rotation)
str(pcarot)
pcarot[ , 1]
plot(pcarot[, 1], type="ln")
plot(pcarot[1:101, 1], type="ln")
plot(pcarot[102:202, 1], type="ln")
plot(pcarot[203:303, 1], type="ln")

#plot portion of explained variance
summary(pca.y)
eigs <- pca.y$sdev^2
myeigs <- eigs / sum(eigs)
plot(myeigs, xlim=c(0, 10)) # look for inflection point where adding more PCs does not explain more variation

############ PCA on all z axis #####################################

widePCA4.z <- widePCA4[ , 208:ncol(widePCA4)] # for z values only

pca.z <- prcomp(widePCA4.z,  
                center = TRUE, scale = TRUE)

str(pca.z)

dtp <- data.frame('behaviorcat' = widePCA4$behaviorcat, pca.z$x[,1:2]) # the first two componets are selected (NB: you can also select 3 for 3D plottings or 3+)
ggplot(data = dtp) + 
        geom_point(aes(x = PC1, y = PC2, col = behaviorcat)) + 
        theme_minimal() +
        ggtitle("PCA of Acceleration in z")


str(pca.z$rotation)
pcarot <- as.data.frame(pca.z$rotation)
str(pcarot)
pcarot[ , 1]
plot(pcarot[, 1], type="ln")
plot(pcarot[1:101, 1], type="ln")
plot(pcarot[102:202, 1], type="ln")
plot(pcarot[203:303, 1], type="ln")

#plot portion of explained variance
summary(pca.z)
eigs <- pca.z$sdev^2
myeigs <- eigs / sum(eigs)
plot(myeigs, xlim=c(0, 10)) # look for inflection point where adding more PCs does not explain more variation

############ PCA for 2 behaviors on all 3 axes #####################################
backupwidePCA4 <- widePCA4
widePCA4 <- widePCA4[widePCA4$behaviorcat == "RunOnly" | widePCA4$behaviorcat == "VigilantOnly", ]

# view some make sure it worked
widePCA4[1:30, 1:5]
widePCA4[1000:1030, 1:5]
nrow(widePCA4) # this should be equal to the number of runOnly and VigilantOnly in the orig dataset

# check that it is...
avlabs2behav <- avlabs[avlabs$behaviorcat == "RunOnly" | avlabs$behaviorcat == "VigilantOnly", ]
head(avlabs2behav)
nrow(avlabs2behav) / 200 == nrow(widePCA4) # this should be true

dim(widePCA4)
head(widePCA4)
max(widePCA4[, 6:ncol(widePCA4)])

first200<-widePCA4[, 6:ncol(widePCA4)][,1:200]
tail(first200)
plot(unlist(first200[1,]), type="l")
plot(unlist(first200), type="l")
plot(unlist(first200[15,])~c(1:200), type="l")
plot(unlist(first200[31,])~c(1:200), type="l")
plot(unlist(first200[106,])~c(1:200), type="l")
plot(unlist(first200[8547,])~c(1:200), type="l")
max(first200)
min(first200)

pca.obj <- prcomp(widePCA4[, 6:ncol(widePCA4)]
                  #, center = TRUE, scale = TRUE
                  )
str(pca.obj)
dim(pca.obj$x)
head(pca.obj$x)

dtp <- data.frame('behaviorcat' = widePCA4$behaviorcat, pca.obj$x[,1:2]) # the first two componets are selected (NB: you can also select 3 for 3D plottings or 3+)
ggplot(data = dtp) + 
        geom_point(aes(x = PC1, y = PC2, col = behaviorcat)) + 
        theme_minimal() +
        ggtitle("PCA of Power Spectrum for Run and Vigilant in x, y, and z")


str(pca.obj$rotation)
pcarot <- as.data.frame(pca.obj$rotation)
str(pcarot)
pcarot[ , 1]
plot(pcarot[, 1], type="l", main="first comp")
plot(pcarot[, 2], type="l", main="second comp")
plot(pcarot[, 3], type="l", main="third comp")

plot(pcarot[1:201, 1], type="l")
plot(pcarot[102:403, 1], type="l")
plot(pcarot[203:604, 1], type="l")

# look at a vig only raw data
myvec <- c(1:200)
firstrow <- widePCA4[1, 6:205]
firstrow <- unlist(firstrow)
plot(firstrow~myvec)

# now look at a run only raw data
widePCA4[widePCA4$behaviorcat=="RunOnly", ]
myvec <- c(1:200)
row99 <- widePCA4[99, 6:205]
row99 <- unlist(row99)
plot(row99~myvec)

#plot portion of explained variance, number of components
summary(pca.obj)
eigs <- pca.obj$sdev^2
myeigs <- eigs / sum(eigs)
plot(myeigs, xlim=c(0, 10)) # look for inflection point where adding more PCs does not explain more variation


################# pca with log of powers 
min(widePCA4[, 6:ncol(widePCA4)])

wideplus1 <- widePCA4[, 6:ncol(widePCA4)] + .01 # add 1 because cannoy take log of 0
min(wideplus1)

logwide <- log(wideplus1)

pca.obj <- prcomp(logwide
                  #, center = TRUE, scale = TRUE
                )

dtp <- data.frame('behaviorcat' = widePCA4$behaviorcat, pca.obj$x[,1:2]) # the first two componets are selected (NB: you can also select 3 for 3D plottings or 3+)
ggplot(data = dtp) + 
        geom_point(aes(x = PC1, y = PC2, col = behaviorcat)) + 
        theme_minimal() +
        ggtitle("PCA of Power Spectrum for Run and Vigilant in x, y, and z")

pca.obj$x
pairs(pca.obj$x[,1:5])

#plot portion of explained variance, number of components
summary(pca.obj)
eigs <- pca.obj$sdev^2
myeigs <- eigs / sum(eigs)
plot(myeigs, xlim=c(0, 10)) # look for inflection point where adding more PCs does not explain more variation

pcarot <- as.data.frame(pca.obj$rotation)

# now look at PC 2 and 3
dtp <- data.frame('behaviorcat' = widePCA4$behaviorcat, pca.obj$x[,2:3]) # the second two componets are selected (NB: you can also select 3 for 3D plottings or 3+)
ggplot(data = dtp) + 
        geom_point(aes(x = PC2, y = PC3, col = behaviorcat)) + 
        theme_minimal() +
        ggtitle("PCA of Power Spectrum for Run and Vigilant in x, y, and z")

# now look at PC 1 and 3
dtp <- data.frame('behaviorcat' = widePCA4$behaviorcat, pca.obj$x[, c(1,3)]) # the second two componets are selected (NB: you can also select 3 for 3D plottings or 3+)
ggplot(data = dtp) + 
        geom_point(aes(x = PC1, y = PC3, col = behaviorcat)) + 
        theme_minimal() +
        ggtitle("PCA of Power Spectrum for Run and Vigilant in x, y, and z")

