library(ggplot2)

# start with running and vigilant only
widePCA4 # this is creaed in file PCA of spectrums.R
table(widePCA4$behaviorcat) # this should have only run and vigilant
widePCA4$behaviorcat <- factor(widePCA4$behaviorcat)

# select 1 training deer and 1 testing deer
table(widePCA4$DeerID)
table(widePCA4$behaviorcat) # this should have only run and vigilant

traindeer <- "NW1WF12" # 126 observations 
testdeer <- "WP2MM6" # 93 observations

# subset to do pca on just training deer
training <-widePCA4[widePCA4$DeerID==traindeer, ]
dim(training)
training$behaviorcat

# subset for testing deer
testing <-widePCA4[widePCA4$DeerID==testdeer, ]
dim(testing)
testing$behaviorcat
table(testing$behaviorcat)

# Prep training data for PCA 
min(training[, 6:ncol(training)]) # check to see if there are zeros
trainingplus1 <- training[, 6:ncol(training)] + .01 # add 1 because cannot take log of 0
min(trainingplus1)

logTrainDataX <- log(trainingplus1) # the predictors only. Power spectrum
TrainDataY <- testing$behaviorcat

######## PCA for training data
train.pca <- prcomp(logTrainDataX
                  #, center = TRUE, scale = TRUE
                  )
summary(train.pca)
str(train.pca)

######## plot
dtp <- data.frame('behaviorcat' = training$behaviorcat, train.pca$x[,1:2]) # the first two componets are selected (NB: you can also select 3 for 3D plottings or 3+)
ggplot(dtp) + 
  geom_point(aes(x = PC1, y = PC2, col = behaviorcat)) + 
  theme_minimal() +
  ggtitle("PCA of Power Spectrum for 1 Deer Run and Vigilant in x, y, and z")

train.pca$x
pairs(train.pca$x[,1:5])

#plot portion of explained variance, number of components
summary(train.pca)
eigs <- train.pca$sdev^2
myeigs <- eigs / sum(eigs)
plot(myeigs, xlim=c(0, 10)) # look for inflection point where adding more PCs does not explain more variation

#decide how many to keep 
keepPCAs <- 5

pcarot <- as.data.frame(train.pca$rotation)

# now look at PC 2 and 3
dtp <- data.frame('behaviorcat' = training$behaviorcat, train.pca$x[,2:3]) # the second two componets are selected (NB: you can also select 3 for 3D plottings or 3+)
ggplot(data = dtp) + 
  geom_point(aes(x = PC2, y = PC3, col = behaviorcat)) + 
  theme_minimal() +
  ggtitle("PCA of Power Spectrum for Run and Vigilant in x, y, and z")

# now look at PC 1 and 3
dtp <- data.frame('behaviorcat' = training$behaviorcat, train.pca$x[, c(1,3)]) # the second two componets are selected (NB: you can also select 3 for 3D plottings or 3+)
ggplot(data = dtp) + 
  geom_point(aes(x = PC1, y = PC3, col = behaviorcat)) + 
  theme_minimal() +
  ggtitle("PCA of Power Spectrum for Run and Vigilant in x, y, and z")


# explained variance
expl.var <- round(train.pca$sdev^2/sum(train.pca$sdev^2)*100) # percent explained variance

###########################

# Prep testing data for PCA 
min(testing[, 6:ncol(testing)]) # check to see if there are zeros
testingplus1 <- testing[, 6:ncol(testing)] + .01 # add 1 because cannot take log of 0
min(testingplus1)

logTestDataX <- log(testingplus1) # the predictors only

####################

## create data frame for logistic regression
length(training[ , "behaviorcat"])
dim(train.pca$x)
mydata <- data.frame(Behavior = training[ , "behaviorcat"], train.pca$x)
## ...and fit the model
mod <- glm(Behavior ~ PC1 + PC2, data = mydata, family = binomial("logit"), maxit = 100)

summary(mod)

###########################

#Predict the scores on PC1 for the test set data; 
#that is, rotate the test set using the same rotation used to form the PCs of the training data. 

test.p <- predict(train.pca, newdata = logTestDataX)

#Now use that to predict the class

pred <- predict(mod, newdata = data.frame(test.p), type = "response")
pred

###########################

predBehav <- factor(ifelse(pred >= 0.5, "Vigilant", "Run"))
table(testing$behaviorcat, predBehav)

###########################

# plot glm line using model
mod
summary(mod)

PCs <- as.data.frame(train.pca$x) 
PC1 <- PCs$PC1
PC2 <- PCs$PC2
PC3 <- PCs$PC3

b0 <- mod$coef[1]
a1 <- mod$coef[2]
a2 <- mod$coef[3]
a3 <- mod$coef[4]

PCs$y <- b0 + a1*PCs$PC1 + a2*PCs$PC2

OurPredictions <- 1/(1+ exp(-PCs$y))
OurPredBehaviors <- factor(ifelse(OurPredictions >= 0.5, "Vigilant", "Run"))
OurPredBehaviors == predBehav
table(training$behaviorcat, OurPredBehaviors)

############ training data - observed labels
dtp <- data.frame('behaviorcat' = training$behaviorcat, train.pca$x[,1:2]) # the first two componets are selected (NB: you can also select 3 for 3D plottings or 3+)
ggplot(dtp) + 
  geom_point(aes(x = PC1, y = PC2, color = behaviorcat)) + 
  theme_minimal() +
  ggtitle("PCA of Power Spectrum for 1 Deer Run and Vigilant in x, y, and z") +
  geom_abline(intercept = -b0, slope = -a1/a2)

#training data - expected labels
train.p <- predict(train.pca, newdata = logTrainDataX)
#Now use that to predict the class
train.pred <- predict(mod, newdata = data.frame(train.p), type = "response")
predBehav <- factor(ifelse(train.pred >= 0.5, "Vigilant", "Run"))
table(training$behaviorcat, predBehav)

x <- seq(from=-100, to=50, 0.1)
?seq
y <- -a1/a2*x + -b0



dtp <- data.frame('behaviorcat' = predBehav, train.pca$x[,1:2]) # the first two componets are selected (NB: you can also select 3 for 3D plottings or 3+)
ggplot(dtp) + 
  geom_point(aes(x = PC1, y = PC2, color = behaviorcat)) + 
  geom_point(aes(x = x, y = y)) + 
  theme_minimal() +
  ggtitle("PCA of Power Spectrum for 1 Deer Run and Vigilant in x, y, and z") 

plot(PC2~PC1)
points(y~x)

dtp <- data.frame('behaviorcat' = predBehav, train.pca$x[,1:2]) # the first two componets are selected (NB: you can also select 3 for 3D plottings or 3+)
ggplot(dtp) + 
  geom_point(aes(x = PC1, y = PC2, color = behaviorcat)) + 
  geom_point(aes(x = x, y = y)) + 
  theme_minimal() +
  ggtitle("PCA of Power Spectrum for 1 Deer Run and Vigilant in x, y, and z") +
  geom_abline(intercept = -b0, slope = -a1/a2)

############ test data - observed labels
testpcas <- test.p[ , c(1:2)]
dtp <- data.frame('behaviorcat' = testing$behaviorcat, testpcas) # the first two componets are selected (NB: you can also select 3 for 3D plottings or 3+)
ggplot(dtp) + 
  geom_point(aes(x = PC1, y = PC2, color = behaviorcat)) + 
  theme_minimal() +
  ggtitle("Observed behaviors test deer Run and Vigilant in x, y, and z") +
  geom_abline(intercept = b0, slope = -a1/a2)

# test data - predicted labels
dtp <- data.frame('behaviorcat' = predBehav, testpcas) # the first two componets are selected (NB: you can also select 3 for 3D plottings or 3+)
ggplot(dtp) + 
  geom_point(aes(x = PC1, y = PC2, color = behaviorcat)) + 
  theme_minimal() +
  ggtitle("Predicted behaviors test deer and Vigilant in x, y, and z") +
  geom_abline(intercept = b0, slope = -a1/a2)
