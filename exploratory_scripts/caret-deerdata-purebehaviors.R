# tutorial from http://www.rebeccabarter.com/blog/2017-11-17-caret_tutorial/

# dataset
mywd <- "C:/Users/Apryle D. Craig/Documents/PhD/Video_Analysis/Incubator/deerfear"
setwd(paste0(mywd, "/data/stages_of_processing/forPCA"))
list.files()
purebehaviorspectrums <- readRDS("purebehaviorspectrums2.dat")


library(caret)
library(ranger)
library(tidyverse)
library(e1071)

data("abalone")
abalone_data <- abalone

# load in column names
colnames(abalone_data) <- c("sex", "length", "diameter", "height", 
                            "whole.weight", "shucked.weight", 
                            "viscera.weight", "shell.weight", "age")
# add a logical variable for "old" (age > 10)
abalone_data <- abalone_data %>%
  mutate(old = age > 10) %>%
  # remove the "age" variable
  select(-age)

# split into training and testing
set.seed(23489)
train_index <- sample(1:nrow(abalone_data), 0.9 * nrow(abalone_data))
abalone_train <- abalone_data[train_index, ]
abalone_test <- abalone_data[-train_index, ]

# remove the original dataset
rm(abalone_data)
# view the first 6 rows of the training data
head(abalone_train)

dim(abalone_train)

# fit a random forest model (using ranger). 
# model old as a function of all of the other variables
rf_fit <- train(as.factor(old) ~ ., 
                data = abalone_train, 
                method = "ranger")

rf_fit
