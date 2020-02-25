library(caret)

setwd("~/Documents/PhD/Video_Analysis/Incubator/deerfear/data/stages_of_processing/forPCA")
list.files()
trainingdeer <- readRDS("trainingdeer.dat")

nrow(trainingdeer)

## 90% of the sample size
smp_size <- floor(0.90 * nrow(trainingdeer))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(trainingdeer)), size = smp_size)

trainingdeer <- trainingdeer[train_ind, ]
validationdeer <- trainingdeer[-train_ind, ]

# see what the behavior splits look like
# do we have behaviors represented in both training and validation
table(trainingdeer$collapsedBehaviors)
table(validationdeer$collapsedBehaviors)
