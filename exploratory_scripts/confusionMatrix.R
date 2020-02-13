set.seed(1)
want <- sample(50, 40)
Iris <- iris[c(51:100, 101:150), ] ## only keep versicolor and virginica
## take our training and test sets
train <- droplevels(Iris[c((1:50)[want], (51:100)[want]), , drop = FALSE])
test <- droplevels(Iris[c((1:50)[-want], (51:100)[-want]), , drop = FALSE])

## fit the PCA
pc <- prcomp(train[, 1:4])

## create data frame for logistic regression
mydata <- data.frame(Species = train[, "Species"], pc$x)
## ...and fit the model
mod <- glm(Species ~ PC1, data = mydata, family = binomial)