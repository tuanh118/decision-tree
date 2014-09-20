# Import libraries
#source("Tree.R")
source("learn_tree.R")
options(warn = -1)

# Training set
train <- read.table("train2.dat", header = T)

## Learn
tree.model <- learn.tree(train)

## Predict train
train.pred <- predict.tree(tree.model, train)
t <- table(train.pred, train$class)
t
(t[1] + t[4]) / sum(t)

## Predict test
test <- read.table("test2.dat", header = T)
head(test)

test.pred <- predict.tree(tree.model, test)
t <- table(test.pred, test$class)
(t[1] + t[4]) / sum(t)