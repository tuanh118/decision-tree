# ID3_decision_tree.R: A file that runs the ID3 algorithm using command line.
#   It accepts two arguments that specify a training file and a test file.
#   It prints out the learned tree along with accuracies of the tree on
#   the training and test sets.
options(warn = -1)
library(methods)

# Read arguments
args <- commandArgs(trailingOnly = T)

# Read data set
train <- read.table(args[1], header = T)
test <- read.table(args[2], header = T)

# Learn
source("learn_tree.R")
tree.model <- learn.tree(train)

## Print the tree
cat(string(tree.model))

# Predict 

## Training set
train.pred <- predict.tree(tree.model, train)
t <- table(train.pred, train$class)
cat("\nAccuracy on training set (", sum(t), " instances):  ", format(round(100*(t[1] + t[4]) / sum(t), 1), nsmall = 1), "%\n", sep = "")

## Test set
test.pred <- predict.tree(tree.model, test)
t <- table(test.pred, test$class)
cat("\nAccuracy on test set (", sum(t), " instances):  ", format(round(100*(t[1] + t[4]) / sum(t), 1), nsmall = 1), "%\n", sep = "")