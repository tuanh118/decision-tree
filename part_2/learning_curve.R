#####################################
# Creates a learning curve from training 
# and test data sets.
# 
# Introduction to Machine Learning
# Assignment 1
# Isaac Butterfield, Tuan Anh Pham
#####################################

source("learn_tree.R")
options(warn = -1)

# read in values from text files
train <- read.table("train2.dat", header = T)
test <- read.table("test2.dat", header = T)

# create data frame of sample sizes covering the range of the training data
v <- seq(nrow(train)/8, nrow(train), length = 8)
v = as.data.frame(v)

#  find average accuracy over 25 samples for each sample size 
learning_curve <- data.frame(v, apply(v, MARGIN = 1, FUN = function(x) mean(replicate(25, getAccuracy(train, x, test)))))

# create plot
plot(learning_curve, col="black", xlab = "Sample Size", ylab = "Accuracy")
lines(lowess(learning_curve), type = "l")

# function to find accuracy of sample
getAccuracy <- function(train, size, test) {
  
  # create sample
  sample <- train[sample(nrow(train), size), ]
  
  # create model of sample
  tree.model <- learn.tree(sample)
  
  # apply prediction to test
  test.pred <- predict.tree(tree.model, test)
  
  # return accuracy
  t <- table(test.pred, test$class)
  (t[1] + t[4]) / sum(t)
}
