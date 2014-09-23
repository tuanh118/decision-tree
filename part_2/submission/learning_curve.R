#####################################
# Creates a learning curve from training 
# and test data sets.
# 
# Introduction to Machine Learning
# Assignment 1
# Isaac Butterfield, Tuan Anh Pham
#####################################

options(warn = -1)
source("learn_tree.R")

# read in values from text files
train <- read.table("train.dat", header = T)
test <- read.table("test.dat", header = T)

# parameters
length = 25
iterations = 10

# create data frame of sample sizes covering the range of the training data
v <- seq(nrow(train)/length, nrow(train), length = length)
v = as.data.frame(v)

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

# find average accuracy over 25 samples for each sample size
set.seed(1234)
learning_curve <- data.frame(v, apply(v, MARGIN = 1, FUN = function(size) mean(replicate(iterations, getAccuracy(train, size, test)))))

# create plot
plot(learning_curve, xaxp = c(0, nrow(train), 5), type = "l", col = "navy", pch = 19, 
     main = "Learning Curve of ID3 Decision Tree", 
     xlab = "Training set Size",
     ylab = "Test set Accuracy (%)")
points(learning_curve, col = "red", pch = 19)

# save plot to file
dev.copy(png, file="learning_curve.png")
dev.off()
