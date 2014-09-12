# Read training and testing sets
train <- read.table("train.dat", header = T)
test <- read.table("test.dat", header = T)

head(train)
str(train)
str(test)

# Build a Regression Tree Model
library(rpart)
library(rpart.plot)
tree = rpart(class ~ ., data=train)
prp(tree)

# Performance

## Training set
tree.train.pred = round(predict(tree))
table.train = table(tree.train.pred, train$class)

(table.train[1] + table.train[4]) / nrow(train) # 89.375% Accuracy

## Test set
tree.test.pred = round(predict(tree, newdata = test))
table.test = table(tree.test.pred, test$class)

(table.test[1] + table.test[4]) / nrow(test) # 87.19212 Accuracy