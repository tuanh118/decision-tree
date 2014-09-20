# Create an empty data frame (of zeroes)
df <- matrix(rep(0, 1000), nrow = 100, ncol = 10)
df <- as.data.frame(df)

# Fill in random values
set.seed(118)
for (i in c(1:10)) {
  df[, i] <- sample.int(n = 2, size = 100, replace = T) - 1
}
df

# Split into training (70%) and testing sets (30%)
library(caTools)
set.seed(1234)
split = sample.split(df$V10, SplitRatio = 0.7)
train = subset(df, split==TRUE)
test = subset(df, split==FALSE)

# Learn with prebuilt package
library(rpart)
tree.model.1 <- rpart(V10 ~ ., data = train)

# Accuracy

## Training
pred.train.1 <- round(predict(tree.model.1))
t <- table(pred.train.1, train$V10)
(t[1] + t[4]) / sum(t)

## Testing
pred.test.1 <- round(predict(tree.model.1, newdata = test))
t <- table(pred.test.1, test$V10)
(t[1] + t[4]) / sum(t)

# Learn with learn_tree.R
source("learn_tree.R")
tree.model.2 <- learn.tree(train)

## Predict train
train.pred <- predict.tree(tree.model.2, train)
t <- table(train.pred, train$V10)
(t[1] + t[4]) / sum(t)

## Predict test
test.pred <- predict.tree(tree.model.2, test)
t <- table(test.pred, test$V10)
(t[1] + t[4]) / sum(t)
