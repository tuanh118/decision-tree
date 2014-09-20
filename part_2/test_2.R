# Import libraries
source("Tree.R")
source("learn_tree_2.R")

# Training set
train <- read.table("train2.dat", header = T)

## Learn
tree.model <- learn.tree.2(train, 0)

## Predict train
new.train <- train
new.train$status <- 1
new.train$predict <- -1
head(new.train)

train.pred <- tree.predict(tree.model, new.train)$predict
t <- table(train.pred, train$class)
t
(t[1] + t[4]) / sum(t)

##
train.compact <- new.train[c(11:15), ]
train.compact

train.c.pred <- tree.predict(tree.model, train.compact)$predict

## Predict test
test <- read.table("test2.dat", header = T)
head(test)

new.test <- test
new.test$status <- 1
new.test$predict <- -1
head(new.test)

test.pred <- tree.predict(tree.model, new.test)$predict
t <- table(test.pred, test$class)
(t[1] + t[4]) / sum(t)

##
test.compact <- new.test[c(1:5), ]
test.compact

test.c.pred <- tree.predict(tree.model, test.compact)$predict

# Test
l <- new("Leaf", label = 0)
b <- new("Branch", attr="tea", zero=new("Leaf", label=0), one=new("Leaf", label=1))
toString(l)
cat(toString(b))

## Create data frame
df <- data.frame(nigeria  = c(1, 0, 0, 1, 0, 1, 0, 1, 0, 1), 
                 viagra   = c(0, 1, 0, 0, 0, 1, 1, 0, 0, 0), 
                 learning = c(0, 0, 0, 1, 0, 0, 1, 0, 0, 0), 
                 class    = c(1, 1, 0, 0, 0, 1, 0, 1, 0, 1))

# Train
tree.model.2 <- learn.tree.2(df, 0)

# Add 2 more fields to the data frame
new.df <- df[c(2:6), ]
new.df$status <- 1
new.df$predict <- -1
new.df

tree.pred.2 <- tree.predict(tree.model.2, new.df)$predict
tree.pred.2
new.df$class
