# Import libraries
source("learn_tree.R")
source("cond_entropy.R")

# Create data frame
df <- data.frame(nigeria  = c(1, 0, 0, 1, 0, 1, 0, 1, 0, 1), 
                 viagra   = c(0, 1, 0, 0, 0, 1, 1, 0, 0, 0), 
                 learning = c(0, 0, 0, 1, 0, 0, 1, 0, 0, 0), 
                 class    = c(1, 1, 0, 0, 0, 1, 0, 1, 0, 1))
df

# Learn a tree model
learn.tree(df)
