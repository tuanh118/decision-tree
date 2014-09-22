# learn_tree.R: A file that learns an ID3 Tree model given a training data frame.
#   The tree model may contain several branches and leaves.

source("Tree.R")
source("cond_entropy.R")

# A function that takes a dataframe of the root (depth 0) as input,
#   then triggers its recursive version to learn tree.
learn.tree <- function(df) {
  learn.tree.rec(df, 0)
}

# A function that recursively learns a decision tree model using the ID3 algorithm.
learn.tree.rec <- function(df, depth) {
  # Remove attributes with only 1 value
  df <- as.data.frame(df)
  unique.values <- apply(df, MARGIN = 2, FUN = function(col) length(unique(col)))
  unique.values <- unique.values[-length(unique.values)]
  if (length(which(unique.values == 1)) > 0)
    df <- df[, -which(unique.values == 1)]
  df <- as.data.frame(df)
  
  # Basis: If the class field is pure or there is no available
  #   attribute, return the major class of the dataframe
  if (pure(df[, length(df)]) || length(colnames(df)) == 1) {
    new("Leaf", label = as.numeric(majority(df[, length(df)])))
  } else {    
  # Recursion
    # Find the attribute with minimum conditional entropy
    ce <- cond.entropy(df)
    min.attr.index <- which.min(ce)
    
    # Recursive calls
    df.zero <- df[df[, min.attr.index] == 0, ]
    tree.zero <- learn.tree.rec(df.zero[, -min.attr.index], depth + 1)
    
    df.one <- df[df[, min.attr.index] == 1, ]
    tree.one <- learn.tree.rec(df.one[, -min.attr.index], depth + 1)
    
    # Create a new branch with two learned children
    new("Branch", attr = colnames(df[min.attr.index]), zero = tree.zero, one = tree.one, depth = depth)
  }
}

# A function that takes a vector of classes as input and
#   tell if that vector is pure (consists of 1 class) or not
pure <- function(class) {
  length(unique(class)) == 1
}

# A function that takes a vector of classes as input 
#   and output the major class
majority <- function(class) {
  names(which.max(table(class)))
}