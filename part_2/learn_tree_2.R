# A function that takes a dataframe of the root and a list of
#   available attributes as input, then recursively learns a 
#   decision tree from that root using the ID3 algorithm
learn.tree.2 <- function(df, depth) {
  source("Tree.R")
  source("cond_entropy.R")
  
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
    ce <- cond.entropy(df)
    min.attr.index <- which.min(ce)
    
    # New implementation
    df.zero <- df[df[, min.attr.index] == 0, ]
    tree.zero <- learn.tree.2(df.zero[, -min.attr.index], depth + 1)
    
    df.one <- df[df[, min.attr.index] == 1, ]
    tree.one <- learn.tree.2(df.one[, -min.attr.index], depth + 1)
    
    # Recursive call
    new("Branch", attr = colnames(df[min.attr.index]), zero = tree.zero, one = tree.one)
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