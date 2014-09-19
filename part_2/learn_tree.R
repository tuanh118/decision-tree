# A function that takes a dataframe of the root and a list of
#   available attributes as input, then recursively learns a 
#   decision tree from that root using the ID3 algorithm
learn.tree <- function(df, depth) {
  # Basis: If the class field is pure or there is no available
  #   attribute, return the major class of the dataframe
  df <- as.data.frame(df)
  
  if (pure(df[, length(df)]) || length(colnames(df)) == 1) {
    cat(majority(df[, length(df)]))
  } else {
    # Recursion
    ce <- cond.entropy(df)
    min.attr.index <- which.min(ce)
    
    # Value vector of the attribute that has the min conditional entropy
    min.attr.vals <- df[, min.attr.index]
    
    for (val in unique(min.attr.vals)) {
      # Shrink the data frame
      new_df <- df[df[, min.attr.index] == val, ]
      
      # Noti
      margin <- paste(rep("| ", depth), collapse = "")
      cat("\n", margin, colnames(df[min.attr.index]), " = ", val, " : ")
      
      # Recursive call
      learn.tree(new_df[, -min.attr.index], depth + 1)
    }
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