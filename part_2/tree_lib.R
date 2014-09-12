# A function that takes a data frame of observations as input
#   and clean the data frame to satisfy some further requirements
clean <- function(df) {
  colnames(df)[ncol(df)] <- "Class"
  df
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