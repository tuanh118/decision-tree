# A function that takes a data frame and a vector of available attributes, 
#   then returns a vector of conditional entropy values corresponding to
#   the vector of attributes
cond.entropy <- function(df) {
  class <- df[, length(df)]
  ce <- rep(0, length(df) - 1)
  
  # Loop through the list of available attributes
  for (i in 1:(length(df) - 1)) {
    ce[i] <- uni.cond.entropy(df[, i], class)
  }
  
  # Return the vector of conditional entropy values
  ce
}

# A function that takes 1 vector of values of 1 attribute and 1 vector of
#   values of classification, then returns the corresponding conditional
#   entropy value
uni.cond.entropy <- function(attr, class) {
  # Verify that the two vectors have same length
  if (length(attr) != length(class)) stop("Two vectors must have same length")
  
  # Number of observations broken down by attribute and class
  t <- table(attr, class)
  
  # Compute the weights of children
  n <- length(attr)
  p <- rowSums(t) / n
  
  # Compute the entropy values of children
  t.norm <- t / rowSums(t) # Normalize values into probability distribution form
  h <- apply(t.norm, MARGIN = 1, FUN = function(row) entropy(row))
  
  # Result
  sum(h * p)
}

# A function that takes a probability distribution vector as input 
#   and produce the entropy value of that distribution
# Note: Assume 0lg0 = 0
entropy <- function(dist) {  
  # Verify that none of the probabilities is negative
  if (sum(dist < 0) > 0) stop("Invalid probability distribution: Negative value")
  
  # Verify that sum of probabilities in the input distribution is 1
  if (sum(dist) != 1) stop("Invalid probability distribution: Sum not equal to 1")
  
  # Compute a vector of entropy components
  ent.comps <- -dist * log2(dist)
  
  # Exemption: 0lg0 = 0
  ent.comps[is.nan(ent.comps)] <- 0
  
  # Output sum of vector elements as the result
  sum(ent.comps)
}