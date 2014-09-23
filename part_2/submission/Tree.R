# Tree.R: A file that defines a Tree structure, including its fields (variables)
#   and its methods (functions).

# Class definition. A Tree may be a branch or a leaf. A branch has
#   two sub-branches corresponding to trees when its attribute is equal to zero 
#   and one. A leaf has a label.
setClass("Tree")
setClass("Leaf", contains = "Tree",
         representation(label = "numeric"))
setClass("Branch", contains =  "Tree",
         representation(attr = "character", zero = "Tree", one = "Tree", depth = "numeric"))

# Functions to convert a tree into a string in a pre-defined manner.
#   E.g.:
#   wesley = 0 :
#   | honor = 0 :
#   | | barclay = 0 : 1
#   | | barclay = 1 : 0
#   | honor = 1 :
#   | | tea = 0 : 0
#   | | tea = 1 : 1
#   wesley = 1 : 0
setGeneric("string", function(tree) standardGeneric("string"))
setMethod(string,
          signature = "Leaf",
          function(l) {
            paste(l@label, "\n")
          })
setMethod(string,
          signature = "Branch",
          function(b) {
            # Number of dashes needed
            margin <- paste(rep("| ", b@depth), collapse = "")

            # Sub-branch 0
	          result <- paste(margin, b@attr, " = 0 : ", sep = "")
            if (class(b@zero) == "Branch") result <- paste(result, "\n")
            result <- paste(result, string(b@zero), sep = "")
            
            # Sub-branch 1
            result <- paste(result, margin, b@attr, " = 1 : ", sep = "")
            if (class(b@one) == "Branch") result <- paste(result, "\n")
            result <- paste(result, string(b@one), sep = "")
            
            result
          })

# Functions to predict labels of a list of observations. Based on different values of
#   each attributes, observations will be passed from branch to branch and may end up
#   in different leaves with different labels.
setGeneric("predict.tree.rec", function(tree, df) standardGeneric("predict.tree.rec"))
setMethod(predict.tree.rec,
          signature(tree = "Leaf", df = "data.frame"),
          function(tree, df) {
            if (nrow(df[df$status == 1, ]) != 0)
              df[df$status == 1, ]$predict <- tree@label
            df
          })
setMethod(predict.tree.rec,
          signature(tree = "Branch", df = "data.frame"),
          function(tree, df) {
            # Find the attribute associated with current branch
            index <- which(colnames(df) == tree@attr)
            
            # Passed to other branch
            
            ## attr = zero
            result <- df
            if (nrow(result[result[, index] == 1 & result$status == 1, ]) != 0)
              result[result[, index] == 1 & result$status == 1, ]$status <- 0
            result <- predict.tree.rec(tree@zero, result)
            
            ## attr = one
            result$status <- df$status
            if (nrow(result[result[, index] == 0 & result$status == 1, ]) != 0)
              result[result[, index] == 0 & result$status == 1, ]$status <- 0
            result <- predict.tree.rec(tree@one, result)
            
            result
          })

# A supporting function to preprocess a data frame before prediction.
#   It assigns a field (status) to specify if an observation is active
#   on a branch and a field (predict) to contain labels.
preprocess.df <- function(df) {
  df$status <- 1
  df$predict <- -1
  df
}

# A function that accepts a tree model and a data frame as its input,
#   then returns a vector of predicted labels.
predict.tree <- function(tree, df) {
  predict.tree.rec(tree, preprocess.df(df))$predict
}