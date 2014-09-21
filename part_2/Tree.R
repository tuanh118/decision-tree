# Class definition
setClass("Tree")
setClass("Leaf", contains = "Tree",
         representation(label = "numeric"))
setClass("Branch", contains =  "Tree",
         representation(attr = "character", zero = "Tree", one = "Tree", depth = "numeric"))

# Print
setGeneric("toString", function(tree) standardGeneric("toString"))
setMethod(toString,
          signature = "Leaf",
          function(l) {
            paste(l@label, "\n")
          })
setMethod(toString,
          signature = "Branch",
          function(b) {
            margin <- paste(rep("| ", b@depth), collapse = "")
            #margin <- paste("level:", b@depth)

	          result <- paste(margin, b@attr, " = 0 : ", sep = "")
            if (class(b@zero) == "Branch") result <- paste(result, "\n")
            result <- paste(result, toString(b@zero), sep = "")
            
            result <- paste(result, margin, b@attr, " = 1 : ", sep = "")
            if (class(b@one) == "Branch") result <- paste(result, "\n")
            result <- paste(result, toString(b@one), sep = "")
            
            result
          })

## Preprocess data frame before prediction
preprocess.df <- function(df) {
  df$status <- 1
  df$predict <- -1
  df
}

# Tree Predict
predict.tree <- function(tree, df) {
  predict.tree.rec(tree, preprocess.df(df))$predict
}

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
            index <- which(colnames(df) == tree@attr)
            
            # attr = zero
            result <- df
            if (nrow(result[result[, index] == 1 & result$status == 1, ]) != 0)
              result[result[, index] == 1 & result$status == 1, ]$status <- 0
            result <- predict.tree.rec(tree@zero, result)
            
            # attr = one
            result$status <- df$status
            if (nrow(result[result[, index] == 0 & result$status == 1, ]) != 0)
              result[result[, index] == 0 & result$status == 1, ]$status <- 0
            result <- predict.tree.rec(tree@one, result)
            
            result
          })