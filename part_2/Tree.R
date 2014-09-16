# Class definition
setClass("Tree")
setClass("Leaf", contains = "Tree",
         representation(label = "numeric"))
setClass("Branch", contains =  "Tree",
         representation(attr = "character", zero = "Tree", one = "Tree"))

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
            result <- paste(b@attr, "= 0:")
            if (class(b@zero) == "Branch") result <- paste(result, "\n|")
            result <- paste(result, toString(b@zero))
            
            result <- paste(result, b@attr, " = 1:", sep = "")
            if (class(b@one) == "Branch") result <- paste(result, "\n|")
            result <- paste(result, toString(b@one))
            
            result
          })

# Train
setGeneric("tree.predict", function(tree, df) standardGeneric("tree.predict"))
setMethod(tree.predict,
          signature(tree = "Leaf", df = "data.frame"),
          function(tree, df) {
            df[df$status == 1, ]$predict <- tree@label
            df
          })
setMethod(tree.predict,
          signature(tree = "Branch", df = "data.frame"),
          function(tree, df) {
            index <- which(colnames(df) == tree@attr)
            
            # attr = zero
            result <- df
            result[result[, index] == 1 & result$status == 1, ]$status <- 0
            result <- tree.predict(tree@zero, result)
            
            # attr = one
            result$status <- df$status
            result[result[, index] == 0 & result$status == 1, ]$status <- 0
            result <- tree.predict(tree@one, result)
            
            result
          })