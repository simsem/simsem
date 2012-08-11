## # tagHeaders: This element will add names in each element of a vector or will add row and columns names of a matrix with variable or
## # factor names

## setMethod("tagHeaders", signature = "VirtualRSet", definition = function(object) {
##     ny <- NULL
##     nx <- NULL
##     nk <- NULL
##     ne <- NULL
##     modelType <- object@modelType
##     if (modelType == "CFA") {
##         ne <- ncol(object@LY)
##         ny <- nrow(object@LY)
##     } else if (modelType == "Path") {
##         ny <- nrow(object@PS)
##     } else if (modelType == "Path.exo") {
##         nx <- ncol(object@GA)
##         ny <- nrow(object@PS)
##     } else if (modelType == "SEM") {
##         ne <- ncol(object@LY)
##         ny <- nrow(object@LY)
##     } else if (modelType == "SEM.exo") {
##         ne <- ncol(object@LY)
##         ny <- nrow(object@LY)
##         nk <- ncol(object@LX)
##         nx <- nrow(object@LX)
##     }
##     names.y <- NULL
##     names.x <- NULL
##     names.e <- NULL
##     names.k <- NULL
##     if (!is.null(ny)) {
##         for (i in 1:ny) {
##             temp <- paste("y", i, sep = "")
##             names.y <- c(names.y, temp)
##         }
##     }
##     if (!is.null(nx)) {
##         for (i in 1:nx) {
##             temp <- paste("x", i, sep = "")
##             names.x <- c(names.x, temp)
##         }
##     }
##     if (!is.null(ne)) {
##         for (i in 1:ne) {
##             temp <- paste("e", i, sep = "")
##             names.e <- c(names.e, temp)
##         }
##     }
##     if (!is.null(nk)) {
##         for (i in 1:nk) {
##             temp <- paste("k", i, sep = "")
##             names.k <- c(names.k, temp)
##         }
##     }
##     if (!isNullObject(object@LY)) {
##         colnames(object@LY) <- names.e
##         rownames(object@LY) <- names.y
##     }
##     if (!isNullObject(object@TE)) {
##         colnames(object@TE) <- names.y
##         rownames(object@TE) <- names.y
##     }
##     if (!isNullObject(object@PS)) {
##         if (modelType == "Path" | modelType == "Path.exo") {
##             colnames(object@PS) <- names.y
##             rownames(object@PS) <- names.y
##         } else {
##             colnames(object@PS) <- names.e
##             rownames(object@PS) <- names.e
##         }
##     }
##     if (!isNullObject(object@BE)) {
##         if (modelType == "Path" | modelType == "Path.exo") {
##             colnames(object@BE) <- names.y
##             rownames(object@BE) <- names.y
##         } else {
##             colnames(object@BE) <- names.e
##             rownames(object@BE) <- names.e
##         }
##     }
##     if (!isNullObject(object@TY)) {
##         names(object@TY) <- names.y
##     }
##     if (!isNullObject(object@AL)) {
##         if (modelType == "Path" | modelType == "Path.exo") {
##             names(object@AL) <- names.y
##         } else {
##             names(object@AL) <- names.e
##         }
##     }
##     if (!isNullObject(object@LX)) {
##         colnames(object@LX) <- names.k
##         rownames(object@LX) <- names.x
##     }
##     if (!isNullObject(object@TD)) {
##         colnames(object@TD) <- names.x
##         rownames(object@TD) <- names.x
##     }
##     if (!isNullObject(object@PH)) {
##         if (modelType == "Path" | modelType == "Path.exo") {
##             colnames(object@PH) <- names.x
##             rownames(object@PH) <- names.x
##         } else {
##             colnames(object@PH) <- names.k
##             rownames(object@PH) <- names.k
##         }
##     }
##     if (!isNullObject(object@GA)) {
##         if (modelType == "Path" | modelType == "Path.exo") {
##             colnames(object@GA) <- names.x
##             rownames(object@GA) <- names.y
##         } else {
##             colnames(object@GA) <- names.k
##             rownames(object@GA) <- names.e
##         }
##     }
##     if (!isNullObject(object@TX)) {
##         names(object@TX) <- names.x
##     }
##     if (!isNullObject(object@KA)) {
##         if (modelType == "Path" | modelType == "Path.exo") {
##             names(object@KA) <- names.x
##         } else {
##             names(object@KA) <- names.k
##         }
##     }
##     if (!isNullObject(object@TH)) {
##         colnames(object@TH) <- names.y
##         rownames(object@TH) <- names.x
##     }
##     return(object)
## }) 
