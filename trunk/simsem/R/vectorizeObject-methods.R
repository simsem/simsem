# vectorizeObject: Change an object to a vector with labels

setMethod("vectorizeObject", signature(object = "vector", labels = "vector"), 
    definition = function(object, labels) {
        result <- as.vector(object[!is.na(labels)])
        names(result) <- as.vector(labels[!is.na(labels)])
        return(result)
    })

setMethod("vectorizeObject", signature(object = "matrix", labels = "matrix"), 
    definition = function(object, labels, symmetric = FALSE) {
        result <- NULL
        if (symmetric) {
            object_lower <- object[lower.tri(object, diag = TRUE)]
            labels_lower <- labels[lower.tri(labels, diag = TRUE)]
            result <- vectorizeObject(object_lower, labels_lower)
        } else {
            result <- as.vector(object[!is.na(labels)])
            names(result) <- as.vector(labels[!is.na(labels)])
        }
        return(result)
    })

setMethod("vectorizeObject", signature(object = "VirtualRSet", labels = "SimLabels"), 
    definition = function(object, labels) {
        result <- NULL
        if (!isNullObject(labels@LX)) 
            result <- c(result, vectorizeObject(object@LX, labels@LX))
        if (!isNullObject(labels@LY)) 
            result <- c(result, vectorizeObject(object@LY, labels@LY))
        if (!isNullObject(labels@GA)) 
            result <- c(result, vectorizeObject(object@GA, labels@GA))
        if (!isNullObject(labels@BE)) 
            result <- c(result, vectorizeObject(object@BE, labels@BE))
        if (!isNullObject(labels@PH)) 
            result <- c(result, vectorizeObject(object@PH, labels@PH, symmetric = TRUE))
        if (!isNullObject(labels@PS)) 
            result <- c(result, vectorizeObject(object@PS, labels@PS, symmetric = TRUE))
        if (!isNullObject(labels@TD)) 
            result <- c(result, vectorizeObject(object@TD, labels@TD, symmetric = TRUE))
        if (!isNullObject(labels@TH)) 
            result <- c(result, vectorizeObject(object@TH, labels@TH))
        if (!isNullObject(labels@TE)) 
            result <- c(result, vectorizeObject(object@TE, labels@TE, symmetric = TRUE))
        if (!isNullObject(labels@KA)) 
            result <- c(result, vectorizeObject(object@KA, labels@KA))
        if (!isNullObject(labels@AL)) 
            result <- c(result, vectorizeObject(object@AL, labels@AL))
        if (!isNullObject(labels@TX)) 
            result <- c(result, vectorizeObject(object@TX, labels@TX))
        if (!isNullObject(labels@TY)) 
            result <- c(result, vectorizeObject(object@TY, labels@TY))
        return(result)
    }) 
