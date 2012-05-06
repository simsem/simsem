# countMACS: Count the number of elements in means and covariance matrix

countMACS <- function(object) {
    modelType <- object@modelType
    p <- NA
    if (modelType == "CFA") {
        p <- nrow(object@LY@free)
    } else if (modelType == "Path") {
        p <- nrow(object@BE@free)
    } else if (modelType == "Path.exo") {
        p <- nrow(object@BE@free) + ncol(object@GA@free)
    } else if (modelType == "SEM") {
        p <- nrow(object@LY@free)
    } else if (modelType == "SEM.exo") {
        p <- nrow(object@LY@free) + nrow(object@LX@free)
    } else {
        stop("Something is wrong!")
    }
    macs <- p + (p * (p + 1)/2)
    return(macs)
} 
