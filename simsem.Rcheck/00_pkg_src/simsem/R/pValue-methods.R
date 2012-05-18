# pValue: Find a p-value from an object

setMethod("pValue", signature(target = "numeric", dist = "vector"), definition = function(target, dist, revDirec = FALSE) {
    if (revDirec) {
        return(mean(target <= dist, na.rm = TRUE))
    } else {
        return(mean(target >= dist, na.rm = TRUE))
    }
})

setMethod("pValue", signature(target = "numeric", dist = "data.frame"), definition = function(target, dist, revDirec = NULL, asLogical = FALSE) {
    if (length(target) != ncol(dist)) 
        stop("The length of target and the number of columns of dist are not equal")
    numVar <- length(target)
    if (is.null(revDirec)) {
        revDirec <- rep(FALSE, numVar)
    } else {
        if (length(revDirec) != numVar) 
            stop("The length of revDirec and the number of columns of dist are not equal")
    }
    if (asLogical) {
        result <- NULL
        for (i in 1:numVar) {
            if (revDirec[i]) {
                result <- cbind(result, target[i] <= dist[, i])
            } else {
                result <- cbind(result, target[i] >= dist[, i])
            }
        }
        return(result)
    } else {
        result <- rep(NA, numVar)
        for (i in 1:numVar) {
            result[i] <- pValue(target[i], dist[, i], revDirec[i])
        }
        return(result)
    }
})

setMethod("pValue", signature(target = "SimModelOut", dist = "SimResult"), definition = function(target, dist, usedFit = NULL) {
    dist <- clean(dist)
    if (is.null(usedFit)) 
        usedFit <- getKeywords()$usedFit
    revDirec <- !(usedFit %in% c("CFI", "TLI"))
    logicalMat <- pValue(target@fit[usedFit], dist@fit[, usedFit], revDirec, asLogical = TRUE)
    result <- apply(logicalMat, 2, mean, na.rm = TRUE)
    names(result) <- usedFit
    andRule <- mean(apply(logicalMat, 1, all), na.rm = TRUE)
    orRule <- mean(apply(logicalMat, 1, any), na.rm = TRUE)
    c(result, andRule = andRule, orRule = orRule)
}) 
