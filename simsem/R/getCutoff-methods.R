# getCutoff: This function will find a cutoff of each fit index based on a priori alpha level from sampling distributions of fit indices

setMethod("getCutoff", signature(object = "data.frame"), definition = function(object, alpha, revDirec = FALSE, usedFit = NULL) {
    if (is.null(usedFit)) 
        usedFit <- getKeywords()$usedFit
    percentile <- 1 - alpha
    if (revDirec) 
        percentile <- 1 - percentile
    object <- as.data.frame(object[, usedFit])
    temp <- rep(NA, ncol(object))
    temp <- apply(object, 2, quantile, probs = percentile, na.rm = TRUE)
    if ("TLI" %in% colnames(object)) 
        temp["TLI"] <- quantile(object[, "TLI"], 1 - percentile, na.rm = TRUE)
    if ("CFI" %in% colnames(object)) 
        temp["CFI"] <- quantile(object[, "CFI"], 1 - percentile, na.rm = TRUE)
    return(temp)
})

setMethod("getCutoff", signature(object = "SimResult"), definition = function(object, alpha, revDirec = FALSE, usedFit = NULL) {
    object <- clean(object)
    Result <- object@fit
    output <- getCutoff(Result, alpha, revDirec, usedFit)
    return(output)
})

setMethod("getCutoff", signature(object = "matrix"), definition = function(object, alpha, revDirec = FALSE, usedFit = NULL) {
    object <- as.data.frame(object)
    output <- getCutoff(object, alpha, revDirec, usedFit)
    return(output)
}) 
