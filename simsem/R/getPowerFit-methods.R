# getPowerFit: This function will find a power of each fit index based on specified cutoffs of each fit index

setMethod("getPowerFit", signature(altObject = "data.frame"), definition = function(altObject, cutoff, revDirec = FALSE, usedFit = NULL) {
    if (is.null(usedFit)) 
        usedFit <- getKeywords()$usedFit
    if (is.null(names(cutoff)) && length(cutoff) == 7) 
        names(cutoff) <- usedFit
    common.name <- Reduce(intersect, list(colnames(altObject), names(cutoff), usedFit))
    temp <- rep(NA, length(common.name))
    names(temp) <- common.name
    altObject <- as.data.frame(altObject[, common.name])
    cutoff <- cutoff[common.name]
    for (i in 1:length(common.name)) {
        temp[i] <- pValue(altObject[, i], cutoff[i], revDirec)
    }
    if ("TLI" %in% common.name) 
        temp["TLI"] <- 1 - temp["TLI"]
    if ("CFI" %in% common.name) 
        temp["CFI"] <- 1 - temp["CFI"]
    return(temp)
})

setMethod("getPowerFit", signature(altObject = "SimResult"), definition = function(altObject, cutoff, revDirec = FALSE, usedFit = NULL) {
    altObject <- clean(altObject)
    Result <- altObject@fit
    output <- getPowerFit(Result, cutoff, revDirec, usedFit)
    return(output)
})

setMethod("getPowerFit", signature(altObject = "matrix"), definition = function(altObject, cutoff, revDirec = FALSE, usedFit = NULL) {
    object <- as.data.frame(altObject)
    output <- getPowerFit(object, cutoff, revDirec, usedFit)
    return(output)
}) 
