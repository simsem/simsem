setGeneric("summaryShort", function(object, ...) {
    return(standardGeneric("summaryShort"))
})

setGeneric("getCutoff", function(object, alpha, revDirec = FALSE, usedFit = NULL, 
    ...) {
    return(standardGeneric("getCutoff"))
})

setGeneric("plotCutoff", function(object, ...) {
    return(standardGeneric("plotCutoff"))
})

setGeneric("getPowerFit", function(altObject, cutoff, revDirec = FALSE, usedFit = NULL, 
    ...) {
    return(standardGeneric("getPowerFit"))
})

setGeneric("pValue", function(target, dist, ...) {
    return(standardGeneric("pValue"))
})

setGeneric("getPowerFitNested", function(altNested, altParent, cutoff, ...) {
    return(standardGeneric("getPowerFitNested"))
})

setGeneric("getPowerFitNonNested", function(dat2Mod1, dat2Mod2, cutoff, ...) {
    return(standardGeneric("getPowerFitNonNested"))
}) 
