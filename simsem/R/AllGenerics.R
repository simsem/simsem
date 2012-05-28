setGeneric("run", function(object, ...) {
    return(standardGeneric("run"))
})

setGeneric("adjust", function(target, value, pos, numAsFixed = TRUE) {
    return(standardGeneric("adjust"))
})

setGeneric("combineObject", function(object1, object2, ...) {
    return(standardGeneric("combineObject"))
})

setGeneric("constrainMatrices", function(object, SimEqualCon, ...) {
    return(standardGeneric("constrainMatrices"))
})

setGeneric("countFreeParameters", function(object, ...) {
    return(standardGeneric("countFreeParameters"))
})

setGeneric("createImpliedMACS", function(object, ...) {
    return(standardGeneric("createImpliedMACS"))
})

setGeneric("setOpenMxObject", function(param, start) {
    return(standardGeneric("setOpenMxObject"))
})

setGeneric("isNullObject", function(target) {
    return(standardGeneric("isNullObject"))
})

setGeneric("divideObject", function(object, constant, ...) {
    return(standardGeneric("divideObject"))
})

setGeneric("makeLabels", function(object, ...) {
    return(standardGeneric("makeLabels"))
})

setGeneric("simModel", function(object, ...) {
    return(standardGeneric("simModel"))
})

setGeneric("startingValues", function(object, trial, ...) {
    return(standardGeneric("startingValues"))
})

setGeneric("summaryShort", function(object, ...) {
    return(standardGeneric("summaryShort"))
})

setGeneric("tagHeaders", function(object, ...) {
    return(standardGeneric("tagHeaders"))
})

setGeneric("getCutoff", function(object, alpha, revDirec = FALSE, usedFit = NULL, ...) {
    return(standardGeneric("getCutoff"))
})

setGeneric("plotCutoff", function(object, ...) {
    return(standardGeneric("plotCutoff"))
})

setGeneric("plotPowerFit", function(altObject, nullObject, ...) {
    return(standardGeneric("plotPowerFit"))
})

setGeneric("getPowerFit", function(altObject, cutoff, revDirec = FALSE, usedFit = NULL) {
    return(standardGeneric("getPowerFit"))
})

setGeneric("vectorizeObject", function(object, labels, ...) {
    return(standardGeneric("vectorizeObject"))
})

setGeneric("summaryParam", function(object, ...) {
    return(standardGeneric("summaryParam"))
})

setGeneric("subtractObject", function(object1, object2, ...) {
    return(standardGeneric("subtractObject"))
})

setGeneric("standardize", function(object) {
    return(standardGeneric("standardize"))
})

setGeneric("extract", function(object, ...) {
    return(standardGeneric("extract"))
})

setGeneric("plotDist", function(object, ...) {
    return(standardGeneric("plotDist"))
})

setGeneric("skew", function(object, ...) {
    return(standardGeneric("skew"))
})

setGeneric("kurtosis", function(object, ...) {
    return(standardGeneric("kurtosis"))
})

setGeneric("simData", function(param, ...) {
    return(standardGeneric("simData"))
})

setGeneric("runFit", function(model, realdata, nRep = 1000, misspec = new("NullSimMisspec"), maxDraw = 100, sequential = NA, facDist = new("NullSimDataDist"), errorDist = new("NullSimDataDist"), 
    indDist = new("NullSimDataDist"), modelBoot = FALSE, seed = 123321, silent = FALSE, multicore = FALSE, cluster = FALSE, numProc = NULL, empiricalMissing = TRUE, missModel = new("NullSimMissing"), 
    usedStd = TRUE) {
    return(standardGeneric("runFit"))
})

setGeneric("pValue", function(target, dist, ...) {
    return(standardGeneric("pValue"))
})

setGeneric("summaryPopulation", function(object) {
    return(standardGeneric("summaryPopulation"))
})

setGeneric("getPopulation", function(object, ...) {
    return(standardGeneric("getPopulation"))
})

setGeneric("setPopulation", function(target, population, ...) {
    return(standardGeneric("setPopulation"))
})

setGeneric("popMisfit", function(param, misspec, dfParam = NULL, fit.measures = "all", ...) {
    return(standardGeneric("popMisfit"))
})

setGeneric("toSimSet", function(out, ...) {
    return(standardGeneric("toSimSet"))
})

setGeneric("isRandom", function(object) {
    return(standardGeneric("isRandom"))
})

setGeneric("toFunction", function(x) {
    return(standardGeneric("toFunction"))
}) 

