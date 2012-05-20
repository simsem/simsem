# simResultParam: Simulate the parameters for all replications.

simResultParam <- function(nRep, object, misspec = new("NullSimMisspec"), SimEqualCon = new("NullSimEqualCon"), seed = 123321, maxDraw = 100) {
    result.l <- NULL
    nFree <- countFreeParameters(object)
    if (!isNullObject(SimEqualCon)) 
        nFree <- nFree + countFreeParameters(SimEqualCon)
    nTotal <- countMACS(object)
    dfParam <- nTotal - nFree
    set.seed(seed)
    numMisspec <- countFreeParameters(misspec)
    for (i in 1:nRep) {
        result.l[[i]] <- drawParametersMisspec(object, misspec, objEqualCon = SimEqualCon, maxDraw = maxDraw)
    }
    param.l <- sapply(result.l, function(object, paramTemplate) {
        result <- object$real
        free <- createFreeParameters(paramTemplate)
        lab <- makeLabels(free, "OpenMx")
        result <- vectorizeObject(result, lab)
    }, paramTemplate = object)
    misspec.l <- sapply(result.l, function(object, misspecTemplate) {
        result <- object$misspecAdd
        lab <- makeLabels(misspecTemplate, "OpenMx")
        result <- vectorizeObject(result, lab)
    }, misspecTemplate = misspec)
    fit.l <- sapply(result.l, function(object, df) {
        popMisfit(object$real, object$misspec, dfParam = df)
    }, df = dfParam)
    paramResult <- as.data.frame(t(param.l))
    misspecResult <- as.data.frame(t(misspec.l))
    fitResult <- as.data.frame(t(fit.l))
    return(new("SimResultParam", modelType = object@modelType, nRep = nRep, param = paramResult, misspec = misspecResult, fit = fitResult, seed = seed))
} 
