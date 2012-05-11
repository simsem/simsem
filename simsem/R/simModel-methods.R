# simModel: Create simModel from model specification

setMethod("simModel", signature(object = "SimParam"), definition = function(object, 
    start = NULL, equalCon = new("NullSimEqualCon"), package = "lavaan", estimator = "ML", 
    auxiliary = new("NullVector"), indLab = new("NullVector"), factorLab = new("NullVector")) {
    modelType <- object@modelType
    if (length(intersect(indLab, auxiliary)) != 0) 
        stop("There are the same variables in the analysis model and in the auxiliary variables list")
    if (!is.null(start)) {
        if (modelType != start@modelType) 
            stop("Starting Values and Parameters do not have the same tag")
    } else {
        start <- defaultStartingValues(object)
    }
    if (!isNullObject(equalCon)) {
        if (modelType != equalCon@modelType) 
            stop("SimEqualCon and SimParam do not have the same tag")
    }
    estimator <- tolower(estimator)
    return(new("SimModel", modelType = modelType, param = object, start = start, 
        equalCon = equalCon, package = package, estimator = estimator, auxiliary = auxiliary, 
        indLab = indLab, factorLab = factorLab))
})

setMethod("simModel", signature(object = "SimSet"), definition = function(object, 
    equalCon = new("NullSimEqualCon"), package = "lavaan", trial = 10, estimator = "ML", 
    auxiliary = new("NullVector"), indLab = new("NullVector"), factorLab = new("NullVector")) {
    if (length(intersect(indLab, auxiliary)) != 0) 
        stop("There are the same variables in the analysis model and in the auxiliary variables list")
    start <- startingValues(object, trial)
    start <- reduceMatrices(start)
    freeParameters <- createFreeParameters(object)
    modelType <- object@modelType
    if (!isNullObject(equalCon)) {
        if (modelType != equalCon@modelType) 
            stop("SimEqualCon and SimSet do not have the same tag")
    }
    estimator <- tolower(estimator)
    return(new("SimModel", modelType = modelType, param = freeParameters, start = start, 
        equalCon = equalCon, package = package, estimator = estimator, auxiliary = auxiliary, 
        indLab = indLab, factorLab = factorLab))
})

setMethod("simModel", signature(object = "SimModelOut"), definition = function(object, 
    start = NULL, equalCon = new("NullSimEqualCon"), package = "lavaan", estimator = "ML", 
    auxiliary = new("NullVector"), indLab = new("NullVector"), factorLab = new("NullVector")) {
    param <- object@param
    modelType <- param@modelType
    if (is.null(start)) 
        start <- object@coef
    if (isNullObject(equalCon)) 
        equalCon <- object@equalCon
    if (isNullObject(indLab)) 
        indLab <- object@indLab
    if (isNullObject(factorLab)) 
        factorLab <- object@factorLab
    return(simModel(object = param, start = start, equalCon = equalCon, package = package, 
        estimator = estimator, auxiliary = auxiliary, indLab = indLab, factorLab = factorLab))
}) 
