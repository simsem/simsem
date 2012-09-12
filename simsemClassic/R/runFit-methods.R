# runFit: Build a result object from analyzing real data

setMethod("runFit", signature(model = "SimModel"), definition = function(model, data, nRep = 1000, misspec = new("NullSimMisspec"), 
    maxDraw = 100, sequential = NA, facDist = new("NullSimDataDist"), errorDist = new("NullSimDataDist"), indDist = new("NullSimDataDist"), 
    modelBoot = FALSE, seed = 123321, silent = FALSE, multicore = FALSE, cluster = FALSE, numProc = NULL, empiricalMissing = TRUE, missModel = new("NullSimMissing"), 
    usedStd = TRUE, analyzeModel=NULL) {
    out <- run(model, data)
    if (empiricalMissing) {
        miss <- new("NullMatrix")
        if (isNullObject(model@indLab)) {
            miss <- is.na(data)
        } else {
            miss <- is.na(data[, model@indLab])
        }
        if (isNullObject(missModel)) {
            missModel <- simMissing(logical = miss)
        } else {
            missModel <- simMissing(numImps = missModel@numImps, logical = miss)
        }
    }
    SimData <- simData(out, misspec = misspec, maxDraw = maxDraw, sequential = sequential, facDist = facDist, errorDist = errorDist, indDist = indDist, 
        usedStd = usedStd, modelBoot = modelBoot, realData = data)
	if(is.null(analyzeModel)) analyzeModel <- model
    simOut <- simResult(nRep, SimData, analyzeModel, objMissing = missModel, seed = seed, silent = silent, multicore = multicore, cluster = cluster, 
        numProc = numProc)
    return(simOut)
})

setMethod("runFit", signature(model = "SimModelOut"), definition = function(model, data = new("NullDataFrame"), nRep = 1000, 
    misspec = new("NullSimMisspec"), maxDraw = 100, sequential = NA, facDist = new("NullSimDataDist"), errorDist = new("NullSimDataDist"), 
    indDist = new("NullSimDataDist"), modelBoot = FALSE, seed = 123321, silent = FALSE, multicore = FALSE, cluster = FALSE, numProc = NULL, 
    empiricalMissing = TRUE, missModel = new("NullSimMissing"), usedStd = TRUE, analyzeModel=NULL) {
    SimData <- simData(model, misspec = misspec, maxDraw = maxDraw, sequential = sequential, facDist = facDist, errorDist = errorDist, 
        indDist = indDist, usedStd = usedStd, modelBoot = modelBoot, realData = data)
    if (empiricalMissing) {
        miss <- new("NullMatrix")
        if (!isNullObject(data)) {
            if (isNullObject(model@indLab)) {
                miss <- is.na(data)
            } else {
                miss <- is.na(data[, model@indLab])
            }
            if (isNullObject(missModel)) {
                missModel <- simMissing(logical = miss)
            } else {
                missModel <- simMissing(numImps = missModel@numImps, logical = miss)
            }
        }
    }
	if(is.null(analyzeModel)) analyzeModel <- simModel(model@param, equalCon = model@equalCon, indLab = model@indLab)
    simOut <- simResult(nRep, SimData, analyzeModel, objMissing = missModel, seed = seed, silent = silent, multicore = multicore, cluster = cluster, 
        numProc = numProc)
    return(simOut)
}) 
