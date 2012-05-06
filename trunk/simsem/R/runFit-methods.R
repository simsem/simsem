# runFit: Build a result object from analyzing real data

setMethod("runFit", signature(model = "SimModel"), definition = function(model, 
    realdata, nRep = 1000, misspec = new("NullSimMisspec"), maxDraw=100, sequential = NA, facDist = new("NullSimDataDist"), 
    errorDist = new("NullSimDataDist"), indDist = new("NullSimDataDist"), modelBoot = FALSE, 
    seed = 123321, silent = FALSE, multicore = FALSE, cluster = FALSE, numProc = NULL, 
    empiricalMissing = TRUE, missModel = new("NullSimMissing"), usedStd = TRUE) {
    out <- run(model, realdata)
    if (empiricalMissing) {
        miss <- new("NullMatrix")
        if (isNullObject(model@indLab)) {
            miss <- is.na(realdata)
        } else {
            miss <- is.na(realdata[, model@indLab])
        }
        if (isNullObject(missModel)) {
            missModel <- simMissing(logical = miss)
        } else {
            missModel <- simMissing(numImps = missModel@numImps, logical = miss)
        }
    }
    SimData <- simData(out, misspec = misspec, maxDraw=maxDraw, sequential = sequential, facDist = facDist, errorDist = errorDist, 
        indDist = indDist, usedStd = usedStd, realData = realdata)
    simOut <- simResult(nRep, SimData, model, objMissing = missModel, seed = seed, 
        silent = silent, multicore = multicore, cluster = cluster, numProc = numProc)
    return(simOut)
})

setMethod("runFit", signature(model = "SimModelOut"), definition = function(model, 
    realdata = new("NullDataFrame"), nRep = 1000, misspec = new("NullSimMisspec"), maxDraw=100, 
    sequential = NA, 
    facDist = new("NullSimDataDist"), errorDist = new("NullSimDataDist"), indDist = new("NullSimDataDist"), 
    modelBoot = FALSE, seed = 123321, silent = FALSE, multicore = FALSE, cluster = FALSE, 
    numProc = NULL, empiricalMissing = TRUE, missModel = new("NullSimMissing"), usedStd = TRUE) {
    SimData <- simData(model, misspec = misspec, maxDraw=maxDraw, sequential = sequential, facDist = facDist, errorDist = errorDist, 
        indDist = indDist, usedStd = usedStd, modelBoot = modelBoot, realData = realdata)
    if (empiricalMissing) {
        miss <- new("NullMatrix")
        if (!isNullObject(realdata)) {
            if (isNullObject(model@indLab)) {
                miss <- is.na(realdata)
            } else {
                miss <- is.na(realdata[, model@indLab])
            }
            if (isNullObject(missModel)) {
                missModel <- simMissing(logical = miss)
            } else {
                missModel <- simMissing(numImps = missModel@numImps, logical = miss)
            }
        }
    }
    analyzeModel <- simModel(model@param, equalCon = model@equalCon, indLab = model@indLab)
    simOut <- simResult(nRep, SimData, analyzeModel, objMissing = missModel, seed = seed, 
        silent = silent, multicore = multicore, cluster = cluster, numProc = numProc)
    return(simOut)
}) 
