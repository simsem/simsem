# runMisspec: Run parameters from SimSet object with the parameters from SimMisspec to be put on top of it. The final parameters will be with and without model misspecification.

runMisspec <- function(object, misspec, SimEqualCon = new("NullSimEqualCon")) {
    Output1 <- NULL
    Output2 <- NULL
    # Should create the misspec here: then compare with the MACS and see what is going on!
    if (misspec@optMisfit == "none") {
        Mis <- list(run(misspec))
    } else {
        Mis <- lapply(1:misspec@numIter, function(obj, m) run(m), m = misspec)
    }
    if (isNullObject(SimEqualCon)) {
        if (misspec@misBeforeFill) {
            paramSet <- run(object, makeList = TRUE)
            Output1 <- paramSet[[1]]
            param <- lapply(Mis, combineObject, object1 = paramSet[[2]])
            Output2 <- lapply(param, fillParam, modelType = object@modelType)
        } else {
            paramSet <- run(object, makeList = TRUE)
            Output1 <- paramSet[[1]]
            param <- lapply(Mis, combineObject, object1 = paramSet[[1]])
            Output2 <- lapply(param, fillParam, modelType = object@modelType)
        }
    } else {
        if (object@modelType != SimEqualCon@modelType) 
            stop("Please provide same tags of SimSet and constraint")
        if (misspec@misBeforeFill & misspec@conBeforeMis & SimEqualCon@conBeforeFill) {
            # 2) Con, AddMis, fill
            paramSet <- run(object, SimEqualCon, makeList = TRUE)
            Output1 <- paramSet[[1]]
            param <- lapply(Mis, combineObject, object1 = paramSet[[2]])
            Output2 <- lapply(param, fillParam, modelType = object@modelType)
        } else if (!misspec@misBeforeFill & misspec@conBeforeMis & SimEqualCon@conBeforeFill) {
            # 1) Con, fillBefore, AddMis
            paramSet <- run(object, SimEqualCon, makeList = TRUE)
            Output1 <- paramSet[[1]]
            param <- lapply(Mis, combineObject, object1 = paramSet[[1]])
            Output2 <- lapply(param, fillParam, modelType = object@modelType)
        } else if (!misspec@misBeforeFill & misspec@conBeforeMis & !SimEqualCon@conBeforeFill) {
            # 3) fillBefore, Con, AddMis misspec@misBeforeFill=F & misspec@conBeforeMis=T SimEqualCon@conBeforeFill=F; FTF
            paramSet <- run(object, SimEqualCon, makeList = TRUE)
            Output1 <- paramSet[[1]]
            param <- lapply(Mis, combineObject, object1 = paramSet[[2]])
            Output2 <- lapply(param, fillParam, modelType = object@modelType)
        } else if (!misspec@misBeforeFill & !misspec@conBeforeMis & !SimEqualCon@conBeforeFill) {
            # 4) fillBefore, AddMis, Con isBeforeFill=F & misspec@conBeforeMis=F SimEqualCon@conBeforeFill=F; FFF
            paramSet <- run(object, makeList = TRUE)
            Output1 <- paramSet[[1]]
            param <- lapply(Mis, combineObject, object1 = paramSet[[1]])
            param <- lapply(param, constrainMatrices, SimEqualCon = SimEqualCon)
            Output2 <- lapply(param, fillParam, modelType = object@modelType)
        } else if (misspec@misBeforeFill & !misspec@conBeforeMis & SimEqualCon@conBeforeFill) {
            # 5) AddMis, Con, fill misspec@misBeforeFill=T & misspec@conBeforeMis=F SimEqualCon@conBeforeFill=T; TFT
            paramSet <- run(object, makeList = TRUE)
            Output1 <- paramSet[[1]]
            param <- lapply(Mis, combineObject, object1 = paramSet[[2]])
            param <- lapply(param, constrainMatrices, SimEqualCon = SimEqualCon)
            Output2 <- lapply(param, fillParam, modelType = object@modelType)
        } else if (misspec@misBeforeFill & !misspec@conBeforeMis & !SimEqualCon@conBeforeFill) {
            # 6) AddMis, fill, Con misspec@misBeforeFill=T & misspec@conBeforeMis=F SimEqualCon@conBeforeFill=F; TFF ; No FFT and TTF
            paramSet <- run(object, makeList = TRUE)
            Output1 <- paramSet[[1]]
            param <- lapply(Mis, combineObject, object1 = paramSet[[2]])
            param <- lapply(param, fillParam, modelType = object@modelType)
            param <- lapply(param, constrainMatrices, SimEqualCon = SimEqualCon)
            Output2 <- lapply(param, fillParam, modelType = object@modelType)
        } else {
            stop("The specifications of 'misBeforeFill' and 'conBeforeMis' in the SimMisspec object and the 'conBeforeFill' in the SimEqualCon are not consistent. Change one of those specifications.")
        }
    }
    if (length(Output2) > 1) {
        macsMis <- lapply(Output2, createImpliedMACS)
        macsPop <- createImpliedMACS(Output1)
        p <- length(macsPop$M)
        nElements <- p + (p * (p + 1)/2)
        nFree <- countFreeParameters(object)
        if (!isNullObject(SimEqualCon)) 
            nFree <- nFree + countFreeParameters(SimEqualCon)
        dfParam <- nElements - nFree
        misfit <- sapply(macsMis, popMisfit, param = macsPop, dfParam = dfParam, fit.measures = misspec@misfitType)
        element <- NULL
        if (misspec@optMisfit == "min") {
            element <- which(misfit == min(misfit))
        } else if (misspec@optMisfit == "max") {
            element <- which(misfit == max(misfit))
        } else {
            stop("Something is wrong in the runMisspec function!")
        }
        Output2 <- Output2[[element]]
        Mis <- Mis[[element]]
    } else {
        Output2 <- Output2[[1]]
        Mis <- Mis[[1]]
    }
    return(list(param = Output1, misspec = Output2, misspecAdd = Mis))
} 
