# runRep: Run one replication for a simulation study using simResult function

runRep <- function(object, objData, objModel, objMissing = new("NullSimMissing"), objFunction = new("NullSimFunction"), silent = FALSE) {
    modelType <- objModel@modelType
    param <- NULL
    coef <- NA
    se <- NA
    fit <- NA
    std <- NA
    FMI1 <- NULL
    FMI2 <- NULL
    converged <- FALSE
	library(parallel)
    RNGkind("L'Ecuyer-CMRG")
	assign(".Random.seed", object[[5]], envir = .GlobalEnv)
    n <- object[[2]]
    if (is.null(n)) {
		if(is(objData, "SimData")) {
			n <- objData@n
		} 
	}
    pmMCAR <- object[[3]]
    pmMAR <- object[[4]]
    if (isNullObject(objMissing)) {
        if (!is.null(pmMAR) | !is.null(pmMCAR)) {
            if (is.null(pmMCAR)) 
                pmMCAR <- 0
            if (is.null(pmMAR)) 
                pmMAR <- 0
            objMissing <- simMissing(pmMCAR = pmMCAR, pmMAR = pmMAR)
        }
    }
    obj <- object[[1]]
    data.mis <- NULL
    if (!is.null(objData) && class(obj) == "list") {
        data.mis <- createData(obj, n, objData, dataOnly = FALSE)
        if (!isNullObject(objData@indLab)) 
            colnames(data.mis@data) <- objData@indLab
    } else {
        data.mis <- obj
    }
    # if(class(dataT) == 'SimDataOut') { data.mis <-dataT@data } else { data.mis <- dataT }
    
    if (!is(objMissing, "NullSimMissing")) {
        data.mis <- run(objMissing, data.mis, pmMCAR = pmMCAR, pmMAR = pmMAR)
        # data.mis <- imposeMissing(data.mis, covs=objMissing@covs, pmMCAR=objMissing@pmMCAR, pmMAR=objMissing@pmMAR,
        # nforms=objMissing@nforms, itemGroups=objMissing@itemGroups, twoMethod=objMissing@twoMethod)
    }
    if (!isNullObject(objFunction)) 
        data.mis <- run(objFunction, data.mis, checkDataOut = TRUE)
    
    temp <- NULL
    # Impute missing and run results
    if (!is(objMissing, "NullSimMissing") && objMissing@numImps > 0) {
        if (silent) {
            invisible(capture.output(suppressMessages(try(temp <- run(object = objModel, data = data.mis, simMissing = objMissing), silent = TRUE))))
            # invisible(capture.output(suppressMessages(try(temp <- runMI(data.mis,objModel,objMissing@numImps,objMissing@impMethod),
            # silent=TRUE))))
        } else {
            try(temp <- run(object = objModel, data = data.mis, simMissing = objMissing), silent = TRUE)
            # try(temp <- runMI(data.mis,objModel,objMissing@numImps,objMissing@impMethod))
        }
    } else {
        if (silent) {
            invisible(capture.output(suppressMessages(try(temp <- run(object = objModel, data = data.mis), silent = TRUE))))
            # tryCatch(temp <- run(objModel, data), error=function(e) {print('Error')})
        } else {
            try(temp <- run(object = objModel, data = data.mis))
        }
    }
    
    if (!is.null(temp)) {
        converged <- temp@converged
        param <- NA
        Labels <- makeLabels(temp@param, "OpenMx")  #As a quick default to use OpenMx
        if (converged) {
            coef <- vectorizeObject(temp@coef, Labels)
            se <- vectorizeObject(temp@se, Labels)
            fit <- temp@fit
            stdSet <- standardize(temp)
            std <- vectorizeObject(stdSet, Labels)
            if (is(temp, "SimModelMIOut")) {
                # Can we make vectorize object work with simModelOutMI too?
                FMI1 <- vectorizeObject(temp@FMI1, Labels)
                FMI2 <- vectorizeObject(temp@FMI2, Labels)
            }
            if (!isNullObject(temp@paramValue)) {
                param <- vectorizeObject(temp@paramValue, Labels)
            } else {
                param <- NA
            }
        }
    } else {
        if (!is.null(data.mis) && is(data.mis, "SimDataOut")) 
            param <- NA
    }
	paramData <- NULL
	if(!is.null(objData)) {
		LabelsDataParam <- makeLabels(createFreeParameters(objData@param), "OpenMx")
		paramData <- vectorizeObject(object[[1]]$real, LabelsDataParam)
	}
    Result <- list(coef = coef, se = se, fit = fit, converged = converged, param = param, FMI1 = FMI1, FMI2 = FMI2, std = std, paramData = paramData)
    return <- Result
}
 
