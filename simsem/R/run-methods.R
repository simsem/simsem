# run: Run a particular object in simsem package.

# Distribution object: draw a random sample from a distribution

setMethod("run", signature(object = "SimNorm"), function(object, n = 1) {
    rnorm(n, object@mean, object@sd)
})

setMethod("run", signature(object = "SimUnif"), function(object, n = 1) {
    runif(n, object@min, object@max)
})

setMethod("run", signature(object = "SimBeta"), function(object, n = 1) {
    rbeta(n, object@shape1, object@shape2, object@ncp)
})

setMethod("run", signature(object = "SimBinom"), function(object, n = 1) {
    rbinom(n, object@size, object@prob)
})

setMethod("run", signature(object = "SimCauchy"), function(object, n = 1) {
    rcauchy(n, object@location, object@scale)
})

setMethod("run", signature(object = "SimChisq"), function(object, n = 1) {
    rchisq(n, object@df, object@ncp)
})

setMethod("run", signature(object = "SimExp"), function(object, n = 1) {
    rexp(n, object@rate)
})

setMethod("run", signature(object = "SimF"), function(object, n = 1) {
    rf(n, object@df1, object@df2, object@ncp)
})

setMethod("run", signature(object = "SimGamma"), function(object, n = 1) {
    rgamma(n, object@shape, object@rate)
})

setMethod("run", signature(object = "SimGeom"), function(object, n = 1) {
    rgeom(n, object@prob)
})

setMethod("run", signature(object = "SimHyper"), function(object, n = 1) {
    rhyper(n, object@m, object@n, object@k)
})

setMethod("run", signature(object = "SimLnorm"), function(object, n = 1) {
    rlnorm(n, object@meanlog, object@sdlog)
})

setMethod("run", signature(object = "SimLogis"), function(object, n = 1) {
    rlogis(n, object@location, object@scale)
})

setMethod("run", signature(object = "SimNbinom"), function(object, n = 1) {
    rnbinom(n, object@size, object@prob)
})

setMethod("run", signature(object = "SimPois"), function(object, n = 1) {
    rpois(n, object@lambda)
})

setMethod("run", signature(object = "SimT"), function(object, n = 1) {
    rt(n, object@df, object@ncp)
})

setMethod("run", signature(object = "SimWeibull"), function(object, n = 1) {
    rweibull(n, object@shape, object@scale)
})

###############################################################################

setMethod("run", signature(object = "SimMatrix"), function(object) {
    Matrix <- object@free
    for (i in 1:nrow(Matrix)) {
        for (j in 1:ncol(Matrix)) {
            if (is.na(Matrix[i, j]) & !is.nan(Matrix[i, j])) {
                Matrix[i, j] <- eval(parse(text = object@value[i, j]))
            }
        }
    }
    return(Matrix)
})

setMethod("run", signature = "SymMatrix", definition = function(object) {
    if (isNullObject(object)) 
        return(new("NullMatrix"))
    Matrix <- object@free
    for (i in 1:nrow(Matrix)) {
        for (j in 1:i) {
            if (is.na(Matrix[i, j]) & !is.nan(Matrix[i, j])) {
                Matrix[i, j] <- eval(parse(text = object@value[i, j]))
                Matrix[j, i] <- Matrix[i, j]
            }
        }
    }
    return(Matrix)
})

setMethod("run", signature = "SimVector", definition = function(object) {
    if (isNullObject(object)) 
        return(new("NullVector"))
    Vector <- object@free
    for (i in 1:length(Vector)) {
        if (is.na(Vector[i]) & !is.nan(Vector[i])) {
            Vector[i] <- eval(parse(text = object@value[i]))
        }
    }
    return(Vector)
})

setMethod("run", signature(object = "NullSimMatrix"), function(object) {
    return(new("NullMatrix"))
})

setMethod("run", signature = "NullSymMatrix", definition = function(object) {
    return(new("NullMatrix"))
})

setMethod("run", signature = "NullSimVector", definition = function(object) {
    return(new("NullVector"))
})

setMethod("run", signature(object = "SimSet"), definition = function(object, equalCon = new("NullSimEqualCon"), makeList = FALSE) {
    param <- new("MatrixSet", modelType = object@modelType, LY = run(object@LY), VTE = run(object@VTE), TE = run(object@TE), RTE = run(object@RTE), 
        VY = run(object@VY), TY = run(object@TY), MY = run(object@MY), BE = run(object@BE), VPS = run(object@VPS), PS = run(object@PS), 
        RPS = run(object@RPS), VE = run(object@VE), AL = run(object@AL), ME = run(object@ME), LX = run(object@LX), VTD = run(object@VTD), 
        TD = run(object@TD), RTD = run(object@RTD), VX = run(object@VX), TX = run(object@TX), MX = run(object@MX), GA = run(object@GA), 
        VPH = run(object@VPH), PH = run(object@PH), RPH = run(object@RPH), KA = run(object@KA), TH = run(object@TH), RTH = run(object@RTH))
    out <- NULL
    if (!isNullObject(equalCon)) {
        if (object@modelType != equalCon@modelType) 
            stop("Please provide same tags of SimSet and constraint")
        if (equalCon@conBeforeFill) {
            param <- constrainMatrices(param, equalCon)
            out <- fillParam(param, object@modelType)
        } else {
            param <- fillParam(param, object@modelType)
            param <- constrainMatrices(param, equalCon)
            out <- fillParam(param, object@modelType)
        }
    } else {
        out <- fillParam(param, object@modelType)
    }
    if (makeList) {
        return(list(out, param))
    } else {
        return(out)
    }
})

setMethod("run", signature(object = "SimMisspec"), definition = function(object) {
    misspec <- new("MisspecSet", modelType = object@modelType, LY = run(object@LY), VTE = run(object@VTE), TE = run(object@TE), RTE = run(object@RTE), 
        VY = run(object@VY), TY = run(object@TY), MY = run(object@MY), BE = run(object@BE), VPS = run(object@VPS), PS = run(object@PS), 
        RPS = run(object@RPS), VE = run(object@VE), AL = run(object@AL), ME = run(object@ME), LX = run(object@LX), VTD = run(object@VTD), 
        TD = run(object@TD), RTD = run(object@RTD), VX = run(object@VX), TX = run(object@TX), MX = run(object@MX), GA = run(object@GA), 
        VPH = run(object@VPH), PH = run(object@PH), RPH = run(object@RPH), KA = run(object@KA), TH = run(object@TH), RTH = run(object@RTH))
    return(misspec)
})

setMethod("run", signature = "SimData", definition = function(object, n = NULL, dataOnly = TRUE) {
    if (!require(MASS)) 
        stop("Please install MASS package")
    if (is.null(n)) 
        n <- object@n
    paramSet <- drawParameters(object)
    DataOut <- createData(paramSet, n, object, dataOnly)
    if (!isNullObject(object@indLab)) {
        if (class(DataOut) == "SimDataOut") {
            data <- DataOut@data
            colnames(data) <- object@indLab
            DataOut@data <- data
        } else {
            colnames(DataOut) <- object@indLab
        }
    }
    return(DataOut)
    
})

setMethod("run", signature = "SimModel", definition = function(object, data, simMissing = new("NullSimMissing"), estimator = NULL) {
    Output <- NULL
    DataOut <- NULL
    if (class(data) == "SimDataOut") {
        DataOut <- data
        data <- DataOut@data
    }
    if (is.null(colnames(data))) 
        colnames(data) <- paste("y", 1:ncol(data))
    if (isNullObject(object@auxiliary)) {
        if (!isNullObject(simMissing) && !(length(simMissing@cov) == 1 && simMissing@cov == 0) && simMissing@covAsAux) 
            object@auxiliary <- simMissing@cov
    }
    if (isNullObject(object@indLab)) {
        if (isNullObject(object@auxiliary)) {
            object@indLab <- colnames(data)
        } else if (is.numeric(object@auxiliary)) {
            if (max(object@auxiliary) > ncol(data)) 
                stop("The maximum index in the auxiliary variable set is greater than the number of variables in the data.")
            object@indLab <- colnames(data)[-object@auxiliary]
        } else {
            if (length(intersect(colnames(data), object@auxiliary)) != length(object@auxiliary)) 
                stop("Some auxiliary variables does not exist in the dataset.")
            object@indLab <- setdiff(colnames(data), object@auxiliary)
        }
    }
    if (is.numeric(object@indLab)) 
        object@indLab <- colnames(data)[object@indLab]
    if (is.numeric(object@auxiliary)) 
        object@auxiliary <- colnames(data)[object@auxiliary]
    if (length(intersect(object@auxiliary, object@indLab)) != 0) 
        stop("There is common variable between the variables in the model and the auxiliary variables.")
    targetCol <- c(object@indLab, object@auxiliary)
    data <- data[, targetCol]
    miss <- sum(is.na(data)) > 0
    if (is.null(estimator)) 
        estimator <- object@estimator
    estimator <- tolower(estimator)
    if (miss && !isNullObject(simMissing) && simMissing@numImps > 0) {
        Output <- runMI(data, object, simMissing@numImps, simMissing@impMethod, opts = simMissing@opts)
    } else {
        if (object@package == "OpenMx") {
            Output <- runOpenMx(object, data)
        } else if (object@package == "lavaan") {
            if (miss) {
                Output <- runLavaan(object, data, miss = "fiml", estimator = estimator)
            } else {
                Output <- runLavaan(object, data, miss = "listwise", estimator = estimator)
            }
        }
    }
    # is.equal(DataOut@param, Output@param) yes --> compute bias
    if (!is.null(DataOut)) {
        param <- DataOut@param
        check <- isTRUE(all.equal(param, Output@param))
        usedX <- NULL
        usedY <- NULL
        if (!(length(check) == 1 && check == TRUE) & !isNullObject(object@auxiliary)) {
            usedY <- which(!(colnames(data) %in% object@auxiliary))
            nx <- 0
            if (object@modelType == "SEM.exo") 
                nx <- nrow(object@param@LX)
            if (object@modelType == "Path.exo") 
                nx <- nrow(object@param@PH)
            if (nx > 0) 
                usedX <- intersect(1:nx, usedY)
            usedY <- setdiff(usedY, usedX)
            param <- extract(param, y = usedY, x = usedX)
        }
        check <- isTRUE(all.equal(param, Output@param))
        if (length(check) == 1 && check == TRUE) {
            paramOut <- DataOut@paramOut
            if (!isNullObject(object@auxiliary)) 
                paramOut <- extract(paramOut, y = usedY, x = usedX)
            Output@paramValue <- paramOut
        }
    }
    Output@n <- nrow(data)
	pMiss <- apply(is.na(data), 2, mean)
	names(pMiss) <- NULL
	Output@pMiss <- pMiss
    if (!isNullObject(object@indLab)) {
        Output@indLab <- object@indLab
    } else {
        Output@indLab <- colnames(data)
    }
    Output@factorLab <- object@factorLab
    # Add labels in the SimModelOut --> go to SimModelOut and relabels it
    
    # Provide a nicer summary --> Groups elements from the same matrix together
    return(Output)
})

setMethod("run", signature = "SimMissing", definition = function(object, data, pmMCAR = NULL, pmMAR = NULL) {
    if (!is.null(pmMCAR)) 
        object@pmMCAR <- pmMCAR
    if (!is.null(pmMAR)) 
        object@pmMAR <- pmMAR
    if (is(data, "SimDataOut")) {
        data@data <- as.data.frame(imposeMissing(data@data, cov = object@cov, pmMCAR = object@pmMCAR, pmMAR = object@pmMAR, nforms = object@nforms, 
            timePoints = object@timePoints, itemGroups = object@itemGroups, twoMethod = object@twoMethod, prAttr = object@prAttr, ignoreCols = object@ignoreCols, 
            threshold = object@threshold, logical = object@logical))
    } else if (is.data.frame(data)) {
        data <- as.data.frame(imposeMissing(data, cov = object@cov, pmMCAR = object@pmMCAR, pmMAR = object@pmMAR, nforms = object@nforms, 
            timePoints = object@timePoints, itemGroups = object@itemGroups, twoMethod = object@twoMethod, prAttr = object@prAttr, ignoreCols = object@ignoreCols, 
            threshold = object@threshold, logical = object@logical))
    } else if (is.matrix(data)) {
        data <- as.data.frame(data)
        data <- as.data.frame(imposeMissing(data, cov = object@cov, pmMCAR = object@pmMCAR, pmMAR = object@pmMAR, nforms = object@nforms, 
            timePoints = object@timePoints, itemGroups = object@itemGroups, twoMethod = object@twoMethod, prAttr = object@prAttr, ignoreCols = object@ignoreCols, 
            threshold = object@threshold, logical = object@logical))
    }
    return(data)
})

setMethod("run", signature = "SimDataDist", definition = function(object, n, m, cm) {
    library(MASS)
    Data <- NULL
    # Check dim(M) dim(CM) dim(copula) are equal
    if (isNullObject(object)) {
        Data <- mvrnorm(n, m, cm)
    } else {
        library(copula)
        if (object@p > 1) {
            varNotZeros <- diag(cm) != 0
            object2 <- object
            cm2 <- cm
            if (sum(varNotZeros) < object@p) {
                object2 <- extract(object, which(varNotZeros))
                cm2 <- extract(cm, which(varNotZeros), which(varNotZeros))
            }
            r <- cov2cor(as.matrix(cm2))
            for (i in 1:object2@p) {
                if (object2@reverse[i] == TRUE) {
                  r[i, ] <- -1 * r[i, ]
                  r[, i] <- -1 * r[, i]
                }
            }
            listR <- r[lower.tri(diag(object2@p))]
            CopNorm <- ellipCopula(family = "normal", dim = object2@p, dispstr = "un", param = listR)
            distName <- sapply(object2@dist, class)
            distName <- tolower(gsub("Sim", "", distName))
            attribute <- list()
            for (i in 1:length(object2@dist)) {
                temp <- list()
                indivAttr <- slotNames(object2@dist[[i]])
                for (j in 1:length(indivAttr)) {
                  temp[[j]] <- call("=", indivAttr[[j]], slot(object2@dist[[i]], indivAttr[[j]]))
                }
                attribute[[i]] <- temp
            }
            Mvdc <- mvdc(CopNorm, distName, attribute)
            Data <- rmvdc(Mvdc, n)
            if (sum(varNotZeros) < object@p) {
                varZeros <- diag(cm) == 0
                constant <- matrix(0, n, sum(varZeros))
                Data <- data.frame(Data, constant)
                Data[, c(which(varNotZeros), which(varZeros))] <- Data
            }
        } else if (object@p == 1) {
            if (as.matrix(cm)[1, 1] == 0) {
                Data <- rep(m[1], n)
            } else {
                Data <- as.matrix(run(object@dist[[1]], n = n))
            }
        } else {
            stop("Error in the run-SimDataDist.")
        }
        for (i in 1:object@p) {
            if (object@reverse[i] == TRUE) {
                meanOld <- mean(Data[, i])
                anchor <- max(Data[, i])
                datNew <- anchor - Data[, i]
                Data[, i] <- datNew - mean(datNew) + meanOld
            }
        }
        if (!is.matrix(Data)) 
            Data <- as.matrix(Data)
        Data <- scale(Data)
        obtainedMean <- attr(Data, "scaled:center")
        obtainedSD <- attr(Data, "scaled:scale")
        Data[is.na(Data)] <- 0
        fakeDat <- mvrnorm(n, m, cm)
        fakeMean <- apply(fakeDat, 2, mean)
        fakeSD <- apply(fakeDat, 2, sd)
        obtainedMean[object@keepScale] <- fakeMean[object@keepScale]
        obtainedSD[object@keepScale] <- fakeSD[object@keepScale]
        Data <- t(apply(Data, 1, function(y, m, s) {
            y * s + m
        }, m = obtainedMean, s = obtainedSD))
        if (nrow(Data) == 1) 
            Data <- t(Data)
    }
    return(Data)
})

setMethod("run", signature(object = "SimFunction"), definition = function(object, x, checkDataOut = FALSE) {
    if (checkDataOut && (class(x) == "SimDataOut")) 
        x <- x@data
    out <- list()
    out[[1]] <- object@fun
    out[[2]] <- x
    outlength <- length(object@attribute)
    for (i in 1:outlength) {
        out[[i + 2]] <- object@attribute[[i]]
    }
    names(out) <- c("", "", names(object@attribute))
    eval(as.call(out))
}) 
