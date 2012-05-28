# popMisfit: The discrepancy value (chi-square value) divided by degree of freedom, which is equal to population RMSEA

setMethod("popMisfit", signature(param = "matrix", misspec = "matrix"), definition = function(param, misspec, dfParam = NULL, fit.measures = "all") {
    p <- nrow(param)
    blankM <- rep(0, p)
    result <- popMisfitMACS(blankM, param, blankM, misspec, dfParam = dfParam, fit.measures = fit.measures)
    return(result)
})

setMethod("popMisfit", signature(param = "list", misspec = "list"), definition = function(param, misspec, dfParam = NULL, fit.measures = "all") {
    paramCM <- NULL
    paramM <- NULL
    misspecCM <- NULL
    misspecM <- NULL
    if (is(param[[1]], "matrix")) {
        paramCM <- param[[1]]
        p <- nrow(paramCM)
        paramM <- rep(0, p)
        if (is(param[[2]], "vector")) {
            paramM <- param[[2]]
        }
    } else if (is(param[[2]], "matrix")) {
        paramCM <- param[[2]]
        if (is(param[[1]], "vector")) {
            paramM <- param[[1]]
        } else {
            stop("Cannot find the mean vector of the parameter values.")
        }
    } else {
        stop("Cannot find covariance matrix in the parameter values")
    }
    if (is(misspec[[1]], "matrix")) {
        misspecCM <- misspec[[1]]
        p <- nrow(misspecCM)
        misspecM <- rep(0, p)
        if (is(misspec[[2]], "vector")) {
            misspecM <- misspec[[2]]
        }
    } else if (is(misspec[[2]], "matrix")) {
        misspecCM <- misspec[[2]]
        if (is(param[[1]], "vector")) {
            misspecM <- misspec[[1]]
        } else {
            stop("Cannot find the mean vector of the misspecification values.")
        }
    } else {
        stop("Cannot find covariance matrix in the misspecification values")
    }
    result <- popMisfitMACS(paramM, paramCM, misspecM, misspecCM, dfParam = dfParam, fit.measures = fit.measures)
    return(result)
})

setMethod("popMisfit", signature(param = "SimRSet", misspec = "SimRSet"), definition = function(param, misspec, dfParam = NULL, fit.measures = "all") {
    paramMacs <- createImpliedMACS(param)
    misspecMacs <- createImpliedMACS(misspec)
    if (!(all(is.finite(misspecMacs$CM)) && all(eigen(misspecMacs$CM)$values > 0))) 
        stop("The misspecification set is not valid.")
    if (!(all(is.finite(paramMacs$CM)) && all(eigen(paramMacs$CM)$values > 0))) 
        stop("The real parameter set is not valid")
    return(popMisfitMACS(paramMacs$M, paramMacs$CM, misspecMacs$M, misspecMacs$CM, dfParam = dfParam, fit.measures = fit.measures))
})

setMethod("popMisfit", signature(param = "MatrixSet", misspec = "MatrixSet"), definition = function(param, misspec, dfParam = NULL, fit.measures = "all") {
    if (!validateObject(param)) 
        stop("The set of actual parameters is not valid.")
    if (!validateObject(misspec)) 
        stop("The set of misspecified paramters is not valid.")
    param <- reduceMatrices(param)
    misspec <- reduceMatrices(misspec)
    return(popMisfit(param, misspec, dfParam = dfParam, fit.measures = fit.measures))
    
})

setMethod("popMisfit", signature(param = "SimSet", misspec = "SimMisspec"), definition = function(param, misspec, dfParam = NULL, fit.measures = "all", equalCon = new("NullSimEqualCon")) {
    Output <- runMisspec(param, misspec, equalCon)
    param2 <- Output$param
    misspec <- Output$misspec
    if (is.null(dfParam)) {
        p <- length(createImpliedMACS(param2)$M)
        nElements <- p + (p * (p + 1)/2)
        nFree <- countFreeParameters(param)
        if (!isNullObject(equalCon)) 
            nFree <- nFree + countFreeParameters(equalCon)
        dfParam <- nElements - nFree
    }
    return(popMisfit(param2, misspec, dfParam = dfParam, fit.measures = fit.measures))
})

popMisfitMACS <- function(paramM, paramCM, misspecM, misspecCM, dfParam = NULL, fit.measures = "all") {
    if (fit.measures == "all") {
        fit.measures <- getKeywords()$usedFitPop
        if (is.null(dfParam)) 
            fit.measures <- fit.measures[c(1, 3)]
    }
    p <- length(paramM)
    fit.measures <- tolower(fit.measures)
    f0 <- popDiscrepancy(paramM, paramCM, misspecM, misspecCM)
    rmsea <- NULL
    srmr <- NULL
    result <- NULL
    if (any(fit.measures %in% "f0")) {
        result <- c(result, f0)
    }
    if (any(fit.measures %in% "rmsea")) {
        rmsea <- sqrt(f0/dfParam)
        result <- c(result, rmsea)
    }
    if (any(fit.measures %in% "srmr")) {
        disSquared <- (cov2cor(misspecCM) - cov2cor(paramCM))^2
        numerator <- 2 * sum(disSquared[lower.tri(disSquared, diag = TRUE)])
        srmr <- sqrt(numerator/(p * (p + 1)))
        result <- c(result, srmr)
    }
    result <- as.vector(result)
    names(result) <- fit.measures
    return(result)
}

# F0 in population: The discrepancy due to approximation (Browne & Cudeck, 1992)
popDiscrepancy <- function(paramM, paramCM, misspecM, misspecCM) {
    p <- length(misspecM)
    inv <- solve(paramCM)
    dis.CM <- misspecCM %*% inv
    t.1 <- sum(diag(dis.CM))
    t.1.1 <- det(dis.CM)
    if (t.1.1 < 0) 
        return(NULL)
    t.2 <- log(t.1.1)
    dis.M <- as.matrix(misspecM - paramM)
    t.3 <- t(dis.M) %*% inv %*% dis.M
    discrepancy <- t.1 - t.2 - p + t.3
    return(discrepancy)
} 
