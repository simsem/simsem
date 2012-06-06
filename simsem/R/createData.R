# createData: Create data from parameters

createData <- function(paramSet, n, object, dataOnly) {
    library(MASS)
    Data <- NULL
    modelType <- object@modelType
    param <- paramSet[["real"]]
    misspec <- paramSet[["misspec"]]
    usedParam <- NULL
    if (!isNullObject(object@misspec)) {
        usedParam <- misspec
    } else {
        usedParam <- param
    }
    Data <- NULL
    if (object@modelBoot) {
        library(lavaan)
        originalData <- object@realData
        if (!isNullObject(object@indLab)) 
            originalData <- originalData[, object@indLab]
        implied <- createImpliedMACS(usedParam)
        S <- cov(originalData)
        Sigma <- implied$CM
        M <- colMeans(originalData)
        M <- matrix(rep(M, n), nrow = n, byrow = TRUE)
        Mu <- implied$M
        Mu <- matrix(rep(Mu, n), nrow = n, byrow = TRUE)
        z <- (scale(originalData, scale = FALSE) %*% (solve(sqrtSymmetricMatrix(S)) %*% sqrtSymmetricMatrix(Sigma))) + Mu
        index <- sample(1:n, replace = TRUE)
        Data <- z[index, ]
    } else {
        if (object@sequential) {
            if (modelType == "CFA") {
                fac <- run(object@facDist, n, usedParam@AL, usedParam@PS)
                trueScore <- fac %*% t(usedParam@LY)
                errorScore <- run(object@errorDist, n, usedParam@TY, usedParam@TE)
                Data <- trueScore + errorScore
            } else {
                usedParam2 <- NULL
                if (modelType == "Path.exo" | modelType == "SEM.exo") {
                  usedParam2 <- collapseExo(usedParam)
                } else if (modelType == "Path" | modelType == "SEM") {
                  usedParam2 <- usedParam
                } else {
                  stop("Incorrect model type")
                }
                set <- findRecursiveSet(usedParam2@BE)
                iv <- set[[1]]
                fac <- run(extract(object@facDist, iv), n, usedParam2@AL[iv], usedParam2@PS[iv, iv])
                for (i in 2:length(set)) {
                  dv <- set[[i]]
                  pred <- fac %*% t(extract(usedParam2@BE, dv, iv))
                  res <- run(extract(object@facDist, dv), n, usedParam2@AL[dv], usedParam2@PS[dv, dv])
                  new <- pred + res
                  fac <- cbind(fac, new)
                  iv <- c(iv, set[[i]])
                }
                if (modelType == "Path" | modelType == "Path.exo") {
                  Data <- fac
                } else {
                  trueScore <- fac %*% t(usedParam2@LY)
                  errorScore <- run(object@errorDist, n, usedParam2@TY, usedParam2@TE)
                  Data <- trueScore + errorScore
                }
            }
        } else {
            suff <- createImpliedMACS(usedParam)
            Data <- run(object@indDist, n, suff$M, suff$CM)
        }
    }
    varnames <- NULL
    if (modelType == "Path.exo") {
        nx <- ncol(run(object@param@GA))
        for (i in 1:nx) {
            temp <- paste("x", i, sep = "")
            varnames <- c(varnames, temp)
        }
        for (i in 1:(ncol(Data) - nx)) {
            temp <- paste("y", i, sep = "")
            varnames <- c(varnames, temp)
        }
    } else if (modelType == "SEM.exo") {
        nx <- nrow(run(object@param@LX))
        for (i in 1:nx) {
            temp <- paste("x", i, sep = "")
            varnames <- c(varnames, temp)
        }
        for (i in 1:(ncol(Data) - nx)) {
            temp <- paste("y", i, sep = "")
            varnames <- c(varnames, temp)
        }
    } else {
        for (i in 1:ncol(Data)) {
            temp <- paste("y", i, sep = "")
            varnames <- c(varnames, temp)
        }
    }
    colnames(Data) <- varnames
    Data <- as.data.frame(Data)
    if (dataOnly) {
        return(Data)
    } else {
        if (is.null(misspec)) 
            misspec <- new("NullRSet")
        out <- new("SimDataOut", modelType = object@modelType, data = Data, param = createFreeParameters(object@param), paramOut = param, 
            misspecOut = misspec, equalCon = object@equalCon, n = n)
        return(out)
    }
} 
