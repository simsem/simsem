# miPoolVector: Pool MI results that providing in matrix or vector formats

miPoolVector <- function(MI.param, MI.se, imps) {
    # compute parameter estimates
    Estimates <- colMeans(MI.param)
    
    # compute between-imputation variance: variance of parameter estimates
    Bm <- apply(MI.param, 2, var)
    
    # compute within-imputation variance: average of squared estimated SEs
    Um <- apply(MI.se^2, 2, mean)
    
    # compute total variance: sum of between- and within- variance with correction
    TV <- Um + ((imps + 1)/imps) * Bm
    
    # compute correction factor for fraction of missing info
    nu <- (imps - 1) * ((((1 + 1/imps) * Bm)/TV)^-2)
    
    # compute 2 estimates of fraction of missing information
    FMI.1 <- 1 - (Um/TV)
    FMI.2 <- 1 - ((nu + 1) * Um)/((nu + 3) * TV)
    FMI.2[is.nan(FMI.2)] <- 0
    FMI <- rbind(FMI.1, FMI.2)
    
    SE <- sqrt(TV)
    MI.res <- list(Estimates, SE, FMI.1, FMI.2)
    names(MI.res) <- c("coef", "se", "FMI.1", "FMI.2")
    return(MI.res)
}

# miPoolChi: Pool Chi-square statistic based on Li, Meng, Raghunathan, & Rubin (1991) adapted from

miPoolChi <- function(chis, df) {
    # From Li, Meng, Raghunathan, & Rubin (1991)
    if (is.matrix(chis)) {
        ifelse(ncol(chis) == 1 | nrow(chis) == 1, chis <- as.vector(chis), stop("Please put a vector of chi-square values"))
    }
    m <- length(chis)
    dbar <- mean(chis)
    sqrtd <- sqrt(chis)
    xbarsqrtd <- mean(sqrtd)
    # Equation 2.2
    r <- (1 + 1/m) * (sum((sqrtd - xbarsqrtd)^2)/(m - 1))
    # Equation 2.1
    D <- (dbar/df - ((m + 1) * r/(m - 1)))/(1 + r)
    if (D < 0) 
        D <- 0
    # Equation 2.16 and 2.17
    aw <- df^(-(3/m)) * (m - 1) * (1 + (1/r))^2
    p <- 1 - pf(D, df, aw)
    result <- c(D, df, aw, p)
    names(result) <- c("F", "df1", "df2", "p.F")
    return(result)
}

# miPool: Pool MI results in SimModelOut class format

miPool <- function(Result.l) {
    Converged <- sapply(Result.l, function(object) {
        object@converged
    })
    
    allNames <- slotNames(Result.l[[which(Converged == TRUE)[1]]]@param)
    paramNames <- allNames != "modelType"
    paramNames <- allNames[paramNames]
    
    OutputCoef <- Result.l[[which(Converged == TRUE)[1]]]@coef
    OutputSE <- Result.l[[which(Converged == TRUE)[1]]]@se
    OutputFMI1 <- Result.l[[which(Converged == TRUE)[1]]]@se
    OutputFMI2 <- Result.l[[which(Converged == TRUE)[1]]]@se
    
    for (i in 1:length(paramNames)) {
        if (!isNullObject(slot(OutputCoef, paramNames[i]))) {
            mparam <- as.matrix(sapply(Result.l, function(result, slotname1, slotname2) {
                slot(slot(result, slotname1), slotname2)
            }, slotname1 = "param", slotname2 = paramNames[i]))
            if (ncol(mparam) == 1) 
                mparam <- t(mparam)  # Prevent single element matrix problem
            mparam <- mparam[, 1]
            mcoef <- as.matrix(sapply(Result.l, function(result, slotname1, slotname2) {
                slot(slot(result, slotname1), slotname2)
            }, slotname1 = "coef", slotname2 = paramNames[i]))
            if (ncol(mcoef) == 1) 
                mcoef <- t(mcoef)  # Prevent single element matrix problem
            mcoef <- mcoef[is.na(mparam), ]
            mse <- as.matrix(sapply(Result.l, function(result, slotname1, slotname2) {
                slot(slot(result, slotname1), slotname2)
            }, slotname1 = "se", slotname2 = paramNames[i]))
            if (ncol(mse) == 1) 
                mse <- t(mse)  # Prevent single element matrix problem
            mse <- mse[is.na(mparam), ]
            temp <- miPoolVector(t(mcoef), t(mse), length(Result.l))
            temp1 <- as.vector(slot(OutputCoef, paramNames[i]))
            temp2 <- as.vector(slot(OutputSE, paramNames[i]))
            temp3 <- as.vector(slot(OutputFMI1, paramNames[i]))
            temp4 <- as.vector(slot(OutputFMI2, paramNames[i]))
            temp1[which(is.na(mparam))] <- temp$coef
            temp2[which(is.na(mparam))] <- temp$se
            temp3[which(is.na(mparam))] <- temp$FMI.1
            temp4[which(is.na(mparam))] <- temp$FMI.2
            if (is.matrix(slot(OutputCoef, paramNames[i]))) {
                numcol <- ncol((slot(OutputCoef, paramNames[i])))
                slot(OutputCoef, paramNames[i]) <- matrix(temp1, ncol = numcol)
                slot(OutputSE, paramNames[i]) <- matrix(temp2, ncol = numcol)
                slot(OutputFMI1, paramNames[i]) <- matrix(temp3, ncol = numcol)
                slot(OutputFMI2, paramNames[i]) <- matrix(temp4, ncol = numcol)
            } else {
                slot(OutputCoef, paramNames[i]) <- temp1
                slot(OutputSE, paramNames[i]) <- temp2
                slot(OutputFMI1, paramNames[i]) <- temp3
                slot(OutputFMI2, paramNames[i]) <- temp4
            }
        }
    }
    conv <- mean(Converged, na.rm = TRUE) > 0.8
    start <- Result.l[[which(Converged == TRUE)[1]]]@start
    equalCon <- Result.l[[which(Converged == TRUE)[1]]]@equalCon
    package <- Result.l[[which(Converged == TRUE)[1]]]@package
    param <- Result.l[[which(Converged == TRUE)[1]]]@param
    
    Fit <- sapply(Result.l, function(object) {
        object@fit
    })
    dfPool <- Fit["df", 1]
    nullDfPool <- Fit["baseline.df", 1]
    chiPool <- miPoolChi(Fit["Chi", Converged], dfPool)
    nullChiPool <- miPoolChi(Fit["baseline.Chi", Converged], nullDfPool)
    OutputFit <- fitMeasuresChi(X2 = chiPool["df1"] * chiPool["F"], df = chiPool["df1"], p = chiPool["p.F"], X2.null = nullChiPool["df1"] * nullChiPool["F"], df.null = nullChiPool["df1"], p.null = nullChiPool["p.F"], 
        N = Result.l[[1]]@n, fit.measures = "all")
    toGetAverage <- setdiff(rownames(Fit), names(OutputFit))
    OutputFit2 <- rowMeans(Fit[toGetAverage, Converged], na.rm = TRUE)
    OutputFit <- c(OutputFit, OutputFit2)[rownames(Fit)]
    names(nullChiPool) <- paste("baseline.", names(nullChiPool), sep = "")
    OutputFit <- c(OutputFit, chiPool, nullChiPool)
    return(new("SimModelMIOut", param = param, start = start, equalCon = equalCon, package = package, coef = OutputCoef, fit = OutputFit, se = OutputSE, converged = conv, FMI1 = OutputFMI1, FMI2 = OutputFMI2))
} 
