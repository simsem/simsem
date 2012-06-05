# getCutoff: This function will find a cutoff of each fit index based on a
# priori alpha level from sampling distributions of fit indices

setMethod("getCutoff", signature(object = "data.frame"), definition = function(object, 
    alpha, revDirec = FALSE, usedFit = NULL, predictor = NULL, predictorVal = NULL, 
    df = 0) {
    if (is.null(usedFit)) 
        usedFit <- getKeywords()$usedFit
    percentile <- 1 - alpha
    if (revDirec) 
        percentile <- 1 - percentile
    object <- as.data.frame(object[, usedFit])
    temp <- rep(NA, ncol(object))
    temp <- apply(object, 2, getCondQtile, qtile = percentile, df = df, x = predictor, 
        xval = predictorVal)
    if ("TLI" %in% colnames(object)) 
        temp["TLI"] <- getCondQtile(object[, "TLI"], x = predictor, xval = predictorVal, 
            qtile = 1 - percentile, df = df)
    if ("CFI" %in% colnames(object)) 
        temp["CFI"] <- getCondQtile(object[, "CFI"], x = predictor, xval = predictorVal, 
            qtile = 1 - percentile, df = df)
    return(temp)
})

setMethod("getCutoff", signature(object = "SimResult"), definition = function(object, 
    alpha, revDirec = FALSE, usedFit = NULL, nVal = NULL, pmMCARval = NULL, pmMARval = NULL, 
    df = 0) {
    if (is.null(nVal) || is.na(nVal)) 
        nVal <- NULL
    if (is.null(pmMCARval) || is.na(pmMCARval)) 
        pmMCARval <- NULL
    if (is.null(pmMARval) || is.na(pmMARval)) 
        pmMARval <- NULL
    object <- clean(object)
    Data <- as.data.frame(object@fit)
    if (!is.null(alpha)) {
        if (revDirec) 
            alpha <- 1 - alpha
        cutoff <- getCutoff(Data, alpha)
    }
    condition <- c(length(object@pmMCAR) > 1, length(object@pmMAR) > 1, length(object@n) > 
        1)
    condValue <- cbind(object@pmMCAR, object@pmMAR, object@n)
    colnames(condValue) <- c("Percent MCAR", "Percent MAR", "N")
    condValue <- condValue[, condition]
    if (is.null(condValue) || length(condValue) == 0) 
        condValue <- NULL
    predictorVal <- rep(NA, 3)
    if (condition[3]) {
        ifelse(is.null(nVal), stop("Please specify the sample size value, 'nVal', because the sample size in the result object is varying"), 
            predictorVal[3] <- nVal)
    }
    if (condition[1]) {
        ifelse(is.null(pmMCARval), stop("Please specify the percent of completely missing at random, 'pmMCARval', because the percent of completely missing at random in the result object is varying"), 
            predictorVal[1] <- pmMCARval)
    }
    if (condition[2]) {
        ifelse(is.null(pmMARval), stop("Please specify the percent of missing at random, 'pmMARval', because the percent of missing at random in the result object is varying"), 
            predictorVal[2] <- pmMARval)
    }
    predictorVal <- predictorVal[condition]
    
    
    output <- getCutoff(Data, alpha, revDirec, usedFit, predictor = condValue, predictorVal = predictorVal, 
        df = df)
    return(output)
})

setMethod("getCutoff", signature(object = "matrix"), definition = function(object, 
    alpha, revDirec = FALSE, usedFit = NULL, predictor = NULL, predictorVal = NULL, 
    df = 0) {
    object <- as.data.frame(object)
    output <- getCutoff(object, alpha, revDirec, usedFit, predictor = predictor, 
        predictorVal = predictorVal, df = df)
    return(output)
}) 
