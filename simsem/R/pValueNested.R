# pValueNested: Find p value for a nested model comparison

pValueNested <- function(outNested, outParent, simNested, simParent, usedFit = NULL, 
    nVal = NULL, pmMCARval = NULL, pmMARval = NULL, df = 0) {
    mod <- clean(simNested, simParent)
    simNested <- mod[[1]]
    simParent <- mod[[2]]
    if (is.null(usedFit)) 
        usedFit <- getKeywords()$usedFit
    revDirec <- (usedFit %in% c("CFI", "TLI"))  # CFA --> FALSE, RMSEA --> TRUE
    
    if (!isTRUE(all.equal(unique(simNested@paramValue), unique(simParent@paramValue)))) 
        stop("Models are based on different data and cannot be compared, check your random seed")
    if (!isTRUE(all.equal(unique(simNested@n), unique(simParent@n)))) 
        stop("Models are based on different values of sample sizes")
    if (!isTRUE(all.equal(unique(simNested@pmMCAR), unique(simParent@pmMCAR)))) 
        stop("Models are based on different values of the percent completely missing at random")
    if (!isTRUE(all.equal(unique(simNested@pmMAR), unique(simParent@pmMAR)))) 
        stop("Models are based on different values of the percent missing at random")
    
    if (is.null(nVal) || is.na(nVal)) 
        nVal <- NULL
    if (is.null(pmMCARval) || is.na(pmMCARval)) 
        pmMCARval <- NULL
    if (is.null(pmMARval) || is.na(pmMARval)) 
        pmMARval <- NULL
    Data <- as.data.frame((simNested@fit - simParent@fit)[, usedFit])
    condition <- c(length(unique(simNested@pmMCAR)) > 1, length(unique(simNested@pmMAR)) > 
        1, length(unique(simNested@n)) > 1)
    condValue <- cbind(simNested@pmMCAR, simNested@pmMAR, simNested@n)
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
        ifelse(is.null(pmMCARval), stop("Please specify the percent of missing completely at random, 'pmMCARval', because the percent of missing completely at random in the result object is varying"), 
            predictorVal[1] <- pmMCARval)
    }
    if (condition[2]) {
        ifelse(is.null(pmMARval), stop("Please specify the percent of missing at random, 'pmMARval', because the percent of missing at random in the result object is varying"), 
            predictorVal[2] <- pmMARval)
    }
    predictorVal <- predictorVal[condition]
    cutoff <- extractLavaanFit(outNested)[usedFit] - extractLavaanFit(outParent)[usedFit]
    if (any(condition)) {
        result <- pValue(cutoff, Data, revDirec, x = condValue, xval = predictorVal, 
            df = df, asLogical = FALSE)
        names(result) <- usedFit
        return(result)
    } else {
        logicalMat <- pValue(cutoff, Data, revDirec, asLogical = TRUE)
        result <- apply(logicalMat, 2, mean, na.rm = TRUE)
        names(result) <- usedFit
        andRule <- mean(apply(logicalMat, 1, all), na.rm = TRUE)
        orRule <- mean(apply(logicalMat, 1, any), na.rm = TRUE)
        return(c(result, andRule = andRule, orRule = orRule))
    }
} 
