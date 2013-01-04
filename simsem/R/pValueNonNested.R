# pValueNonNested: Find p value for a nonnested model comparison

pValueNonNested <- function(outMod1, outMod2, dat1Mod1, dat1Mod2, dat2Mod1, dat2Mod2, 
    usedFit = NULL, nVal = NULL, pmMCARval = NULL, pmMARval = NULL, df = 0, onetailed = FALSE) {
    mod1 <- cleanMultiple(dat1Mod1, dat1Mod2)
    dat1Mod1 <- mod1[[1]]
    dat1Mod2 <- mod1[[2]]
    mod2 <- cleanMultiple(dat2Mod1, dat2Mod2)
    dat2Mod1 <- mod2[[1]]
    dat2Mod2 <- mod2[[2]]
    if (is.null(usedFit)) 
        usedFit <- getKeywords()$usedFit
    revDirec <- (usedFit %in% c("CFI", "TLI"))  # CFA --> FALSE, RMSEA --> TRUE
    
    if (!isTRUE(all.equal(unique(dat2Mod1$paramValue), unique(dat2Mod2$paramValue)))) 
        stop("'dat2Mod1' and 'dat2Mod2' are based on different data and cannot be compared, check your random seed")
    if (!isTRUE(all.equal(unique(dat1Mod1$paramValue), unique(dat1Mod2$paramValue)))) 
        stop("'dat1Mod1' and 'dat1Mod2' are based on different data and cannot be compared, check your random seed")
    if (!multipleAllEqual(unique(dat2Mod1$n), unique(dat2Mod2$n), unique(dat1Mod1$n), 
        unique(dat1Mod2$n))) 
        stop("Models are based on different values of sample sizes")
    if (!multipleAllEqual(unique(dat2Mod1$pmMCAR), unique(dat2Mod2$pmMCAR), unique(dat1Mod1$pmMCAR), 
        unique(dat1Mod2$pmMCAR))) 
        stop("Models are based on different values of the percent completely missing at random")
    if (!multipleAllEqual(unique(dat2Mod1$pmMAR), unique(dat2Mod2$pmMAR), unique(dat1Mod1$pmMAR), 
        unique(dat1Mod2$pmMAR))) 
        stop("Models are based on different values of the percent missing at random")
    
    if (is.null(nVal) || is.na(nVal)) 
        nVal <- NULL
    if (is.null(pmMCARval) || is.na(pmMCARval)) 
        pmMCARval <- NULL
    if (is.null(pmMARval) || is.na(pmMARval)) 
        pmMARval <- NULL
    
    condition <- c(length(unique(dat2Mod1$pmMCAR)) > 1, length(unique(dat2Mod1$pmMAR)) > 
        1, length(unique(dat2Mod1$n)) > 1)
    condValue <- cbind(dat2Mod1$pmMCAR, dat2Mod1$pmMAR, dat2Mod1$n)
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
    
    Data1 <- as.data.frame((dat1Mod1$fit - dat1Mod2$fit)[, usedFit])
    Data2 <- as.data.frame((dat2Mod1$fit - dat2Mod2$fit)[, usedFit])
    
    cutoff <- extractLavaanFit(outMod1)[usedFit] - extractLavaanFit(outMod2)[usedFit]
    
    result1 <- NULL
    result2 <- NULL
    if (any(condition)) {
        result1 <- pValue(cutoff, Data1, revDirec, x = condValue, xval = predictorVal, 
            df = df, asLogical = FALSE)
        names(result1) <- usedFit
        result2 <- pValue(cutoff, Data2, !revDirec, x = condValue, xval = predictorVal, 
            df = df, asLogical = FALSE)
        names(result2) <- usedFit
    } else {
        logicalMat1 <- pValue(cutoff, Data1, revDirec, asLogical = TRUE)
        result1 <- apply(logicalMat1, 2, mean, na.rm = TRUE)
        names(result1) <- usedFit
        andRule1 <- mean(apply(logicalMat1, 1, all), na.rm = TRUE)
        orRule1 <- mean(apply(logicalMat1, 1, any), na.rm = TRUE)
        result1 <- c(result1, andRule = andRule1, orRule = orRule1)
        logicalMat2 <- pValue(cutoff, Data2, !revDirec, asLogical = TRUE)
        result2 <- apply(logicalMat2, 2, mean, na.rm = TRUE)
        names(result2) <- usedFit
        andRule2 <- mean(apply(logicalMat2, 1, all), na.rm = TRUE)
        orRule2 <- mean(apply(logicalMat2, 1, any), na.rm = TRUE)
        result2 <- c(result2, andRule = andRule2, orRule = orRule2)
    }
    if (!onetailed) {
        result1 <- twoTailedPValue(result1)
        result2 <- twoTailedPValue(result2)
    }
    return(list(pValueMod1 = result1, pValueMod2 = result2))
}

# twoTaledPValue: Find two-tailed \emph{p} value from one-tailed \emph{p} value

# arguments: vec A vector of one-tailed \emph{p} value.

# value A vector of two-tailed \emph{p} value.

twoTailedPValue <- function(vec) {
    apply(cbind(vec, 1 - vec), 1, min) * 2
} 
