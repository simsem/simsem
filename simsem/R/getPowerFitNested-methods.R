# getPowerFitNested: This function will find a power of each fit index in
# nested model comparison based on specified cutoffs of each fit index

getPowerFitNested <- function(altNested, altParent, cutoff = NULL, nullNested = NULL, nullParent = NULL, revDirec = FALSE, usedFit = NULL, alpha = 0.05, nVal = NULL, pmMCARval = NULL, pmMARval = NULL, condCutoff = TRUE, df = 0) {
	result <- NULL
	if(is.null(cutoff)) {
		if(!is.null(nullNested) & !is.null(nullParent)) {
			result <- getPowerFitNestedNullObj(altNested = altNested, altParent = altParent, nullNested = nullNested, nullParent = nullParent, revDirec = revDirec, usedFit = usedFit, alpha = alpha, nVal = nVal, pmMCARval = pmMCARval, pmMARval = pmMARval, df = df)
		} else {
			stop("Please specify fit index cutoff, 'cutoff', or the result object representing the null model, 'nullObject'.")
		}
	} else {
		if(is.null(nullNested) & is.null(nullParent)) {
			result <- getPowerFitNestedCutoff(altNested = altNested, altParent = altParent, cutoff = cutoff, revDirec = revDirec, usedFit = usedFit, nVal = nVal, pmMCARval = pmMCARval, pmMARval = pmMARval, condCutoff = condCutoff, df = df)
		} else {
			stop("Please specify either fit index cutoff, 'cutoff', or the result object representing the null model, 'nullObject', but not both.")
		}
	}
	result
}

getPowerFitNestedCutoff <- function(altNested, altParent, cutoff, revDirec = FALSE, 
    usedFit = NULL, nVal = NULL, pmMCARval = NULL, pmMARval = NULL, condCutoff = TRUE, 
    df = 0) {
    if (is.null(nVal) || is.na(nVal)) 
        nVal <- NULL
    if (is.null(pmMCARval) || is.na(pmMCARval)) 
        pmMCARval <- NULL
    if (is.null(pmMARval) || is.na(pmMARval)) 
        pmMARval <- NULL
    mod <- clean(altNested, altParent)
    altNested <- mod[[1]]
    altParent <- mod[[2]]
    if (!isTRUE(all.equal(unique(altNested@paramValue), unique(altParent@paramValue)))) 
        stop("Models are based on different data and cannot be compared, check your random seed")
    if (!isTRUE(all.equal(unique(altNested@n), unique(altParent@n)))) 
        stop("Models are based on different values of sample sizes")
    if (!isTRUE(all.equal(unique(altNested@pmMCAR), unique(altParent@pmMCAR)))) 
        stop("Models are based on different values of the percent completely missing at random")
    if (!isTRUE(all.equal(unique(altNested@pmMAR), unique(altParent@pmMAR)))) 
        stop("Models are based on different values of the percent missing at random")
    Data <- as.data.frame((altNested@fit - altParent@fit))
    condition <- c(length(unique(altNested@pmMCAR)) > 1, length(unique(altNested@pmMAR)) > 
        1, length(unique(altNested@n)) > 1)
    condValue <- cbind(altNested@pmMCAR, altNested@pmMAR, altNested@n)
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
    
    
    output <- getPowerFitDataFrame(Data, cutoff, revDirec, usedFit, predictor = condValue, 
        predictorVal = predictorVal, condCutoff = condCutoff, df = df)
    return(output)
}

getPowerFitNestedNullObj <- function(altNested, altParent, 
    nullNested, nullParent, revDirec = FALSE, usedFit = NULL, alpha = 0.05, nVal = NULL, 
    pmMCARval = NULL, pmMARval = NULL, df = 0) {
    if (!multipleAllEqual(unique(altNested@n), unique(altParent@n), unique(nullNested@n), 
        unique(nullParent@n))) 
        stop("Models are based on different values of sample sizes")
    if (!multipleAllEqual(unique(altNested@pmMCAR), unique(altParent@pmMCAR), unique(nullNested@pmMCAR), 
        unique(nullParent@pmMCAR))) 
        stop("Models are based on different values of the percent completely missing at random")
    if (!multipleAllEqual(unique(altNested@pmMAR), unique(altParent@pmMAR), unique(nullNested@pmMAR), 
        unique(nullParent@pmMAR))) 
        stop("Models are based on different values of the percent missing at random")
    if (!isTRUE(all.equal(unique(altNested@paramValue), unique(altParent@paramValue)))) 
        stop("'altNested' and 'altParent' are based on different data and cannot be compared, check your random seed")
    if (!isTRUE(all.equal(unique(nullNested@paramValue), unique(nullParent@paramValue)))) 
        stop("'nullNested' and 'nullParent' are based on different data and cannot be compared, check your random seed")
	usedFit <- cleanUsedFit(usedFit, colnames(altNested@fit), colnames(altParent@fit), colnames(nullNested@fit), colnames(nullParent@fit))
	if(is.null(nullNested)) nullNested <- altNested
	if(is.null(nullParent)) nullParent <- altParent
    mod <- clean(altNested, altParent, nullNested, nullParent)
    altNested <- mod[[1]]
    altParent <- mod[[2]]
    nullNested <- mod[[3]]
    nullParent <- mod[[4]]
    if (is.null(nVal) || is.na(nVal)) 
        nVal <- NULL
    if (is.null(pmMCARval) || is.na(pmMCARval)) 
        pmMCARval <- NULL
    if (is.null(pmMARval) || is.na(pmMARval)) 
        pmMARval <- NULL
    condition <- c(length(unique(altNested@pmMCAR)) > 1, length(unique(altNested@pmMAR)) > 
        1, length(unique(altNested@n)) > 1)
    condValue <- cbind(altNested@pmMCAR, altNested@pmMAR, altNested@n)
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
    
    usedDirec <- (usedFit %in% getKeywords()$reversedFit)  # CFA --> TRUE, RMSEA --> FALSE
    if (revDirec) 
        usedDirec <- !usedDirec
    usedDist <- as.data.frame((altNested@fit - altParent@fit)[, usedFit])
    nullFit <- as.data.frame((nullNested@fit - nullParent@fit)[, usedFit])
    temp <- rep(NA, length(usedFit))
    if (is.null(condValue)) {
        usedCutoff <- as.vector(t(getCutoff(nullFit, alpha = alpha, usedFit = usedFit)))
        names(usedCutoff) <- usedFit
        temp <- pValue(usedCutoff, usedDist, revDirec = usedDirec)
		names(temp) <- usedFit
		cutoffChisq <- qchisq(1 - alpha, df=(nullNested@fit - nullParent@fit)[,"df"])
		powerChi <- mean((altNested@fit - altParent@fit)[,"chisq"] > cutoffChisq)
		temp <- c("TraditionalChi" = powerChi, temp)		
    } else {
        varyingCutoff <- getCutoff(object = nullFit, alpha = alpha, revDirec = FALSE, 
            usedFit = usedFit, predictor = condValue, df = df, predictorVal = "all")
        for (i in 1:length(temp)) {
            temp[i] <- pValueVariedCutoff(varyingCutoff[, i], usedDist[, i], revDirec = usedDirec[i], 
                x = condValue, xval = predictorVal)
        }
		names(temp) <- usedFit
    }
    return(temp)
}

# multipleAllEqual: Check whether all objects are equal by using all.equal
# function

multipleAllEqual <- function(...) {
    obj <- list(...)
    multipleAllEqualList(obj)
}

multipleAllEqualList <- function(obj) {
    for (i in 2:length(obj)) {
        for (j in 1:(i - 1)) {
            temp <- isTRUE(all.equal(obj[[i]], obj[[j]]))
            if (!temp) 
                return(FALSE)
        }
    }
    return(TRUE)
} 
