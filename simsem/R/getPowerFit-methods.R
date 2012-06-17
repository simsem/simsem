# getPowerFit: This function will find a power of each fit index based on specified cutoffs of each fit index

setMethod("getPowerFit", signature(altObject = "data.frame", cutoff = "vector"), definition = function(altObject, cutoff, revDirec = FALSE, usedFit = NULL, predictor = NULL, predictorVal = NULL, condCutoff=TRUE, df = 0) {
    if (is.null(usedFit)) 
        usedFit <- getKeywords()$usedFit
    if (is.null(names(cutoff)) && length(cutoff) == 7) 
        names(cutoff) <- usedFit
    common.name <- Reduce(intersect, list(colnames(altObject), names(cutoff), usedFit))
    temp <- rep(NA, length(common.name))
    names(temp) <- common.name
    altObject <- as.data.frame(altObject[, common.name])
    cutoff <- cutoff[common.name]
    for (i in 1:length(common.name)) {
        temp[i] <- pValue(target = as.numeric(cutoff[i]), dist=as.vector(altObject[, i]), revDirec = revDirec, x = predictor, xval = predictorVal, df=df, condCutoff=condCutoff)
    }
    if ("TLI" %in% common.name) 
        temp["TLI"] <- revText(temp["TLI"])
    if ("CFI" %in% common.name) 
        temp["CFI"] <- revText(temp["CFI"])
    return(temp)
})

setMethod("getPowerFit", signature(altObject = "SimResult", cutoff = "vector"), definition = function(altObject, cutoff, revDirec = FALSE, usedFit = NULL, nVal = NULL, pmMCARval = NULL, pmMARval = NULL, condCutoff=TRUE, df = 0) {
    if (is.null(nVal) || is.na(nVal)) 
        nVal <- NULL
    if (is.null(pmMCARval) || is.na(pmMCARval)) 
        pmMCARval <- NULL
    if (is.null(pmMARval) || is.na(pmMARval)) 
        pmMARval <- NULL
    altObject <- clean(altObject)
    Data <- as.data.frame(altObject@fit)
    condition <- c(length(altObject@pmMCAR) > 1, length(altObject@pmMAR) > 1, length(altObject@n) > 1)
    condValue <- cbind(altObject@pmMCAR, altObject@pmMAR, altObject@n)
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
    
    
    output <- getPowerFit(Data, cutoff, revDirec, usedFit, predictor = condValue, predictorVal = predictorVal, condCutoff=condCutoff, df = df)
    return(output)
})

setMethod("getPowerFit", signature(altObject = "matrix", cutoff = "vector"), definition = function(altObject, cutoff, revDirec = FALSE, usedFit = NULL, predictor = NULL, predictorVal = NULL, df = 0) {
    object <- as.data.frame(altObject)
    output <- getPowerFit(object, cutoff, revDirec, usedFit, predictor=predictor, predictorVal=predictorVal, df=df)
    return(output)
}) 

setMethod("getPowerFit", signature(altObject = "SimResult", cutoff = "missing"), definition = function(altObject, cutoff=NULL, nullObject, revDirec = FALSE, usedFit = NULL, alpha = 0.05, nVal = NULL, pmMCARval = NULL, pmMARval = NULL, df = 0) {
    if (is.null(usedFit)) 
        usedFit <- getKeywords()$usedFit
	mod <- clean(altObject, nullObject)
	altObject <- mod[[1]]
	nullObject <- mod[[2]]
	if(!all.equal(unique(altObject@n), unique(nullObject@n))) stop("Models are based on different values of sample sizes")
	if(!all.equal(unique(altObject@pmMCAR), unique(nullObject@pmMCAR))) stop("Models are based on different values of the percent completely missing at random")
	if(!all.equal(unique(altObject@pmMAR), unique(nullObject@pmMAR))) stop("Models are based on different values of the percent missing at random")
	if (is.null(nVal) || is.na(nVal)) 
        nVal <- NULL
    if (is.null(pmMCARval) || is.na(pmMCARval)) 
        pmMCARval <- NULL
    if (is.null(pmMARval) || is.na(pmMARval)) 
        pmMARval <- NULL
    condition <- c(length(altObject@pmMCAR) > 1, length(altObject@pmMAR) > 1, length(altObject@n) > 1)
    condValue <- cbind(altObject@pmMCAR, altObject@pmMAR, altObject@n)
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
	
	usedDirec <- (usedFit %in% c("CFI", "TLI")) # CFA --> TRUE, RMSEA --> FALSE
	if(revDirec) usedDirec <- !usedDirec	
	usedDist <- as.data.frame(altObject@fit[,usedFit])
	nullFit <- as.data.frame(nullObject@fit[,usedFit])
	temp <- rep(NA, length(usedFit))
	if(is.null(condValue)) {
		usedCutoff <- as.vector(t(getCutoff(nullObject, alpha=alpha, usedFit=usedFit)))
		names(usedCutoff) <- usedFit
		temp <- pValue(usedCutoff, as.data.frame(usedDist), revDirec=usedDirec)
	} else {
		varyingCutoff <- getCutoff(object=nullFit, alpha = alpha, revDirec = FALSE, usedFit = usedFit, predictor = condValue, df = df, predictorVal="all")
		for(i in 1:length(temp)) {
			temp[i] <- pValueVariedCutoff(varyingCutoff[,i], usedDist[,i], revDirec = usedDirec[i], x = condValue, xval = predictorVal)
		}	
	}
	names(temp) <- usedFit
    return(temp)
})
