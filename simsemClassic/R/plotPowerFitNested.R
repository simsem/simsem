# plotPowerFitNested: This function will plot sampling distributions of difference in fit indices that visualize power

plotPowerFitNested <- function(altNested, altParent, nullNested = NULL, nullParent = NULL, cutoff = NULL, usedFit = NULL, alpha = 0.05, contN = TRUE, contMCAR = TRUE, contMAR = TRUE, useContour = TRUE, logistic = TRUE) {
	if(is.null(nullNested) & is.null(nullParent)) {
		mod <- clean(altNested, altParent)
		altNested <- mod[[1]]
		altParent <- mod[[2]]
		if(!isTRUE(all.equal(unique(altNested@paramValue), unique(altParent@paramValue)))) stop("Models are based on different data and cannot be compared, check your random seed")
		if(!isTRUE(all.equal(unique(altNested@n), unique(altParent@n)))) stop("Models are based on different values of sample sizes")
		if(!isTRUE(all.equal(unique(altNested@pmMCAR), unique(altParent@pmMCAR)))) stop("Models are based on different values of the percent completely missing at random")
		if(!isTRUE(all.equal(unique(altNested@pmMAR), unique(altParent@pmMAR)))) stop("Models are based on different values of the percent missing at random")
    } else if (!is.null(nullNested) & !is.null(nullParent)) {
		mod <- clean(altNested, altParent, nullNested, nullParent)
		altNested <- mod[[1]]
		altParent <- mod[[2]]
		nullNested <- mod[[3]]
		nullParent <- mod[[4]]
		if(!isTRUE(all.equal(unique(altNested@paramValue), unique(altParent@paramValue)))) stop("'altNested' and 'altParent' are based on different data and cannot be compared, check your random seed")
		if(!isTRUE(all.equal(unique(nullNested@paramValue), unique(nullParent@paramValue)))) stop("'nullNested' and 'nullParent' are based on different data and cannot be compared, check your random seed")
		if(!multipleAllEqual(unique(altNested@n), unique(altParent@n), unique(nullNested@n), unique(nullParent@n))) stop("Models are based on different values of sample sizes")
		if(!multipleAllEqual(unique(altNested@pmMCAR), unique(altParent@pmMCAR), unique(nullNested@pmMCAR), unique(nullParent@pmMCAR))) stop("Models are based on different values of the percent completely missing at random")
		if(!multipleAllEqual(unique(altNested@pmMAR), unique(altParent@pmMAR), unique(nullNested@pmMAR), unique(nullParent@pmMAR))) stop("Models are based on different values of the percent missing at random")
	} else {
		stop("The nullNested and nullParent arguments should be both specified.")
	}
	nrep <- dim(altNested@fit)[[1]]
	if (is.null(usedFit)) 
        usedFit <- getKeywords()$usedFit
	if (!is.null(cutoff)) {
		usedFit <- intersect(usedFit, names(cutoff))
		cutoff <- cutoff[usedFit]
    }
    # Create matrix of predictors (randomly varying params)
    x <- NULL
    pred <- NULL
    
    if ((length(altNested@n) > 1) && contN) {
        if (!length(altNested@n) == nrep) {
            stop("Number of random sample sizes is not the same as the number of replications, check to see if N varied across replications")
        }
        x <- cbind(x, N = altNested@n)
        pred$N <- min(altNested@n):max(altNested@n)
    }
    if ((length(altNested@pmMCAR) > 1) && contMCAR) {
        if (!length(altNested@pmMCAR) == nrep) {
            stop("Number of random pmMCARs is not the same as the number of replications, check to see if pmMCAR varied across replications")
        }
        x <- cbind(x, pmMCAR = altNested@pmMCAR)
        pred$MCAR <- seq(min(altNested@pmMCAR), max(altNested@pmMCAR), by = 0.01)
        
    }
    if ((length(altNested@pmMAR) > 1) && contMAR) {
        if (!length(altNested@pmMAR) == nrep) {
            stop("Number of random pmMARs is not the same as the number of replications, check to see if pmMAR varied across replications")
        }
        x <- cbind(x, pmMAR = altNested@pmMAR)
        pred$MAR <- seq(min(altNested@pmMAR), max(altNested@pmMAR), by = 0.01)
        
    }
	nullMod <- NULL
	altMod <- as.data.frame(altNested@fit - altParent@fit)
	if(!is.null(nullNested)) nullMod <- as.data.frame(nullNested@fit - nullParent@fit)
	plotPowerFitDf(altMod, nullObject = nullMod, cutoff = cutoff, usedFit = usedFit, alpha = alpha, x = x, xval = pred, useContour = useContour, logistic = logistic)
}
