# getCutoff: This function will find a cutoff of each fit index based on a priori alpha level from sampling distributions of fit indices

setMethod("getCutoff", signature(object = "data.frame"), definition = function(object, alpha, revDirec = FALSE, usedFit = NULL, predictor = NULL, predictorVal = NULL, df = 0) {
    if (is.null(usedFit)) 
        usedFit <- getKeywords()$usedFit
    percentile <- 1 - alpha
    if (revDirec) 
        percentile <- 1 - percentile
    object <- as.data.frame(object[, usedFit])
    temp <- rep(NA, ncol(object))
    temp <- apply(object, 2, getCondQtile, qtile = percentile, df = df, x = predictor, xval = predictorVal)
    if ("TLI" %in% colnames(object)) 
        temp["TLI"] <- getCondQtile(object[, "TLI"], x = predictor, xval = predictorVal, qtile = 1 - percentile, df = df)
    if ("CFI" %in% colnames(object)) 
        temp["CFI"] <- getCondQtile(object[, "CFI"], x = predictor, xval = predictorVal, qtile = 1 - percentile, df = df)
    return(temp)
})

setMethod("getCutoff", signature(object = "SimResult"), definition = function(object, alpha, revDirec = FALSE, usedFit = NULL, nVal = NULL, pmMCARval = NULL, pmMARval = NULL, df = 0) {
    object <- clean(object)
	Data <- as.data.frame(object@fit)
    if (!is.null(alpha)) {
        if (revDirec) 
            alpha <- 1 - alpha
        cutoff <- getCutoff(Data, alpha)
    }
    condition <- c(length(object@pmMCAR) > 1, length(object@pmMAR) > 1, length(object@n) > 1)
    condValue <- cbind(object@pmMCAR, object@pmMAR, object@n)
    colnames(condValue) <- c("Percent MCAR", "Percent MAR", "N")
	condValue <- condValue[,condition]
	predictorVal <- rep(NA, 3)
	if(condition[1]) {
		ifelse(is.null(nVal), stop("Please specify the sample size value, 'nVal', because the sample size in the result object is varying"), predictorVal[1] <- nVal)
	}
	if(condition[2]) {
		ifelse(is.null(pmMCARval), stop("Please specify the percent of completely missing at random, 'pmMCARval', because the percent of completely missing at random in the result object is varying"), predictorVal[2] <- pmMCARval)
	}
	if(condition[3]) {
		ifelse(is.null(pmMARval), stop("Please specify the percent of missing at random, 'pmMARval', because the percent of missing at random in the result object is varying"), predictorVal[3] <- pmMARval)
	}
	predictorVal <- predictorVal[,condition]
	
	
    output <- getCutoff(Data, alpha, revDirec, usedFit, predictor = condValue, predictorVal = predictorVal, df = df)
    return(output)
})

setMethod("getCutoff", signature(object = "matrix"), definition = function(object, alpha, revDirec = FALSE, usedFit = NULL) {
    object <- as.data.frame(object)
    output <- getCutoff(object, alpha, revDirec, usedFit)
    return(output)
}) 

getCondQtile <- function(y, x=NULL, xval=NULL, df = 0, qtile = 0.5) {
	if(is.null(x)) {
		return(quantile(y, probs=qtile, na.rm=TRUE))
	} else {
		if(!is.matrix(x)) x <- as.matrix(x)
		p <- ncol(x)
		name <- paste("x", 1:p, sep="")
		colnames(x) <- name
		names(xval) <- name
		if(df == 0) {
			name2 <- name
		} else {
			library(splines)
			name2 <- paste("ns(", name, ",", df, ")", sep="")
		}
		firstord <- paste(name2, collapse=" + ")
		FUN <- function(x, y) paste(x, " * ", y, sep="")
		secondord <- outer(name2, name2, FUN)[lower.tri(diag(length(name2)))]
		secondord <- paste(secondord, collapse=" + ")
		express <- paste("y ~ ", firstord, " + ", secondord, sep="")
		dat <- data.frame(y = y, x)
		mod <- rq(express, data = dat, tau = qtile)
		xvalSecondord <- outer(xval, xval, "*")[lower.tri(diag(length(xval)))]
		predictorVal <- c(1, xval, xvalSecondord)
		result <- sum(mod$coefficients * predictorVal)
		return(result)
	}
}
