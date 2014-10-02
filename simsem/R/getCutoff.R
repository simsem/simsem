# getCutoff: This function will find a cutoff of each fit index based on a
# priori alpha level from sampling distributions of fit indices

getCutoffDataFrame <- function(object, 
    alpha, revDirec = FALSE, usedFit = NULL, predictor = NULL, predictorVal = NULL, 
    df = 0) {
	usedFit <- cleanUsedFit(usedFit, tolower(colnames(object)))
    percentile <- 1 - alpha
    if (revDirec) 
        percentile <- 1 - percentile
    object <- as.data.frame(object[, match(usedFit, tolower(colnames(object)))])
    colnames(object) <- usedFit
    temp <- list()
    temp <- lapply(object, getCondQtile, qtile = percentile, df = df, x = predictor, 
        xval = predictorVal)
	reversedCol <- which(colnames(object) %in% getKeywords()$reversedFit)
	for (i in seq_along(reversedCol)) {
        temp[[reversedCol[i]]] <- getCondQtile(object[, reversedCol[i]], x = predictor, xval = predictorVal, 
            qtile = 1 - percentile, df = df)
	}
    temp <- data.frame(temp)
    return(temp)
}

getCutoff <- function(object, 
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
    condValuePredictorVal <- getCondValuePredictorVal(object, nVal, pmMCARval, pmMARval)
    
    output <- getCutoffDataFrame(Data, alpha, revDirec, usedFit, predictor = condValuePredictorVal[[1]], predictorVal = condValuePredictorVal[[2]], 
        df = df)
    return(output)
}

getCondValuePredictorVal <- function(object, nVal = NULL, pmMCARval = NULL, pmMARval = NULL) {
	condition <- c(length(unique(object@pmMCAR)) > 1, length(unique(object@pmMAR)) > 
        1, length(unique(object@n)) > 1)
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
	list(condValue, predictorVal)
}

# setMethod("getCutoff", signature(object = "matrix"), definition = function(object, 
    # alpha, revDirec = FALSE, usedFit = NULL, predictor = NULL, predictorVal = NULL, 
    # df = 0) {
    # object <- as.data.frame(object)
    # output <- getCutoff(object, alpha, revDirec, usedFit, predictor = predictor, 
        # predictorVal = predictorVal, df = df)
    # return(output)
# }) 

## getCondQtile: Get a quantile of a variable given values of predictors

# \title{
	# Get a quantile of a variable given values of predictors
# }
# \description{
# Find a quantile of a variable. If the predictors are specified, the result will provide the conditional quantile given specified value of predictors. The \code{quantreg} package is used to find conditional quantile.
# }
# \usage{
# getCondQtile(y, x=NULL, xval=NULL, df = 0, qtile = 0.5)
# }
# \arguments{
  # \item{y}{
	# The variable that users wish to find a quantile from
# }
  # \item{x}{
	# The predictors variables. If \code{NULL}, the unconditional quantile of the \code{y} is provided.
# }
  # \item{xval}{
	# The vector of predictors' values that users wish to find the conditional quantile from. If \code{"all"} is specified, the function will provide the conditional quantile of every value in \code{x}.
# }
  # \item{df}{
	# The degree of freedom used in spline method in predicting the fit indices by the predictors. If \code{df} is 0, the spline method will not be applied. 
# }
  # \item{qtile}{
	# The quantile rank.
# }
# }
# \value{
	# A (conditional) quantile value
# }

getCondQtile <- function(y, x = NULL, xval = NULL, df = 0, qtile = 0.5) {
    if (is.null(x)) {
        return(quantile(y, probs = qtile, na.rm = TRUE))
    } else {
        if (!is.matrix(x)) 
            x <- as.matrix(x)
        p <- ncol(x)
        name <- paste("x", 1:p, sep = "")
        colnames(x) <- name
        if (df == 0) {
            name2 <- name
        } else {
            library(splines)
            name2 <- paste("ns(", name, ",", df, ")", sep = "")
        }
        firstord <- paste(name2, collapse = " + ")
        FUN <- function(x, y) paste(x, " * ", y, sep = "")
        secondord <- outer(name2, name2, FUN)[lower.tri(diag(length(name2)))]
        secondord2 <- paste(secondord, collapse = " + ")
        if (secondord2 == "") {
            express <- paste("y ~ ", firstord, sep = "")
        } else {
            express <- paste("y ~ ", firstord, " + ", secondord2, sep = "")
        }
        dat <- data.frame(y = y, x)
        library(quantreg)
        mod <- quantreg::rq(express, data = dat, tau = qtile)
        if (length(xval) == 1 && xval == "all") {
            result <- predict(mod, as.data.frame(x), interval = "none")
        } else {
            names(xval) <- name
            xvalSecondord <- outer(as.vector(xval), as.vector(xval), "*")[lower.tri(diag(length(xval)))]
            predictorVal <- c(1, xval, xvalSecondord)
            pred <- data.frame(t(xval))
            colnames(pred) <- colnames(x)
            result <- predict(mod, pred, interval = "none")
        }
        return(result)
    }
} 
