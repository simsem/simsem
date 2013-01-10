# pValue: Find a p-value from an object

setMethod("pValue", signature(target = "numeric", dist = "vector"), definition = function(target, 
    dist, revDirec = FALSE, x = NULL, xval = NULL, condCutoff = TRUE, df = 0) {
    if (length(target) == 1) {
        if (is.null(x)) {
            if (revDirec) {
                return(mean(target >= dist, na.rm = TRUE))  # Appropriate for pValue of CFI
            } else {
                return(mean(target <= dist, na.rm = TRUE))  # Appropriate for pValue of RMSEA
            }
        } else {
            # Assume that the target value is appropriate for the specific 'xval' only.
            # Thus, the conditional quantile method is used.
            if (condCutoff) {
                return(pValueCondCutoff(target, dist, revDirec = revDirec, x = x, 
                  xval = xval, df = df))
            } else {
                return(pValueVariedCutoff(rep(target, length(dist)), dist, revDirec = revDirec, 
                  x = x, xval = xval))
            }
        }
    } else if (length(target) > 1) {
        return(pValueVariedCutoff(target, dist, revDirec = revDirec, x = x, xval = xval))
    } else {
        stop("Something is wrong!")
    }
})

setMethod("pValue", signature(target = "numeric", dist = "data.frame"), definition = function(target, 
    dist, revDirec = NULL, x = NULL, xval = NULL, df = 0, asLogical = FALSE) {
    if (length(target) != ncol(dist)) 
        stop("The length of target and the number of columns of dist are not equal")
    numVar <- length(target)
    if (is.null(revDirec)) {
        revDirec <- rep(FALSE, numVar)
    } else {
        if (length(revDirec) != numVar) 
            stop("The length of revDirec and the number of columns of dist are not equal")
    }
    if (asLogical) {
        result <- NULL
        for (i in 1:numVar) {
            if (revDirec[i]) {
                result <- cbind(result, target[i] >= dist[, i])
            } else {
                result <- cbind(result, target[i] <= dist[, i])
            }
        }
        return(result)
    } else {
        result <- rep(NA, numVar)
        for (i in 1:numVar) {
            result[i] <- pValue(target[i], dist[, i], revDirec[i], x = x, xval = xval, 
                df = df)
        }
        return(result)
    }
})

setMethod("pValue", signature(target = "lavaan", dist = "SimResult"), definition = function(target, 
    dist, usedFit = NULL, nVal = NULL, pmMCARval = NULL, pmMARval = NULL, df = 0) {
    dist <- clean(dist)
	usedFit <- cleanUsedFit(usedFit)
    revDirec <- (usedFit %in% getKeywords()$reversedFit)  # CFA --> FALSE, RMSEA --> TRUE
    
    if (is.null(nVal) || is.na(nVal)) 
        nVal <- NULL
    if (is.null(pmMCARval) || is.na(pmMCARval)) 
        pmMCARval <- NULL
    if (is.null(pmMARval) || is.na(pmMARval)) 
        pmMARval <- NULL
    Data <- as.data.frame(dist@fit[, usedFit])
    condition <- c(length(unique(dist@pmMCAR)) > 1, length(unique(dist@pmMAR)) > 
        1, length(unique(dist@n)) > 1)
    condValue <- cbind(dist@pmMCAR, dist@pmMAR, dist@n)
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
    cutoff <- inspect(target, "fit")[usedFit]
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
})

# pValueCondCutoff: Find the p-value comparing between a cutoff and a
# distribution when a cutoff is conditional on a predictor value

# \title{
# Find a p value when the target is conditional (valid) on a specific value of a predictor
# }
# \description{
# Find a \emph{p} value when the target is conditional (valid) on a specific value of a predictor. That is, the target value is applicable only a given value of a predictor.
# }
# \usage{
# pValueCondCutoff(target, dist, revDirec = FALSE, x = NULL, xval = NULL, df = 0)
# }
# \arguments{
  # \item{target}{
	# A target value used to find \code{p} values. 
# }
# \item{dist}{
	# The comparison distribution, which can be a vector of numbers, a data frame, or a result object.
# }
# \item{revDirec}{
	# A logical argument whether to reverse the direction of comparison. If \code{TRUE}, the proportion of the \code{dist} that is lower than \code{target} value is reported. If \code{FALSE}, the proportion of the \code{dist} that is higher than the \code{target} value is reported.
# }
  # \item{x}{
	# the \code{data.frame} of the predictor values. The number of rows of the \code{x} argument should be equal to the number of rows in the \code{dist}
# }
  # \item{xval}{
	# the values of predictor that researchers would like to find the fit indices cutoffs from.
# }
  # \item{df}{
	# the degree of freedom used in spline method in predicting the fit indices by the predictors. If \code{df} is 0, the spline method will not be applied. 
# }
# }
# \value{
	# A vector of \emph{p} values based on the comparison.
# }

pValueCondCutoff <- function(target, dist, revDirec = FALSE, x = NULL, xval = NULL, 
    df = 0) {
    if (!is.matrix(x)) 
        x <- as.matrix(x)
    p <- ncol(x)
    name <- paste("x", 1:p, sep = "")
    colnames(x) <- name
    names(xval) <- name
    if (df == 0) {
        name2 <- name
    } else {
        library(splines)
        name2 <- paste("ns(", name, ",", df, ")", sep = "")
    }
    firstord <- paste(name2, collapse = " + ")
    FUN <- function(x, y) paste(x, " * ", y, sep = "")
    secondord <- outer(name2, name2, FUN)[lower.tri(diag(length(name2)))]
    secondord <- paste(secondord, collapse = " + ")
    if (secondord == "") {
        express <- paste("y ~ ", firstord, sep = "")
    } else {
        express <- paste("y ~ ", firstord, " + ", secondord, sep = "")
    }
    dat <- data.frame(y = dist, x)
    library(quantreg)
    percVal <- 1:49/50
    mod <- rq(express, data = dat, tau = percVal)
    xval <- data.frame(t(as.matrix(xval)))
    colnames(xval) <- name2
    perc <- predict(mod, xval, interval = "none")
    perc <- whichMonotonic(perc, percVal)
    result <- interpolate(perc, target)
    if (!revDirec) {
        result <- revText(result)
    }
    return(result)
}

# revText: Reverse the direction of p value such as '> .98' to '< .02'

# \title{
	# Reverse the proportion value by subtracting it from 1
# }
# \description{
	# Reverse the proportion value by subtracting it from 1. This function can reverse a value reported in text, such as from "> .98" to "< .02"
# }
# \usage{
# revText(val)
# }
# \arguments{
  # \item{val}{
	# The value to be reversed
# }
# }
# \value{
	# The reversed value or text
# }

revText <- function(val) {
    if (suppressWarnings(is.na(as.numeric(val)))) {
        ntext <- nchar(val)
        comp <- substr(val, 1, 1)
        val <- as.numeric(substr(val, 3, nchar(val)))
        if (comp == ">") {
            comp <- "<"
        } else if (comp == "<") {
            comp <- ">"
        } else {
            stop("Something is wrong")
        }
        result <- paste(comp, 1 - val)
    } else {
        result <- 1 - as.numeric(val)
    }
    return(result)
}

# whichMonotonic: Find the center of a vector that is monotonically increasing
# or decreasing

# \title{
	# Extract a part of a vector that is monotonically increasing or decreasing
# }
# \description{
	# Extract a part of a vector that is monotonically increasing or decreasing. This function will go to the anchor value and extract the neighbor values that are monotonically increasing or decreasing.
# }
# \usage{
# whichMonotonic(vec, ord=NULL, anchor=NULL)
# }
# \arguments{
  # \item{vec}{
	# The target vector to be extracted
# }
  # \item{ord}{
	# The names of each element of the vector to be attached
# }
  # \item{anchor}{
	# The position of the element to be anchored. The default value is the middle position.
# }
# }
# \value{
	# The monotonic part of a vector
# }

whichMonotonic <- function(vec, ord = NULL, anchor = NULL) {
    vec <- as.vector(vec)
    p <- length(vec)
    if (is.null(anchor)) 
        anchor <- round((p/2) + 0.001)  # Add a small decimal so that 0.5 will be rounded as 1 instead of 0
    testDirection <- vec[2:p] > vec[1:(p - 1)]
    directionCenter <- vec[anchor + 1] > vec[anchor - 1]
    if (!directionCenter) 
        testDirection <- !testDirection
    notMonotone <- which(!testDirection)
    minchange <- 1
    maxchange <- p
    if (length(notMonotone) > 0) {
        lhs <- notMonotone < anchor
        if (length(which(lhs)) > 0) 
            minchange <- notMonotone[max(which(lhs))] + 1
        rhs <- notMonotone > anchor
        if (length(which(rhs)) > 0) 
            maxchange <- notMonotone[min(which(rhs))]
    }
    result <- vec[minchange:maxchange]
    if (is.null(ord)) {
        names(result) <- NULL
    } else {
        names(result) <- ord[minchange:maxchange]
    }
    return(result)
}

# interpolate: Find a specific percentile value by a linear interpolation of a
# closed value

# \title{
	# Find the value of one vector relative to a value of another vector by interpolation
# }
# \description{
	# Find the value of the resulting vector that have the position similar to the value of the baseline vector. If the starting value in the baseline vector is in between two elements, the resulting value will be predicted by linear interpolation.
# }
# \usage{
# interpolate(baselineVec, val, resultVec=NULL)
# }
# \arguments{
  # \item{baselineVec}{
	# The target vector to be used as a baseline. The resulting vector can be attached as the element names of this vector.
# }
  # \item{val}{
	# The value relative to the baseline vector to be used for projecting the resulting value
# }
  # \item{resultVec}{
	# The vector that the resulting value will be used to base their result form
# }
# }
# \value{
	# The interpolated value from the resulting vector relative to the value in the baseline vector
# }

interpolate <- function(baselineVec, val, resultVec = NULL) {
    p <- length(baselineVec)
    if (is.null(resultVec)) 
        resultVec <- names(baselineVec)
    lessThanVal <- which(baselineVec < val)
    if (length(lessThanVal) == 0) {
        return(paste("<", resultVec[1]))
    } else if (length(lessThanVal) == p) {
        return(paste(">", resultVec[p]))
    } else {
        floorIndex <- max(which(baselineVec < val))
        val1 <- baselineVec[floorIndex]
        val2 <- baselineVec[floorIndex + 1]
        perc1 <- as.numeric(resultVec[floorIndex])
        perc2 <- as.numeric(resultVec[floorIndex + 1])
        prop <- (val - val1)/(val2 - val1)
        targetPerc <- prop * (perc2 - perc1) + perc1
        names(targetPerc) <- NULL
        return(targetPerc)
    }
}

# pValueVariedCutoff: Find a value when the cutoffs are specified as a vector

# \title{
# Find a p value when the cutoff is specified as a vector given the values of predictors
# }
# \description{
# Find a \emph{p} value when the cutoff is specified as a vector given the values of predictors. 
# }
# \usage{
# pValueVariedCutoff(cutoff, obtainedValue, revDirec = FALSE, x = NULL, xval = NULL)
# }
# \arguments{
  # \item{cutoff}{
	# A vector of values used to find \code{p} values. Each value in the vector should be the target value conditional (applicable) to each value of the predictors (\code{x}) respectively.
# }
# \item{obtainedValue}{
	# The comparison distribution, which can be a vector of numbers, a data frame, or a result object.
# }
# \item{revDirec}{
	# A logical argument whether to reverse the direction of comparison. If \code{TRUE}, the proportion of the \code{dist} that is lower than \code{target} value is reported. If \code{FALSE}, the proportion of the \code{dist} that is higher than the \code{target} value is reported.
# }
  # \item{x}{
	# the \code{data.frame} of the predictor values. The number of rows of the \code{x} argument should be equal to the number of rows in the \code{dist}
# }
  # \item{xval}{
	# the values of predictor that researchers would like to find the fit indices cutoffs from.
# }
# }
# \value{
	# A vector of \emph{p} values based on the comparison.
# }

pValueVariedCutoff <- function(cutoff, obtainedValue, revDirec = FALSE, x = NULL, 
    xval = NULL) {
    # Change warning option to supress warnings
    warnT <- as.numeric(options("warn"))
    options(warn = -1)
    
    sig <- NULL
    if (revDirec) {
        sig <- cutoff >= obtainedValue  # sig for CFI
    } else {
        sig <- cutoff <= obtainedValue  # sig for RMSEA
    }
    if (is.null(x)) {
        result <- mean(sig, na.rm = TRUE)
    } else {
        x <- as.matrix(x)
        mod <- invisible(try(glm(sig ~ x, family = binomial(link = "logit")), silent = TRUE))
        result <- predProb(c(1, xval), mod)
    }
    ## Return warnings setting to user's settings
    options(warn = warnT)
    
    return(result)
}
 
