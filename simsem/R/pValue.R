### Sunthud Pornprasertmanit & Terrence D. Jorgensen
### Last updated: 6 March 2026
### Fit index p-value utilities used in dynamic cutoff evaluation

#' Compute p-values for a single fit index
#'
#' Internal helper used by \code{\link{pValue}} to compute a p-value
#' comparing a target statistic against a simulated distribution.
#'
#' @param target Target value used to compute the p-value.
#' @param dist Simulated distribution of the statistic.
#' @param revDirec Logical indicating whether the comparison direction
#' should be reversed.
#' @param x Predictor matrix for conditional p-value estimation.
#' @param xval Specific predictor value for conditional estimation.
#' @param condCutoff Logical indicating whether conditional cutoffs are used.
#' @param df Degrees of freedom used for spline modeling.
#'
#' @return A p-value.
#'
#' @keywords internal
pValueVector <- function(target,
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
}

#' Compute p-values for multiple fit indices
#'
#' Internal helper function used to compute p-values for multiple
#' fit indices simultaneously.
#'
#' @param target Vector of target values.
#' @param dist Data frame containing simulated distributions.
#' @param revDirec Logical vector indicating whether comparisons
#' should be reversed.
#' @param x Predictor values used for conditional estimation.
#' @param xval Predictor values corresponding to the target.
#' @param df Degrees of freedom for spline estimation.
#' @param asLogical Logical indicating whether logical comparisons
#' should be returned instead of probabilities.
#'
#' @return A vector of p-values or a logical matrix.
#'
#' @keywords internal
pValueDataFrame <- function(target,
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
            result[i] <- pValueVector(target[i], dist[, i], revDirec[i], x = x, xval = xval,
                df = df)
        }
        return(result)
    }
}

#' Compute p-values for fit indices based on a simulated distribution
#'
#' Compute \emph{p}-values for model fit indices by comparing a target model
#' against the empirical distribution of fit indices obtained from a
#' simulation result object.
#'
#' The function extracts fit indices from a model object (lavaan or OpenMx)
#' and compares them with the distribution of fit indices stored in a
#' \code{SimResult} object. When simulation conditions vary (e.g., sample size
#' or missing data proportions), conditional comparisons can be performed.
#'
#' @param target A fitted model object (e.g., a \code{lavaan}, \code{lavaan.mi},
#' or \code{MxModel} object) whose fit indices will be evaluated.
#' @param dist A \code{SimResult} object containing the simulated distribution
#' of fit indices.
#' @param usedFit Character vector specifying which fit indices should be used.
#' @param nVal Sample size value used when the simulation includes varying
#' sample sizes.
#' @param pmMCARval Percent missing completely at random used when simulations
#' vary in MCAR levels.
#' @param pmMARval Percent missing at random used when simulations vary in MAR
#' levels.
#' @param df Degrees of freedom used in spline-based conditional estimation.
#'
#' @return
#' A named vector of \emph{p}-values corresponding to the requested fit indices.
#' When conditions do not vary, additional summary statistics are returned:
#'
#' \itemize{
#' \item individual fit index probabilities
#' \item \code{andRule}: proportion of replications satisfying all criteria
#' \item \code{orRule}: proportion satisfying at least one criterion
#' }
#'
#' @examples
#' \dontrun{
#' fit <- lavaan::cfa(model, data)
#' pValue(fit, simResult)
#' }
#'
#' @export
pValue <- function(target,
    dist, usedFit = NULL, nVal = NULL, pmMCARval = NULL, pmMARval = NULL, df = 0) {
	if(!is(dist, "SimResult")) stop("The 'dist' object must be a result object.")

    dist <- clean(dist)
	usedFit <- cleanUsedFit(usedFit, colnames(dist@fit))
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
	if (is(target, "lavaan")) {
		cutoff <- lavaan::fitMeasures(target, fit.measures = usedFit)
	} else if (is(target, "lavaan.mi")) {
	  cutoff <- getMethod("fitMeasures", "lavaan.mi")(target, fit.measures = usedFit)
	} else if (is(target, "MxModel")) {
		cutoff <- fitMeasuresMx(target)[usedFit]
	} else {
		stop("The target argument must be a lavaan object or MxModel object.")
	}
    if (any(condition)) {
        result <- pValueDataFrame(cutoff, Data, revDirec, x = condValue, xval = predictorVal,
            df = df, asLogical = FALSE)
        names(result) <- usedFit
        return(result)
    } else {
        logicalMat <- pValueDataFrame(cutoff, Data, revDirec, asLogical = TRUE)
        result <- apply(logicalMat, 2, mean, na.rm = TRUE)
        names(result) <- usedFit
        andRule <- mean(apply(logicalMat, 1, all), na.rm = TRUE)
        orRule <- mean(apply(logicalMat, 1, any), na.rm = TRUE)
        return(c(result, andRule = andRule, orRule = orRule))
    }
}

#' Find a p value when the target is conditional on predictor values
#'
#' Find a \emph{p} value when the target value is conditional (valid)
#' only for a specific value of a predictor.
#'
#' That is, the target value is applicable only at a given value of a
#' predictor, and the p-value is computed relative to that conditional
#' distribution.
#'
#' @param target A target value used to find \code{p} values.
#' @param dist The comparison distribution.
#' @param revDirec Logical indicating whether to reverse the direction
#' of comparison.
#' @param x Data frame of predictor values. The number of rows must match
#' the number of rows in \code{dist}.
#' @param xval Predictor value at which the conditional p-value is evaluated.
#' @param df Degrees of freedom used in spline modeling. If \code{df = 0},
#' splines are not used.
#'
#' @return A vector of \emph{p}-values based on the comparison.
#'
#' @keywords internal
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
        requireNamespace("splines")
		if(!("package:splines" %in% search())) attachNamespace("splines")
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
    percVal <- 1:49/50
	requireNamespace("quantreg")
	if(!("package:quantreg" %in% search())) attachNamespace("quantreg")
    mod <- quantreg::rq(express, data = dat, tau = percVal)
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

#' Reverse the direction of a proportion value
#'
#' Reverse the proportion value by subtracting it from 1.
#' This function can also reverse text representations such as
#' \code{"> .98"} to \code{"< .02"}.
#'
#' @param val Value or character representation of a proportion.
#'
#' @return The reversed value or text.
#'
#' @keywords internal
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

#' Extract the monotonic portion of a vector
#'
#' Extract the portion of a vector that is monotonically increasing
#' or decreasing. The function identifies an anchor point and retains
#' neighboring elements that follow a consistent monotonic direction.
#'
#' @param vec Target vector.
#' @param ord Optional names associated with vector elements.
#' @param anchor Position of the anchor element.
#'
#' @return A monotonic subset of the vector.
#'
#' @keywords internal
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

#' Interpolate values relative to a baseline vector
#'
#' Find the value of one vector relative to a value in another vector
#' using linear interpolation.
#'
#' If the target value lies between two baseline values, the resulting
#' value is estimated by linear interpolation.
#'
#' @param baselineVec Baseline vector used for interpolation.
#' @param val Value relative to the baseline vector.
#' @param resultVec Vector from which resulting values are taken.
#'
#' @return Interpolated value relative to the baseline vector.
#'
#' @keywords internal
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

#' Compute p values for varying cutoff values
#'
#' Compute a \emph{p} value when cutoff values vary across simulation
#' conditions, typically as a function of predictor variables.
#'
#' @param cutoff Vector of cutoff values.
#' @param obtainedValue Comparison distribution.
#' @param revDirec Logical indicating whether the comparison direction
#' should be reversed.
#' @param x Predictor matrix.
#' @param xval Predictor value at which the p-value is evaluated.
#'
#' @return A vector of \emph{p}-values.
#'
#' @keywords internal
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
        try(mod <- glm(sig ~ x, family = binomial(link = "logit")), silent = TRUE)
        result <- predProb(c(1, xval), mod)[2]
    }
    ## Return warnings setting to user's settings
    options(warn = warnT)

    return(result)
}

