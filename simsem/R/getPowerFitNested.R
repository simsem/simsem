### Sunthud Pornprasertmanit
### Last updated: 6 March 2026
### Compute power of fit indices for nested model comparisons

#' Estimate Power of Fit Indices for Nested Model Comparisons
#'
#' Computes the statistical power of fit indices for nested model comparisons.
#' Power is defined as the probability that the difference in fit indices
#' between the nested and parent models exceeds a specified cutoff criterion.
#'
#' Cutoffs can either be provided directly or derived from simulation results
#' representing the null model.
#'
#' @param altNested A simulation result object representing the nested model
#' under the alternative hypothesis.
#' @param altParent A simulation result object representing the parent model
#' under the alternative hypothesis.
#' @param cutoff Optional named numeric vector specifying cutoff values for
#' each fit index.
#' @param nullNested Optional simulation result object representing the nested
#' model under the null hypothesis.
#' @param nullParent Optional simulation result object representing the parent
#' model under the null hypothesis.
#' @param revDirec Logical indicating whether the rejection direction of the
#' fit index should be reversed.
#' @param usedFit Character vector specifying which fit indices should be used.
#' @param alpha Significance level used when deriving cutoffs from null-model
#' simulations.
#' @param nVal Optional sample size value when sample size varies across
#' simulations.
#' @param pmMCARval Optional value specifying the proportion of missing
#' completely at random (MCAR).
#' @param pmMARval Optional value specifying the proportion of missing at
#' random (MAR).
#' @param condCutoff Logical indicating whether cutoffs should depend on
#' simulation conditions.
#' @param df Degrees of freedom used when estimating conditional cutoffs using
#' spline regression.
#'
#' @return A named vector containing estimated power values for each fit index.
#'
#' @seealso
#' \code{\link{getPowerFit}},
#' \code{\link{getCutoffNested}}
#'
#' @export
getPowerFitNested <- function(altNested, altParent, cutoff = NULL,
                              nullNested = NULL, nullParent = NULL,
                              revDirec = FALSE, usedFit = NULL,
                              alpha = 0.05, nVal = NULL,
                              pmMCARval = NULL, pmMARval = NULL,
                              condCutoff = TRUE, df = 0) {
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

#' Compute Power for Nested Models Given Fixed Fit-Index Cutoffs
#'
#' Internal helper used by \code{getPowerFitNested} when cutoff values are
#' supplied directly. Power is computed from the distribution of differences
#' in fit indices between nested and parent models.
#'
#' @param altNested Simulation result object for the nested model.
#' @param altParent Simulation result object for the parent model.
#' @param cutoff Named numeric vector specifying cutoff values for fit indices.
#' @param revDirec Logical indicating whether rejection direction is reversed.
#' @param usedFit Character vector specifying which fit indices should be used.
#' @param nVal Optional sample size value.
#' @param pmMCARval Optional MCAR missingness value.
#' @param pmMARval Optional MAR missingness value.
#' @param condCutoff Logical indicating whether cutoffs vary across conditions.
#' @param df Degrees of freedom used in spline-based conditional estimation.
#'
#' @return A named vector of power estimates.
#'
#' @keywords internal
getPowerFitNestedCutoff <- function(altNested, altParent, cutoff,
                                    revDirec = FALSE, usedFit = NULL,
                                    nVal = NULL, pmMCARval = NULL,
                                    pmMARval = NULL, condCutoff = TRUE,
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

#' Compute Power for Nested Models Using Null-Model Simulations
#'
#' Internal helper that computes power by deriving cutoff values from
#' simulated null-model results and applying them to alternative-model
#' simulations.
#'
#' @param altNested Simulation result object representing the nested model
#' under the alternative hypothesis.
#' @param altParent Simulation result object representing the parent model
#' under the alternative hypothesis.
#' @param nullNested Simulation result object representing the nested model
#' under the null hypothesis.
#' @param nullParent Simulation result object representing the parent model
#' under the null hypothesis.
#' @param revDirec Logical indicating whether rejection direction is reversed.
#' @param usedFit Character vector specifying which fit indices should be used.
#' @param alpha Significance level used when computing cutoffs.
#' @param nVal Optional sample size value.
#' @param pmMCARval Optional MCAR missingness value.
#' @param pmMARval Optional MAR missingness value.
#' @param df Degrees of freedom used in spline-based conditional cutoff
#' estimation.
#'
#' @return A named vector containing estimated power values for each fit index.
#'
#' @keywords internal
getPowerFitNestedNullObj <- function(altNested, altParent,
                                     nullNested, nullParent,
                                     revDirec = FALSE, usedFit = NULL,
                                     alpha = 0.05, nVal = NULL,
                                     pmMCARval = NULL, pmMARval = NULL,
                                     df = 0) {
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
        usedCutoff <- as.vector(t(getCutoffDataFrame(nullFit, alpha = alpha, usedFit = usedFit)))
        names(usedCutoff) <- usedFit
        temp <- pValueDataFrame(usedCutoff, usedDist, revDirec = usedDirec)
		names(temp) <- usedFit
		if(all(c("chisq", "df") %in% colnames(nullNested@fit))) {
			cutoffChisq <- qchisq(1 - alpha, df=(nullNested@fit - nullParent@fit)[,"df"])
			powerChi <- mean((altNested@fit - altParent@fit)[,"chisq"] > cutoffChisq)
			temp <- c("TraditionalChi" = powerChi, temp)
		}
    } else {
        varyingCutoff <- getCutoffDataFrame(object = nullFit, alpha = alpha, revDirec = FALSE, 
            usedFit = usedFit, predictor = condValue, df = df, predictorVal = "all")
        for (i in 1:length(temp)) {
            temp[i] <- pValueVariedCutoff(varyingCutoff[, i], usedDist[, i], revDirec = usedDirec[i], 
                x = condValue, xval = predictorVal)
        }
		names(temp) <- usedFit
    }
    return(temp)
}

#' Check Whether Multiple Objects Are Equal
#'
#' Determines whether all supplied objects are equal using
#' \code{all.equal()}.
#'
#' @param ... Objects to compare.
#'
#' @return Logical value indicating whether all objects are equal.
#'
#' @keywords internal
multipleAllEqual <- function(...) {
    obj <- list(...)
    multipleAllEqualList(obj)
}

#' Check Equality of Objects in a List
#'
#' Internal helper that checks whether all elements of a list are equal
#' using \code{all.equal()}.
#'
#' @param obj A list of objects.
#'
#' @return Logical value indicating whether all objects are equal.
#'
#' @keywords internal
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

#' Check Whether Any Objects Are Equal
#'
#' Determines whether any pair of supplied objects are equal using
#' \code{all.equal()}.
#'
#' @param ... Objects to compare.
#'
#' @return Logical value indicating whether any objects are equal.
#'
#' @keywords internal
multipleAnyEqual <- function(...) {
    obj <- list(...)
    multipleAnyEqualList(obj)
}

#' Check Whether Any Elements in a List Are Equal
#'
#' Internal helper that checks whether any pair of elements in a list
#' are equal using \code{all.equal()}.
#'
#' @param obj A list of objects.
#'
#' @return Logical value indicating whether any objects are equal.
#'
#' @keywords internal
multipleAnyEqualList <- function(obj) {
    for (i in 2:length(obj)) {
        for (j in 1:(i - 1)) {
            temp <- isTRUE(all.equal(obj[[i]], obj[[j]]))
            if (temp) 
                return(TRUE)
        }
    }
    return(FALSE)
} 
