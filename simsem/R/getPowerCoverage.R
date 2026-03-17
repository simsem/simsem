### Sunthud Pornprasertmanit & Terry D. Jorgensen
### Last updated: 6 March 2026
### Automatically find the power for all values in the range of given varying parameters or for a set of given value of varying parameters


#' Estimate Statistical Power from Simulation Results
#'
#' Computes statistical power from simulation results. If simulation
#' conditions vary (e.g., sample size, missingness, or parameter values),
#' power is estimated as a continuous function of the varying parameters
#' using logistic regression.
#'
#' If no simulation conditions vary, power is calculated as the proportion
#' of replications in which the null hypothesis is rejected.
#'
#' @param simResult A simulation result object.
#' @param alpha Significance level used for hypothesis testing. Default is
#'   \code{0.05}.
#' @param contParam Character vector specifying parameters that vary
#'   continuously in the simulation.
#' @param powerParam Character vector specifying parameters for which power
#'   should be calculated.
#' @param nVal Optional numeric value specifying the sample size when sample
#'   size varies across simulations.
#' @param pmMCARval Optional numeric value specifying the proportion of
#'   missing completely at random (MCAR).
#' @param pmMARval Optional numeric value specifying the proportion of
#'   missing at random (MAR).
#' @param paramVal Optional vector or list specifying values of varying
#'   parameters at which power should be evaluated.
#'
#' @return A vector or data frame containing estimated power values.
#'
#' @export
getPower <- function(simResult, alpha = 0.05, contParam = NULL, powerParam = NULL,
    nVal = NULL, pmMCARval = NULL, pmMARval = NULL, paramVal = NULL) {
    object <- clean(simResult)
    condition <- c(length(unique(object@pmMCAR)) > 1, length(unique(object@pmMAR)) >
        1, length(unique(object@n)) > 1)
    if (any(condition)) {
        pred <- getPred(contParam, nVal, pmMCARval, pmMARval, paramVal)
        pow <- continuousPower(object, length(unique(object@n)) > 1, length(unique(object@pmMCAR)) >
            1, length(unique(object@pmMAR)) > 1, contParam = contParam, alpha = alpha,
            powerParam = powerParam, pred = pred)
        return(pow)
    } else {
        z <- object@coef/object@se
        crit.value <- qnorm(1 - alpha/2)
        sig <- abs(z) > crit.value
        pow <- apply(sig, 2, mean, na.rm = TRUE)
        return(pow)
    }
}

#' Estimate Confidence Interval Coverage from Simulation Results
#'
#' Computes coverage rates of confidence intervals from simulation results.
#' If simulation conditions vary, coverage is estimated as a function of the
#' varying parameters using logistic regression.
#'
#' If no simulation conditions vary, coverage is calculated as the proportion
#' of replications in which the confidence interval contains the target value.
#'
#' @param simResult A simulation result object.
#' @param coverValue Optional value used to assess coverage. If \code{NULL},
#'   the true parameter values stored in the simulation object are used.
#' @param contParam Character vector specifying parameters that vary
#'   continuously in the simulation.
#' @param coverParam Character vector specifying parameters for which
#'   coverage should be calculated.
#' @param nVal Optional numeric value specifying the sample size when sample
#'   size varies across simulations.
#' @param pmMCARval Optional numeric value specifying the proportion of
#'   missing completely at random (MCAR).
#' @param pmMARval Optional numeric value specifying the proportion of
#'   missing at random (MAR).
#' @param paramVal Optional vector or list specifying values of varying
#'   parameters at which coverage should be evaluated.
#'
#' @return A vector or data frame containing estimated coverage rates.
#'
#' @export
getCoverage <- function(simResult, coverValue = NULL, contParam = NULL, coverParam = NULL,
    nVal = NULL, pmMCARval = NULL, pmMARval = NULL, paramVal = NULL) {
    object <- clean(simResult)
    condition <- c(length(unique(object@pmMCAR)) > 1, length(unique(object@pmMAR)) >
        1, length(unique(object@n)) > 1)
    if (any(condition)) {
        pred <- getPred(contParam, nVal, pmMCARval, pmMARval, paramVal)
        pow <- continuousCoverage(object, coverValue = coverValue, contN = length(unique(object@n)) > 1, contMCAR = length(unique(object@pmMCAR)) >
            1, contMAR = length(unique(object@pmMAR)) > 1, contParam = contParam,
            coverParam = coverParam, pred = pred)
        return(pow)
    } else {
        cover <- calcCoverMatrix(object, coverValue = coverValue)
        coverrate <- apply(cover, 2, mean, na.rm = TRUE)
        return(coverrate)
    }
}

#' Construct Predictor Values for Continuous Simulation Conditions
#'
#' Creates a list of predictor values used for evaluating power or coverage
#' when simulation conditions vary continuously.
#'
#' @param contParam Character vector specifying parameters that vary.
#' @param nVal Sample size value.
#' @param pmMCARval Proportion of MCAR missingness.
#' @param pmMARval Proportion of MAR missingness.
#' @param paramVal Values of continuously varying parameters.
#'
#' @return A list containing predictor values.
#'
#' @keywords internal
getPred <- function(contParam = NULL, nVal = NULL, pmMCARval = NULL,
                    pmMARval = NULL, paramVal = NULL) {
	pred <- NULL
	pred$N <- nVal
	pred$MCAR <- pmMCARval
	pred$MAR <- pmMARval
	if (!is.null(paramVal)) {
		if (is.list(paramVal)) {
			if (is.null(names(paramVal)))
			  names(paramVal) <- contParam
			pred <- c(pred, paramVal)
		} else if (is.vector(paramVal)) {
			if (length(contParam) == 1) {
			  temp <- list(paramVal)
			  names(temp) <- contParam
			  pred <- c(pred, temp)
			} else {
			  temp <- as.list(paramVal)
			  names(temp) <- contParam
			  pred <- c(pred, temp)
			}
		}
	}
	pred
}

#' Compute Power for Continuously Varying Simulation Conditions
#'
#' Estimates statistical power when simulation conditions vary continuously.
#' Power is estimated using logistic regression applied to indicators of
#' statistical significance across replications.
#'
#' @param simResult A simulation result object.
#' @param contN Logical indicating whether sample size varies.
#' @param contMCAR Logical indicating whether MCAR proportion varies.
#' @param contMAR Logical indicating whether MAR proportion varies.
#' @param contParam Character vector specifying continuously varying
#'   parameters.
#' @param alpha Significance level used to determine rejection.
#' @param powerParam Parameters for which power should be calculated.
#' @param pred Predictor values used for evaluating power.
#'
#' @return A data frame of predicted power values.
#'
#' @keywords internal
continuousPower <- function(simResult, contN = TRUE, contMCAR = FALSE,
                            contMAR = FALSE, contParam = NULL, alpha = 0.05,
                            powerParam = NULL, pred = NULL) {
	object <- clean(simResult)
    crit.value <- qnorm(1 - alpha/2)
    sig <- 0 + (abs(object@coef/object@se) > crit.value)
	continuousLogical(object, logical = sig, contN = contN, contMCAR = contMCAR,
	                  contMAR = contMAR, contParam = contParam,
	                  logicalParam = powerParam, pred = pred)
}

#' Compute Coverage for Continuously Varying Simulation Conditions
#'
#' Estimates confidence interval coverage when simulation conditions vary
#' continuously using logistic regression.
#'
#' @param simResult A simulation result object.
#' @param coverValue Target value used to determine coverage.
#' @param contN Logical indicating whether sample size varies.
#' @param contMCAR Logical indicating whether MCAR proportion varies.
#' @param contMAR Logical indicating whether MAR proportion varies.
#' @param contParam Character vector specifying continuously varying
#'   parameters.
#' @param coverParam Parameters for which coverage should be calculated.
#' @param pred Predictor values used for evaluating coverage.
#'
#' @return A data frame of predicted coverage rates.
#'
#' @keywords internal
continuousCoverage <- function(simResult, coverValue = NULL, contN = TRUE,
                               contMCAR = FALSE, contMAR = FALSE,
                               contParam = NULL, coverParam = NULL, pred = NULL) {
	object <- clean(simResult)
	cover <- calcCoverMatrix(object, coverValue = coverValue)
	continuousLogical(object, logical = cover, contN = contN, contMCAR = contMCAR,
	                  contMAR = contMAR, contParam = contParam,
	                  logicalParam = coverParam, pred = pred)
}

#' Compute Coverage Indicator Matrix
#'
#' Determines whether confidence intervals contain the target parameter
#' values for each replication.
#'
#' @param object A simulation result object.
#' @param coverValue Optional target value used to evaluate coverage.
#'
#' @return A logical matrix indicating whether each confidence interval
#' contains the target value.
#'
#' @keywords internal
calcCoverMatrix <- function(object, coverValue = NULL) {
	lowerBound <- object@cilower
	upperBound <- object@ciupper
	if(is.null(coverValue)) {
		paramValue <- object@paramValue
		usedParam <- intersect(colnames(lowerBound), colnames(paramValue)) # colnames of lower and upper bounds are the same
		lowerBound <- lowerBound[,usedParam]
		upperBound <- upperBound[,usedParam]
		paramValue <- paramValue[,usedParam]
		if(nrow(paramValue) == 1) {
			paramValue <- matrix(rep(paramValue, each = nrow(lowerBound)), nrow(lowerBound))
			colnames(paramValue) <- usedParam
		}
		cover <- (paramValue > as.matrix(lowerBound)) & (paramValue < as.matrix(upperBound))
	} else {
		cover <- (coverValue > as.matrix(lowerBound)) & (coverValue < as.matrix(upperBound))
	}
	cover
}

#' Logistic Regression for Continuous Simulation Outcomes
#'
#' Fits logistic regression models to estimate probabilities of logical
#' outcomes (e.g., power or coverage) when simulation conditions vary
#' continuously.
#'
#' @param object A simulation result object.
#' @param logical Logical matrix indicating outcomes across replications.
#' @param contN Logical indicating whether sample size varies.
#' @param contMCAR Logical indicating whether MCAR proportion varies.
#' @param contMAR Logical indicating whether MAR proportion varies.
#' @param contParam Character vector specifying continuously varying
#'   parameters.
#' @param logicalParam Parameters for which probabilities should be computed.
#' @param pred Predictor values used for prediction.
#'
#' @return A data frame containing predicted probabilities.
#'
#' @keywords internal
continuousLogical <- function(object, logical, contN = TRUE, contMCAR = FALSE,
                              contMAR = FALSE, contParam = NULL,
                              logicalParam = NULL, pred = NULL) {

    # Change warning option to supress warnings
    warnT <- as.numeric(options("warn"))
    options(warn = -1)

    # Clean simResult object and get a replications by parameters matrix of 0s and
    # 1s for logistic regression
    nrep <- dim(logical)[[1]]

    # Find paramaterss to get power for
    if (!is.null(logicalParam)) {
        j <- match(logicalParam, dimnames(logical)[[2]])  # Return column indices that start with 'param'
        logical <- data.frame(logical[, j, drop = FALSE])
    }

    # Create matrix of predictors (randomly varying params)
    x <- NULL
    predDefault <- is.null(pred)

    if (contN) {
        if (!length(object@n) == nrep) {
            stop("Number of random sample sizes is not the same as the number of replications, check to see if N varied across replications")
        }
        x <- cbind(x, object@n)
        if (predDefault)
            pred$N <- min(object@n):max(object@n)
    }
    if (contMCAR) {
        if (!length(object@pmMCAR) == nrep) {
            stop("Number of random pmMCARs is not the same as the number of replications, check to see if pmMCAR varied across replications")
        }
        x <- cbind(x, object@pmMCAR)
        if (predDefault)
            pred$MCAR <- seq(min(object@pmMCAR), max(object@pmMCAR), by = 0.01)

    }
    if (contMAR) {
        if (!length(object@pmMAR) == nrep) {
            stop("Number of random pmMARs is not the same as the number of replications, check to see if pmMAR varied across replications")
        }
        x <- cbind(x, object@pmMAR)
        if (predDefault)
            pred$MAR <- seq(min(object@pmMAR), max(object@pmMAR), by = 0.01)

    }
    if (!is.null(contParam)) {
        if (!(dim(object@paramValue)[[1]] == nrep)) {
            stop("Number of random parameters is not the same as the number of replications, check to see if parameters varied across replications")
        }
        j <- match(contParam, names(object@paramValue))  # Return column indices that start with 'contParam'
        x <- cbind(x, as.matrix(object@paramValue[, j, drop = FALSE])) #TDJ fix on 22-6-2021
        if (predDefault) {
            paramVal <- list()
            for (i in 1:length(contParam)) {
                temp <- seq(from = min(object@paramValue[, contParam[i]]),
                            to = max(object@paramValue[, contParam[i]]),
                            length.out = 5)
                paramVal[[i]] <- unique(temp)
            }
            names(paramVal) <- contParam
            pred <- c(pred, paramVal)
        }
    }

    res <- NULL
    powVal <- data.frame(expand.grid(pred))
    powVal <- cbind(rep(1, dim(powVal)[1]), powVal)
    x <- as.matrix(x)
    for (i in 1:dim(logical)[[2]]) {
        try(mod <- glm(logical[, i] ~ x, family = binomial(link = "logit")), silent = TRUE)
        res[[dimnames(logical)[[2]][[i]]]] <- apply(powVal, 1, function(x) predProb(x, mod)[2])
    }
    if (is.list(res)) {
        res <- do.call(cbind, res)
    } else {
        res <- t(as.matrix(res))
    }
    names(res) <- names(logical)
    colnames(powVal) <- paste("iv.", colnames(powVal), sep = "")
    pow <- cbind(powVal[, -1], res)
    colnames(pow) <- c(colnames(powVal)[-1], colnames(res))


    ## Return warnings setting to user's settings
    options(warn = warnT)

    return(pow)
}

#' Predicted Probability from Logistic Regression
#'
#' Computes predicted probabilities and confidence bounds from a fitted
#' logistic regression model.
#'
#' @param newdat Numeric vector containing predictor values including the
#'   intercept.
#' @param glmObj A fitted \code{glm} object with a logit link.
#' @param alpha Significance level used to compute confidence bounds.
#'
#' @return A numeric vector containing the lower bound, predicted
#' probability, and upper bound.
#'
#' @keywords internal
predProb <- function(newdat, glmObj, alpha = 0.05) {
    slps <- as.numeric(coef(glmObj))
    logi <- sum(newdat * slps)
	predVal <- as.matrix(newdat)
	se <- sqrt(t(predVal) %*% vcov(glmObj) %*% predVal)
	critVal <- qnorm(1 - alpha/2)
	logi <- c(logi - critVal * se, logi, logi + critVal * se)
	logi[logi > 500] <- 500
	logi[logi < -500] <- -500
    pp <- exp(logi)/(1 + exp(logi))
	if(round(pp[2], 6) == 1) pp[3] <- 1
	if(round(pp[2], 6) == 0) pp[1] <- 0
    return(pp)
}

