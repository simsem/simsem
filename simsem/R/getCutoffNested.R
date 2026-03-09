### Sunthud Pornprasertmanit
### Last updated: 6 March 2026
### Get the cutoff from the simulated sampling distribution of difference in fit indices

#' Compute Cutoffs for Differences in Fit Indices Between Nested Models
#'
#' Computes cutoff values for differences in fit indices between a nested model
#' and its parent model using their simulated sampling distributions. The
#' function subtracts the fit indices of the parent model from those of the
#' nested model and determines quantile-based cutoffs corresponding to a
#' specified Type I error rate.
#'
#' The nested model must be estimated from the same simulated datasets as the
#' parent model, and both result objects must share identical simulation
#' conditions (e.g., parameter values, sample sizes, and missingness rates).
#'
#' @param nested A simulation result object for the nested model (the model with
#'   more degrees of freedom).
#' @param parent A simulation result object for the parent model (the model with
#'   fewer degrees of freedom).
#' @param alpha Numeric value representing the Type I error rate used to
#'   determine the cutoff. Default is \code{0.05}.
#' @param usedFit Character vector specifying which fit indices should be used.
#'   If \code{NULL}, all available fit indices are used.
#' @param nVal Optional numeric value specifying the sample size condition when
#'   multiple sample sizes were simulated.
#' @param pmMCARval Optional numeric value specifying the proportion of missing
#'   completely at random (MCAR) when multiple MCAR conditions were simulated.
#' @param pmMARval Optional numeric value specifying the proportion of missing
#'   at random (MAR) when multiple MAR conditions were simulated.
#' @param df Degrees of freedom used when estimating conditional quantiles using
#'   spline regression. Default is \code{0}.
#'
#' @return A data frame containing cutoff values for the differences in fit
#'   indices between the nested and parent models.
#'
#' @details
#' The difference in fit indices is computed as
#'
#' \deqn{\Delta Fit = Fit_{nested} - Fit_{parent}}
#'
#' Cutoffs are then determined from the simulated sampling distribution of these
#' differences using quantiles corresponding to the specified \code{alpha}.
#'
#' @seealso \code{\link{getCutoffDataFrame}}, \code{\link{getCutoff}}
#'
#' @keywords simulation
#'
#' @export
getCutoffNested <- function(nested, parent, alpha = 0.05, usedFit = NULL, nVal = NULL, 
    pmMCARval = NULL, pmMARval = NULL, df = 0) {
    mod <- clean(nested, parent)
    nested <- mod[[1]]
    parent <- mod[[2]]
    if (!isTRUE(all.equal(unique(nested@paramValue), unique(parent@paramValue)))) 
        stop("Models are based on different data and cannot be compared, check your random seed")
    if (!isTRUE(all.equal(unique(nested@n), unique(parent@n)))) 
        stop("Models are based on different values of sample sizes")
    if (!isTRUE(all.equal(unique(nested@pmMCAR), unique(parent@pmMCAR)))) 
        stop("Models are based on different values of the percent completely missing at random")
    if (!isTRUE(all.equal(unique(nested@pmMAR), unique(parent@pmMAR)))) 
        stop("Models are based on different values of the percent missing at random")
    if (is.null(nVal) || is.na(nVal)) 
        nVal <- NULL
    if (is.null(pmMCARval) || is.na(pmMCARval)) 
        pmMCARval <- NULL
    if (is.null(pmMARval) || is.na(pmMARval)) 
        pmMARval <- NULL
    Data <- as.data.frame((nested@fit - parent@fit))
    condition <- c(length(unique(nested@pmMCAR)) > 1, length(unique(nested@pmMAR)) > 
        1, length(unique(nested@n)) > 1)
    condValue <- cbind(nested@pmMCAR, nested@pmMAR, nested@n)
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
    output <- getCutoffDataFrame(Data, alpha, FALSE, usedFit, predictor = condValue, predictorVal = predictorVal, df = df)
    return(output)
} 
