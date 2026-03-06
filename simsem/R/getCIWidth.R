### Sunthud Pornprasertmanit
### Last updated: 6 March 2026
### Compute confidence interval widths given sample size or missingness rates

#' Compute Confidence Interval Widths from Simulation Results
#'
#' Extracts the widths of confidence intervals (CI) from a simulation object and
#' summarizes them based on a specified assurance level. The CI width is defined
#' as the difference between the upper and lower confidence bounds for each
#' parameter. The function returns cutoff values corresponding to the specified
#' assurance probability.
#'
#' @param object A simulation result object containing the elements
#'   \code{ciupper} and \code{cilower}.
#' @param assurance Numeric value indicating the assurance level (i.e., the
#'   probability that the CI width will not exceed the returned cutoff). Default
#'   is \code{0.50}.
#' @param nVal Optional numeric value specifying the sample size condition to
#'   filter results.
#' @param pmMCARval Optional numeric value specifying the proportion of MCAR
#'   missingness used to filter results.
#' @param pmMARval Optional numeric value specifying the proportion of MAR
#'   missingness used to filter results.
#' @param df Degrees of freedom used when computing cutoff values. Default is
#'   \code{0}.
#'
#' @return A data frame containing CI width cutoff values for each parameter
#'   under the specified assurance level.
#'
#' @details
#' The CI width is computed as
#' \deqn{CI_{width} = CI_{upper} - CI_{lower}.}
#'
#' The function filters simulation results according to the specified
#' condition values and computes quantile-based cutoff values using
#' \code{getCutoffDataFrame()}.
#'
#' @seealso \code{\link{getCutoffDataFrame}}, \code{\link{clean}}
#'
#' @keywords simulation confidence-interval
#'
#' @export
getCIwidth <- function(object, assurance = 0.50, nVal = NULL, pmMCARval = NULL, pmMARval = NULL, df = 0) {
    if (is.null(nVal) || is.na(nVal)) 
        nVal <- NULL
    if (is.null(pmMCARval) || is.na(pmMCARval)) 
        pmMCARval <- NULL
    if (is.null(pmMARval) || is.na(pmMARval)) 
        pmMARval <- NULL
    object <- clean(object)
    Data <- as.data.frame(object@ciupper - object@cilower)
    condValuePredictorVal <- getCondValuePredictorVal(object, nVal, pmMCARval, pmMARval)
    
    output <- getCutoffDataFrame(Data, alpha = 1 - assurance, revDirec = FALSE, usedFit = colnames(Data), predictor = condValuePredictorVal[[1]], predictorVal = condValuePredictorVal[[2]], df = df)
	names(output) <- colnames(Data)
    return(output)
}
