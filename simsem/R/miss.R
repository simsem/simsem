### Sunthud Pornprasertmanit & Terrence D. Jorgensen
### Last updated: 6 March 2026
### Constructor for specifying missing-data mechanisms in simsem

#' Specify Missing-Data Mechanisms for Simulation
#'
#' Creates a \code{SimMissing} object describing how missing data should be
#' imposed in simulated datasets. The object stores parameters controlling
#' missing-data mechanisms such as MCAR, MAR, planned missing designs, and
#' auxiliary-variable handling.
#'
#' The resulting object is used internally by simulation functions to generate
#' missing data patterns before model estimation.
#'
#' @param cov Numeric value indicating the proportion of covariates with missing data.
#' @param pmMCAR Proportion of data missing completely at random (MCAR).
#' @param pmMAR Proportion of data missing at random (MAR).
#' @param logit Character string specifying a logistic model used to generate
#'   missingness probabilities.
#' @param nforms Number of planned missing forms.
#' @param itemGroups List defining groups of items used in planned missing designs.
#' @param timePoints Number of measurement occasions in longitudinal designs.
#' @param twoMethod Proportion of variables using a two-method measurement design.
#' @param prAttr Proportion of attributes used for generating missingness.
#' @param m Number of imputations (currently unused internally).
#' @param convergentCutoff Numeric cutoff used for assessing convergence when
#'   imputation is performed.
#' @param ignoreCols Columns to ignore when imposing missingness.
#' @param threshold Threshold controlling missing-data generation.
#' @param covAsAux Logical indicating whether covariates should be treated as
#'   auxiliary variables during estimation.
#' @param logical Optional logical matrix specifying a fixed missing-data pattern.
#' @param ... Additional arguments passed internally to imputation procedures.
#'
#' @return An object of class \code{SimMissing} describing the missing-data
#'   mechanism used in simulations.
#'
#' @details
#' This function defines missing-data mechanisms used during simulation.
#' Currently, the default estimation method assumes full information maximum
#' likelihood (FIML) through \code{lavaan}.
#'
#' Planned missing-data designs and logistic missingness mechanisms can be
#' specified using the corresponding arguments.
#' 
#' @seealso imposeMissing
#'
#' @export
miss <- function(cov = 0, pmMCAR = 0, pmMAR = 0, logit = "", nforms = 0,
                 itemGroups = list(), timePoints = 1, twoMethod = 0, prAttr = 0,
                 m = 0, convergentCutoff = 0.8, # package = "default",
                 ignoreCols = 0, threshold = 0,
                 covAsAux = TRUE, logical = NULL, ...) {

#TDJ 2 April 2025: Deprecated option to impute within analyze()
#TODO:  Use ... to pass arguments to lavaan.mi() --> lavaanList()
#       VIGNETTE IDEA:
#       use more store.slots= or FUN= to enable extra output for sim(outfun=)

  args <- list(...)

  if (!is.null(logical) && is.data.frame(logical)) logical <- as.matrix(logical)
  if (!is.null(logical)) {
      stopifnot(is.logical(logical))
      stopifnot(is.matrix(logical))
  } else {
      logical <- as.matrix(FALSE)
  }
  ##FIXME: without runMI(), no sustainable way to automate imputation
#   if (!(package %in% c("default", "Amelia", "mice"))) {
#       stop("Only \"default\", \"mice\", or \"Amelia\" are accepted as arguments to package.")
#   }
# 	if (package %in% c("Amelia", "mice")) {
# 		if (!(m > 0)) stop("Because the Amelia or mice packages are specified,",
# 		                   " the m argument should be specified.")
# 	}
# 	if (package == "default") {
# 		if (m > 0) package <- "mice"
# 	}
  return(new("SimMissing", cov = cov, pmMCAR = pmMCAR, pmMAR = pmMAR,
             logit = logit, nforms = nforms, itemGroups = itemGroups,
             twoMethod = twoMethod, prAttr = prAttr, timePoints = timePoints,
             threshold = threshold, ignoreCols = ignoreCols,
             covAsAux = covAsAux, logical = logical,
             #FIXME: reinstate m > 0, but remove package= from SimMissing-class
             m = 0, package = "default", convergentCutoff = convergentCutoff,
             args = args))
}
