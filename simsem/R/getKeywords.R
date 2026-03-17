### Sunthud Pornprasertmanit
### Last updated: 6 March 2026
### Internal keywords used across the simsem package

#' Internal Keyword Definitions for simsem
#'
#' Provides a centralized list of keywords used internally across the
#' \code{simsem} package. These keywords define default fit indices,
#' optimization options, and identifiers used in various utility functions.
#'
#' @return A list containing keyword groups:
#' \describe{
#'   \item{usedFit}{Default fit indices returned by simulation summaries.}
#'   \item{usedFitScaled}{Scaled versions of fit indices when available.}
#'   \item{reversedFit}{Fit indices where larger values indicate better fit
#'   and therefore require reversed cutoff logic.}
#'   \item{usedFitPop}{Population fit indices used as input in simulation
#'   generation.}
#'   \item{optMin}{Keywords specifying minimum misfit selection methods.}
#'   \item{optMax}{Keywords specifying maximum misfit selection methods.}
#'   \item{optNone}{Keywords indicating that no optimization method is used.}
#' }
#'
#' @keywords internal
getKeywords <- function() {
  usedFit <- c("chisq", "aic", "bic", "rmsea", "cfi", "tli", "srmr")
  usedFitScaled <- c("chisq.scaled", "aic", "bic", "rmsea.scaled", "cfi.scaled", "tli.scaled", "srmr")
  reversedFit <- c("cfi", "tli", "nnfi", "nfi", "rfi", "ifi", "gfi", "agfi", "pnfi", "pgfi", "mfi", "rni")
  usedFitPop <- c("f0", "rmsea", "srmr")
  optMin <- c("min", "minimum", "lower")
  optMax <- c("max", "maximum", "upper")
  optNone <- c("none", "null")

  result <- list(
    usedFit = usedFit, usedFitScaled = usedFitScaled, reversedFit = reversedFit, usedFitPop = usedFitPop, optMin = optMin, optMax = optMax,
    optNone = optNone
  )
  return(result)
}
