### Sunthud Pornprasertmanit
### Last updated: 5 March 2026
### constructor for simulation data distribution objects

#' Create a data distribution specification
#'
#' Constructs a \code{SimDataDist} object describing the marginal
#' distributions and dependence structure used to generate simulated data.
#'
#' The function allows users to specify marginal distributions, distribution
#' parameters, and optional skewness or kurtosis values for each variable.
#' A copula may also be supplied to define the multivariate dependence
#' structure.
#'
#' @param margins Character vector specifying the marginal distribution
#'   type for each variable.
#' @param ... Optional list of parameter specifications for the marginal
#'   distributions. Each element corresponds to a variable and typically
#'   contains a list of named parameters required by the distribution.
#' @param p Integer specifying the number of variables. If not supplied,
#'   the number of variables is inferred from the length of the distribution
#'   parameter list or the skewness/kurtosis vectors.
#' @param keepScale Logical indicating whether generated variables should
#'   retain their original scale.
#' @param reverse Logical vector indicating whether variables should be
#'   reversed after generation.
#' @param copula Optional copula object defining the multivariate dependence
#'   structure between variables.
#' @param skewness Numeric vector specifying skewness values for the
#'   marginal distributions.
#' @param kurtosis Numeric vector specifying kurtosis values for the
#'   marginal distributions.
#'
#' @details
#' The function constructs a \code{SimDataDist} object used internally
#' by \code{simsem} to generate simulated data with user-specified
#' marginal distributions and dependence structures.
#'
#' Marginal distributions can be specified either by providing a list of
#' distribution parameters through \code{...}, or by specifying target
#' skewness and kurtosis values.
#'
#' If a copula object is supplied, its dimension is automatically updated
#' to match the number of variables.
#'
#' @return
#' An object of class \code{SimDataDist}.
#'
#' @seealso
#' \code{\link{SimDataDist-class}}
#'
#' @examples
#' # Specify normal marginal distributions
#' bindDist(rep("norm", 3))
#'
#' # Specify distributions using skewness and kurtosis
#' bindDist(skewness = c(0, 1), kurtosis = c(0, 3))
#'
#' @export
bindDist <- function(margins = NULL, ..., p = NULL, keepScale = TRUE, reverse = FALSE, copula = NULL, skewness = NULL, kurtosis = NULL) {
  List <- list(...)
  if (length(List) > 0) {
    if (!is.null(skewness)) stop("CONFLICT: skewness and list of distributions cannot be both specified.")
    if (!is.null(kurtosis)) stop("CONFLICT: kurtosis and list of distributions cannot be both specified.")
    skewness <- rep(NA, length(List))
    kurtosis <- rep(NA, length(List))
  } else {
    ## SP: BUG FIX: Allow margins-only specification (e.g., margins="norm", p=3)
    if (!is.null(margins)) {
      if (is.null(p)) {
        if (!is.null(skewness)) {
          p <- length(skewness)
        } else if (!is.null(kurtosis)) {
          p <- length(kurtosis)
        } else {
          p <- length(margins)
        }
      }

      if (is.null(skewness) && is.null(kurtosis)) {
        skewness <- rep(NA, p)
        kurtosis <- rep(NA, p)
      }
    }
    if (!is.null(skewness)) {
      if (!is.null(kurtosis)) {
        if (length(skewness) != length(kurtosis)) stop("CONFLICT: The length of skewness and kurtosis must be equal.")
      } else {
        kurtosis <- rep(0, length(skewness))
      }
    } else {
      if (!is.null(kurtosis)) {
        skewness <- rep(0, length(kurtosis))
      } else {
        stop("CONFLICT: Either the list of distributions and the skewness (or kurtosis) argument must be specified.")
      }
    }
    List <- rep(list(NA), length(skewness))
  }
  if (is.null(p)) {
    if (length(List) > 0) {
      p <- length(List)
    } else {
      # Already checked for skewness existence above
      p <- length(skewness)
    }
  }
  if (!is.null(margins)) {
    if (length(margins) == 1) margins <- rep(margins, p)
  } else {
    margins <- rep("NA", p)
  }
  if (length(reverse) == 1) {
    reverse <- rep(reverse, p)
  }
  if (length(reverse) != p) {
    stop("Please specify the reverse option as TRUE or FALSE or the vector of TRUE/FALSE with the length of the number of the marginal distributions.")
  }
  if (length(margins) != p) {
    stop("Please specify the type of marginal distribution so that the length of the number of the marginal distributions is equal to the number of desired variables.")
  }
  if (length(keepScale) == 1) {
    keepScale <- rep(keepScale, p)
  }
  if (length(keepScale) != p) {
    stop("Please specify the keepScale option as TRUE or FALSE or the vector of TRUE/FALSE with the length of the number of the marginal distributions.")
  }
  if (length(List) != p) {
    List <- rep(List, length.out = p)
  }
  if (length(skewness) != p) {
    skewness <- rep(skewness, length.out = p)
  }
  if (length(kurtosis) != p) {
    kurtosis <- rep(kurtosis, length.out = p)
  }
  if (!is.null(copula)) {
    if (!inherits(copula, "copula")) stop("The 'copula' argument is not a multivariate copula")
    copula@dimension <- as.integer(p)
  } else {
    copula <- new("NullCopula")
  }
  return(new("SimDataDist",
    margins = margins, paramMargins = List, p = p, keepScale = keepScale,
    reverse = reverse, copula = copula, skewness = skewness, kurtosis = kurtosis
  ))
}
