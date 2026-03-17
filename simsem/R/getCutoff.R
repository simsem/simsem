### Sunthud Pornprasertmanit
### Last updated: 6 March 2026
### Find a cutoff of each fit index based on a priori alpha level from sampling distributions of fit indices

#' Compute Fit-Index Cutoffs from Sampling Distributions
#'
#' Computes cutoff values for fit indices from their sampling distributions
#' given a prespecified Type I error rate (\code{alpha}). The function extracts
#' the required fit indices, computes the relevant quantiles, and optionally
#' adjusts directions for fit indices whose interpretation is reversed
#' (e.g., RMSEA).
#'
#' @param object A data frame containing simulated fit indices.
#' @param alpha Numeric value representing the Type I error rate used to
#'   determine the cutoff.
#' @param revDirec Logical. If \code{TRUE}, the percentile direction is reversed.
#' @param usedFit Character vector specifying which fit indices should be used.
#' @param predictor Optional matrix of predictor variables used for conditional
#'   quantile estimation.
#' @param predictorVal Vector of predictor values at which the conditional
#'   quantile should be evaluated.
#' @param df Degrees of freedom used when fitting spline terms in conditional
#'   quantile regression. Default is \code{0}.
#'
#' @return A data frame containing cutoff values for the specified fit indices.
#'
#' @seealso \code{\link{getCutoff}}, \code{\link{getCondQtile}}
#'
#' @keywords internal
getCutoffDataFrame <- function(
  object,
  alpha, revDirec = FALSE, usedFit = NULL, predictor = NULL, predictorVal = NULL,
  df = 0
) {
  usedFit <- cleanUsedFit(usedFit, tolower(colnames(object)))
  percentile <- 1 - alpha
  if (revDirec) {
    percentile <- 1 - percentile
  }
  object <- as.data.frame(object[, match(usedFit, tolower(colnames(object)))])
  colnames(object) <- usedFit
  temp <- list()
  temp <- lapply(object, getCondQtile,
    qtile = percentile, df = df, x = predictor,
    xval = predictorVal
  )
  reversedCol <- which(colnames(object) %in% getKeywords()$reversedFit)
  for (i in seq_along(reversedCol)) {
    temp[[reversedCol[i]]] <- getCondQtile(object[, reversedCol[i]],
      x = predictor, xval = predictorVal,
      qtile = 1 - percentile, df = df
    )
  }
  temp <- data.frame(temp)
  return(temp)
}

#' Obtain Fit-Index Cutoffs from Simulation Results
#'
#' Extracts simulated fit indices from a result object and computes cutoff
#' values based on a prespecified Type I error rate (\code{alpha}). The function
#' allows conditioning on simulation design factors such as sample size or
#' missingness proportions.
#'
#' @param object A simulation result object containing fit indices.
#' @param alpha Numeric value representing the Type I error rate used to
#'   determine the cutoff.
#' @param revDirec Logical. If \code{TRUE}, the percentile direction is reversed.
#' @param usedFit Character vector specifying which fit indices should be used.
#' @param nVal Optional numeric value specifying the sample size condition.
#' @param pmMCARval Optional numeric value specifying the proportion of
#'   missing completely at random (MCAR).
#' @param pmMARval Optional numeric value specifying the proportion of
#'   missing at random (MAR).
#' @param df Degrees of freedom used in spline smoothing when estimating
#'   conditional quantiles.
#'
#' @return A data frame containing cutoff values for each requested fit index.
#'
#' @seealso \code{\link{getCutoffDataFrame}}
#'
#' @export
getCutoff <- function(
  object,
  alpha, revDirec = FALSE, usedFit = NULL, nVal = NULL, pmMCARval = NULL, pmMARval = NULL,
  df = 0
) {
  if (is.null(nVal) || is.na(nVal)) {
    nVal <- NULL
  }
  if (is.null(pmMCARval) || is.na(pmMCARval)) {
    pmMCARval <- NULL
  }
  if (is.null(pmMARval) || is.na(pmMARval)) {
    pmMARval <- NULL
  }
  object <- clean(object)
  Data <- as.data.frame(object@fit)
  condValuePredictorVal <- getCondValuePredictorVal(object, nVal, pmMCARval, pmMARval)

  output <- getCutoffDataFrame(Data, alpha, revDirec, usedFit,
    predictor = condValuePredictorVal[[1]], predictorVal = condValuePredictorVal[[2]],
    df = df
  )
  return(output)
}

#' Extract Predictor Values for Conditional Quantile Estimation
#'
#' Identifies which simulation design factors vary in the simulation results
#' and prepares predictor matrices and corresponding values for conditional
#' quantile estimation.
#'
#' @param object A simulation result object containing simulation conditions.
#' @param nVal Numeric value specifying the sample size condition.
#' @param pmMCARval Numeric value specifying the proportion of missing
#'   completely at random (MCAR).
#' @param pmMARval Numeric value specifying the proportion of missing
#'   at random (MAR).
#'
#' @return A list containing two elements:
#' \describe{
#'   \item{condValue}{Matrix of predictor variables.}
#'   \item{predictorVal}{Vector of predictor values used for evaluation.}
#' }
#'
#' @keywords internal
getCondValuePredictorVal <- function(object, nVal = NULL, pmMCARval = NULL, pmMARval = NULL) {
  condition <- c(length(unique(object@pmMCAR)) > 1, length(unique(object@pmMAR)) >
    1, length(unique(object@n)) > 1)
  condValue <- cbind(object@pmMCAR, object@pmMAR, object@n)
  colnames(condValue) <- c("Percent MCAR", "Percent MAR", "N")
  condValue <- condValue[, condition]
  if (is.null(condValue) || length(condValue) == 0) {
    condValue <- NULL
  }
  predictorVal <- rep(NA, 3)
  if (condition[3]) {
    ifelse(is.null(nVal), stop("Please specify the sample size value, 'nVal', because the sample size in the result object is varying"),
      predictorVal[3] <- nVal
    )
  }
  if (condition[1]) {
    ifelse(is.null(pmMCARval), stop("Please specify the percent of completely missing at random, 'pmMCARval', because the percent of completely missing at random in the result object is varying"),
      predictorVal[1] <- pmMCARval
    )
  }
  if (condition[2]) {
    ifelse(is.null(pmMARval), stop("Please specify the percent of missing at random, 'pmMARval', because the percent of missing at random in the result object is varying"),
      predictorVal[2] <- pmMARval
    )
  }
  predictorVal <- predictorVal[condition]
  list(condValue, predictorVal)
}


#' Conditional Quantile Estimation
#'
#' Computes a quantile of a variable. If predictors are supplied, the function
#' estimates the conditional quantile using quantile regression from the
#' \pkg{quantreg} package.
#'
#' @param y Numeric vector containing the variable for which the quantile is
#'   computed.
#' @param x Optional predictor matrix used to estimate conditional quantiles.
#'   If \code{NULL}, the unconditional quantile of \code{y} is returned.
#' @param xval Vector of predictor values at which the conditional quantile
#'   should be evaluated. If \code{"all"}, predictions are returned for all
#'   observed predictor values.
#' @param df Degrees of freedom used when fitting spline terms for predictors.
#'   If \code{0}, spline terms are not used.
#' @param qtile Numeric value specifying the desired quantile (e.g., 0.5 for
#'   the median).
#'
#' @return A numeric value representing the requested (conditional) quantile.
#'
#' @references
#' Koenker, R. (2005). \emph{Quantile Regression}. Cambridge University Press.
#'
#' @keywords internal
getCondQtile <- function(y, x = NULL, xval = NULL, df = 0, qtile = 0.5) {
  if (is.null(x)) {
    return(quantile(y, probs = qtile, na.rm = TRUE))
  } else {
    if (!is.matrix(x)) {
      x <- as.matrix(x)
    }
    p <- ncol(x)
    name <- paste("x", 1:p, sep = "")
    colnames(x) <- name
    if (df == 0) {
      name2 <- name
    } else {
      requireNamespace("splines")
      if (!("package:splines" %in% search())) attachNamespace("splines")
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
    requireNamespace("quantreg")
    if (!("package:quantreg" %in% search())) attachNamespace("quantreg")
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
