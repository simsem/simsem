### Sunthud Pornprasertmanit
### Last updated: 6 March 2026
### Compute cutoffs from the simulated sampling distribution of differences in fit indices

#' Compute Cutoffs for Differences in Fit Indices Between Non-Nested Models
#'
#' Computes cutoff values for differences in fit indices between two competing
#' (non-nested) models using simulated sampling distributions. The function
#' evaluates the distribution of differences in fit indices between two models
#' estimated from the same simulated datasets.
#'
#' The function can optionally compare models under two different data-generating
#' conditions. In that case, cutoffs are computed separately for each condition.
#'
#' @param dat1Mod1 A simulation result object for Model 1 under the first
#'   data-generating condition.
#' @param dat1Mod2 A simulation result object for Model 2 under the first
#'   data-generating condition.
#' @param dat2Mod1 Optional simulation result object for Model 1 under the
#'   second data-generating condition.
#' @param dat2Mod2 Optional simulation result object for Model 2 under the
#'   second data-generating condition.
#' @param alpha Numeric value representing the Type I error rate used to
#'   determine the cutoff. Default is \code{0.05}.
#' @param usedFit Character vector specifying which fit indices should be used.
#'   If \code{NULL}, all available fit indices are used.
#' @param onetailed Logical. If \code{TRUE}, one-tailed cutoffs are computed.
#'   Otherwise, two-tailed cutoffs are returned.
#' @param nVal Optional numeric value specifying the sample size condition when
#'   multiple sample sizes were simulated.
#' @param pmMCARval Optional numeric value specifying the proportion of missing
#'   completely at random (MCAR) when multiple MCAR conditions were simulated.
#' @param pmMARval Optional numeric value specifying the proportion of missing
#'   at random (MAR) when multiple MAR conditions were simulated.
#' @param df Degrees of freedom used when estimating conditional quantiles
#'   using spline regression. Default is \code{0}.
#'
#' @return A list containing cutoff intervals for each fit index. If only one
#' data-generating condition is provided, the list contains a single element
#' (\code{model1}). If two data-generating conditions are provided, the list
#' contains two elements (\code{model1} and \code{model2}).
#'
#' Each element is a matrix with rows:
#' \describe{
#'   \item{lower}{Lower cutoff bound.}
#'   \item{upper}{Upper cutoff bound.}
#' }
#'
#' @details
#' Differences in fit indices are computed as
#'
#' \deqn{\Delta Fit = Fit_{Model1} - Fit_{Model2}}
#'
#' Cutoff values are obtained from the simulated sampling distribution of these
#' differences. When \code{onetailed = TRUE}, the cutoff corresponds to the
#' specified tail of the distribution. When \code{onetailed = FALSE}, symmetric
#' two-tailed cutoffs are returned.
#'
#' All compared models must be estimated from the same simulated datasets and
#' share identical simulation conditions (e.g., parameter values, sample sizes,
#' and missingness rates).
#'
#' @seealso
#' \code{\link{getCutoff}},
#' \code{\link{getCutoffNested}},
#' \code{\link{getCutoffDataFrame}}
#'
#' @keywords simulation
#'
#' @export
getCutoffNonNested <- function(
  dat1Mod1, dat1Mod2, dat2Mod1 = NULL, dat2Mod2 = NULL,
  alpha = 0.05, usedFit = NULL, onetailed = FALSE, nVal = NULL, pmMCARval = NULL,
  pmMARval = NULL, df = 0
) {
  usedFit <- cleanUsedFit(usedFit, colnames(dat1Mod1@fit), colnames(dat1Mod2@fit))
  mod1 <- clean(dat1Mod1, dat1Mod2)
  dat1Mod1 <- mod1[[1]]
  dat1Mod2 <- mod1[[2]]
  if (!isTRUE(all.equal(unique(dat1Mod1@paramValue), unique(dat1Mod2@paramValue)))) {
    stop("'dat1Mod1' and 'dat1Mod2' are based on different data and cannot be compared, check your random seed")
  }
  if (!is.null(dat2Mod1) & !is.null(dat2Mod2)) {
    mod2 <- clean(dat2Mod1, dat2Mod2)
    dat2Mod1 <- mod2[[1]]
    dat2Mod2 <- mod2[[2]]
    if (!isTRUE(all.equal(unique(dat2Mod1@paramValue), unique(dat2Mod2@paramValue)))) {
      stop("'dat2Mod1' and 'dat2Mod2' are based on different data and cannot be compared, check your random seed")
    }
    if (!multipleAllEqual(
      unique(dat1Mod1@n), unique(dat1Mod2@n), unique(dat2Mod1@n),
      unique(dat2Mod2@n)
    )) {
      stop("Models are based on different values of sample sizes")
    }
    if (!multipleAllEqual(
      unique(dat1Mod1@pmMCAR), unique(dat1Mod2@pmMCAR), unique(dat2Mod1@pmMCAR),
      unique(dat2Mod2@pmMCAR)
    )) {
      stop("Models are based on different values of the percent completely missing at random")
    }
    if (!multipleAllEqual(
      unique(dat1Mod1@pmMAR), unique(dat1Mod2@pmMAR), unique(dat2Mod1@pmMAR),
      unique(dat2Mod2@pmMAR)
    )) {
      stop("Models are based on different values of the percent missing at random")
    }
  } else {
    if (!isTRUE(all.equal(unique(dat1Mod1@n), unique(dat1Mod2@n)))) {
      stop("Models are based on different values of sample sizes")
    }
    if (!isTRUE(all.equal(unique(dat1Mod1@pmMCAR), unique(dat1Mod2@pmMCAR)))) {
      stop("Models are based on different values of the percent completely missing at random")
    }
    if (!isTRUE(all.equal(unique(dat1Mod1@pmMAR), unique(dat1Mod2@pmMAR)))) {
      stop("Models are based on different values of the percent missing at random")
    }
  }
  if (is.null(nVal) || is.na(nVal)) {
    nVal <- NULL
  }
  if (is.null(pmMCARval) || is.na(pmMCARval)) {
    pmMCARval <- NULL
  }
  if (is.null(pmMARval) || is.na(pmMARval)) {
    pmMARval <- NULL
  }
  Data1 <- as.data.frame((dat1Mod1@fit - dat1Mod2@fit))
  Data2 <- NULL
  if (!is.null(dat2Mod1) & !is.null(dat2Mod2)) {
    Data2 <- as.data.frame((dat2Mod1@fit - dat2Mod2@fit))
  }

  condition <- c(length(unique(dat1Mod1@pmMCAR)) > 1, length(unique(dat1Mod1@pmMAR)) >
    1, length(unique(dat1Mod1@n)) > 1)
  condValue <- cbind(dat1Mod1@pmMCAR, dat1Mod1@pmMAR, dat1Mod1@n)
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
  result <- list()
  if (onetailed) {
    cutoffDat1 <- getCutoffDataFrame(Data1, alpha, FALSE, usedFit,
      predictor = condValue,
      predictorVal = predictorVal, df = df
    )
    bound <- rep(-Inf, length(cutoffDat1))
    bound[names(cutoffDat1) %in% getKeywords()$reversedFit] <- Inf
    resultModel1 <- rbind(bound, cutoffDat1)
    resultModel1 <- apply(resultModel1, 2, sort)
    rownames(resultModel1) <- c("lower", "upper")
    result$model1 <- resultModel1
    if (!is.null(dat2Mod1) & !is.null(dat2Mod2)) {
      cutoffDat2 <- getCutoffDataFrame(Data2, 1 - alpha, FALSE, usedFit,
        predictor = condValue,
        predictorVal = predictorVal, df = df
      )
      bound <- rep(Inf, length(cutoffDat2))
      bound[names(cutoffDat2) %in% getKeywords()$reversedFit] <- -Inf
      resultModel2 <- rbind(bound, cutoffDat2)
      resultModel2 <- apply(resultModel2, 2, sort)
      rownames(resultModel2) <- c("lower", "upper")
      result$model2 <- resultModel2
    }
  } else {
    lower <- alpha / 2
    upper <- 1 - (alpha / 2)
    cutoffDat1Low <- getCutoffDataFrame(Data1, lower, FALSE, usedFit,
      predictor = condValue,
      predictorVal = predictorVal, df = df
    )
    cutoffDat1High <- getCutoffDataFrame(Data1, upper, FALSE, usedFit,
      predictor = condValue,
      predictorVal = predictorVal, df = df
    )
    resultModel1 <- rbind(cutoffDat1Low, cutoffDat1High)
    resultModel1 <- apply(resultModel1, 2, sort)
    rownames(resultModel1) <- c("lower", "upper")
    result$model1 <- resultModel1
    if (!is.null(dat2Mod1) & !is.null(dat2Mod2)) {
      cutoffDat2Low <- getCutoffDataFrame(Data2, lower, FALSE, usedFit,
        predictor = condValue,
        predictorVal = predictorVal, df = df
      )
      cutoffDat2High <- getCutoffDataFrame(Data2, upper, FALSE, usedFit,
        predictor = condValue,
        predictorVal = predictorVal, df = df
      )
      resultModel2 <- rbind(cutoffDat2Low, cutoffDat2High)
      resultModel2 <- apply(resultModel2, 2, sort)
      rownames(resultModel2) <- c("lower", "upper")
      result$model2 <- resultModel2
    }
  }
  return(result)
}
