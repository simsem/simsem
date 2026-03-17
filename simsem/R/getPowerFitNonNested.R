### Sunthud Pornprasertmanit
### Last updated: 6 March 2026
### Compute power of fit indices for non-nested model comparisons

#' Estimate Power of Fit Indices for Non-Nested Model Comparisons
#'
#' Computes the statistical power of fit indices for comparisons between
#' non-nested models. Power is defined as the probability that the difference
#' in fit indices between two competing models exceeds a specified cutoff
#' criterion.
#'
#' Cutoffs may be supplied directly or derived from simulated null-model
#' results.
#'
#' @param dat2Mod1 Simulation result object for Model 1 under the alternative
#' data-generating condition.
#' @param dat2Mod2 Simulation result object for Model 2 under the alternative
#' data-generating condition.
#' @param cutoff Optional named numeric vector specifying cutoff values for
#' each fit index.
#' @param dat1Mod1 Optional simulation result object for Model 1 under the
#' null data-generating condition.
#' @param dat1Mod2 Optional simulation result object for Model 2 under the
#' null data-generating condition.
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
#' @param onetailed Logical indicating whether a one-tailed test should be used.
#'
#' @return
#' A list containing two elements:
#' \describe{
#' \item{reject1FromNull2}{Power for rejecting Model 1 when Model 2 is the null.}
#' \item{reject2FromNull1}{Power for rejecting Model 2 when Model 1 is the null.}
#' }
#'
#' @seealso
#' \code{\link{getPowerFit}},
#' \code{\link{getPowerFitNested}},
#' \code{\link{getCutoffNonNested}}
#'
#' @export
getPowerFitNonNested <- function(dat2Mod1, dat2Mod2, cutoff = NULL,
                                 dat1Mod1 = NULL, dat1Mod2 = NULL,
                                 revDirec = FALSE, usedFit = NULL,
                                 alpha = 0.05, nVal = NULL,
                                 pmMCARval = NULL, pmMARval = NULL,
                                 condCutoff = TRUE, df = 0,
                                 onetailed = FALSE) {
  result <- NULL
  if (is.null(cutoff)) {
    if (!is.null(dat1Mod1) & !is.null(dat1Mod2)) {
      result <- getPowerFitNonNestedNullObj(dat2Mod1 = dat2Mod1, dat2Mod2 = dat2Mod2, dat1Mod1 = dat1Mod1, dat1Mod2 = dat1Mod2, usedFit = usedFit, alpha = alpha, revDirec = revDirec, nVal = nVal, pmMCARval = pmMCARval, pmMARval = pmMARval, df = df, onetailed = onetailed)
    } else {
      stop("Please specify fit index cutoff, 'cutoff', or the result object representing the null model, 'nullObject'.")
    }
  } else {
    if (is.null(dat1Mod1) & is.null(dat1Mod2)) {
      result <- getPowerFitNonNestedCutoff(dat2Mod1 = dat2Mod1, dat2Mod2 = dat2Mod2, cutoff = cutoff, usedFit = usedFit, revDirec = revDirec, nVal = nVal, pmMCARval = pmMCARval, pmMARval = pmMARval, condCutoff = condCutoff, df = df)
    } else {
      stop("Please specify either fit index cutoff, 'cutoff', or the result object representing the null model, 'nullObject', but not both.")
    }
  }
  result
}

#' Compute Power for Non-Nested Models Given Fixed Fit-Index Cutoffs
#'
#' Internal helper used by \code{getPowerFitNonNested} when cutoff values
#' are supplied directly. Power is computed using the distribution of
#' differences in fit indices between two competing models.
#'
#' @param dat2Mod1 Simulation result object for Model 1.
#' @param dat2Mod2 Simulation result object for Model 2.
#' @param cutoff Named numeric vector specifying cutoff values for fit indices.
#' @param usedFit Character vector specifying which fit indices should be used.
#' @param revDirec Logical indicating whether rejection direction is reversed.
#' @param nVal Optional sample size value.
#' @param pmMCARval Optional MCAR missingness value.
#' @param pmMARval Optional MAR missingness value.
#' @param condCutoff Logical indicating whether cutoffs vary across conditions.
#' @param df Degrees of freedom used in spline-based conditional estimation.
#'
#' @return A named vector of power estimates.
#'
#' @keywords internal
getPowerFitNonNestedCutoff <- function(dat2Mod1, dat2Mod2, cutoff,
                                       usedFit = NULL, revDirec = FALSE,
                                       nVal = NULL, pmMCARval = NULL,
                                       pmMARval = NULL, condCutoff = TRUE,
                                       df = 0) {
  getPowerFitNested(
    altNested = dat2Mod1, altParent = dat2Mod2, cutoff = cutoff,
    revDirec = revDirec, usedFit = usedFit, nVal = nVal, pmMCARval = pmMCARval,
    pmMARval = pmMARval, condCutoff = condCutoff, df = df
  )
}

#' Compute Power for Non-Nested Models Using Null-Model Simulations
#'
#' Internal helper that computes power by deriving cutoff values from
#' null-model simulations and applying them to alternative-model simulations
#' for non-nested model comparisons.
#'
#' @param dat2Mod1 Simulation result object for Model 1 under the alternative
#' condition.
#' @param dat2Mod2 Simulation result object for Model 2 under the alternative
#' condition.
#' @param dat1Mod1 Simulation result object for Model 1 under the null
#' condition.
#' @param dat1Mod2 Simulation result object for Model 2 under the null
#' condition.
#' @param usedFit Character vector specifying which fit indices should be used.
#' @param alpha Significance level used when computing cutoffs.
#' @param revDirec Logical indicating whether rejection direction is reversed.
#' @param nVal Optional sample size value.
#' @param pmMCARval Optional MCAR missingness value.
#' @param pmMARval Optional MAR missingness value.
#' @param df Degrees of freedom used in spline-based conditional cutoff
#' estimation.
#' @param onetailed Logical indicating whether a one-tailed test should be used.
#'
#' @return A list containing power estimates for both rejection directions.
#'
#' @keywords internal
getPowerFitNonNestedNullObj <- function(dat2Mod1, dat2Mod2,
                                        dat1Mod1, dat1Mod2,
                                        usedFit = NULL, alpha = 0.05,
                                        revDirec = FALSE, nVal = NULL,
                                        pmMCARval = NULL, pmMARval = NULL,
                                        df = 0, onetailed = FALSE) {
  usedFit <- cleanUsedFit(usedFit, colnames(dat2Mod1@fit), colnames(dat2Mod2@fit), colnames(dat1Mod1@fit), colnames(dat1Mod2@fit))
  mod1 <- clean(dat2Mod1, dat2Mod2)
  dat2Mod1 <- mod1[[1]]
  dat2Mod2 <- mod1[[2]]
  mod2 <- clean(dat1Mod1, dat1Mod2)
  dat1Mod1 <- mod2[[1]]
  dat1Mod2 <- mod2[[2]]
  if (!isTRUE(all.equal(unique(dat2Mod1@paramValue), unique(dat2Mod2@paramValue)))) {
    stop("'dat2Mod1' and 'dat2Mod2' are based on different data and cannot be compared, check your random seed")
  }
  if (!isTRUE(all.equal(unique(dat1Mod1@paramValue), unique(dat1Mod2@paramValue)))) {
    stop("'dat1Mod1' and 'dat1Mod2' are based on different data and cannot be compared, check your random seed")
  }
  if (!multipleAllEqual(
    unique(dat2Mod1@n), unique(dat2Mod2@n), unique(dat1Mod1@n),
    unique(dat1Mod2@n)
  )) {
    stop("Models are based on different values of sample sizes")
  }
  if (!multipleAllEqual(
    unique(dat2Mod1@pmMCAR), unique(dat2Mod2@pmMCAR), unique(dat1Mod1@pmMCAR),
    unique(dat1Mod2@pmMCAR)
  )) {
    stop("Models are based on different values of the percent completely missing at random")
  }
  if (!multipleAllEqual(
    unique(dat2Mod1@pmMAR), unique(dat2Mod2@pmMAR), unique(dat1Mod1@pmMAR),
    unique(dat1Mod2@pmMAR)
  )) {
    stop("Models are based on different values of the percent missing at random")
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
  condition <- c(length(unique(dat2Mod1@pmMCAR)) > 1, length(unique(dat2Mod1@pmMAR)) >
    1, length(unique(dat2Mod1@n)) > 1)
  condValue <- cbind(dat2Mod1@pmMCAR, dat2Mod1@pmMAR, dat2Mod1@n)
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

  usedDirec <- (usedFit %in% getKeywords()$reversedFit) # CFA --> TRUE, RMSEA --> FALSE
  if (revDirec) {
    usedDirec <- !usedDirec
  }
  usedDirecInverse <- !usedDirec

  Data1 <- as.data.frame((dat1Mod1@fit - dat1Mod2@fit)[, usedFit])
  Data2 <- as.data.frame((dat2Mod1@fit - dat2Mod2@fit)[, usedFit])

  cut1Data1 <- alpha
  cut2Data1 <- NULL
  cut1Data2 <- 1 - alpha
  cut2Data2 <- NULL
  if (onetailed == FALSE) {
    lower <- alpha / 2
    upper <- 1 - (alpha / 2)
    cut1Data1 <- lower
    cut2Data1 <- upper
    cut1Data2 <- upper
    cut2Data2 <- lower
  }
  cutoff1 <- list()
  cutoff1[[1]] <- getCutoffDataFrame(Data1, cut1Data1, FALSE, usedFit,
    predictor = condValue,
    predictorVal = "all", df = df
  )
  if (!is.null(cut2Data1)) {
    cutoff1[[2]] <- getCutoffDataFrame(Data1, cut2Data1, FALSE, usedFit,
      predictor = condValue,
      predictorVal = "all", df = df
    )
  }

  cutoff2 <- list()
  cutoff2[[1]] <- getCutoffDataFrame(Data2, cut1Data2, FALSE, usedFit,
    predictor = condValue,
    predictorVal = "all", df = df
  )
  if (!is.null(cut2Data2)) {
    cutoff2[[2]] <- getCutoffDataFrame(Data2, cut2Data2, FALSE, usedFit,
      predictor = condValue,
      predictorVal = "all", df = df
    )
  }

  power1 <- rep(NA, length(usedFit))
  power2 <- rep(NA, length(usedFit))
  if (is.null(condValue)) {
    power2 <- pValueDataFrame(as.numeric(cutoff1[[1]]), Data2, revDirec = usedDirec)
    power1 <- pValueDataFrame(as.numeric(cutoff2[[1]]), Data1, revDirec = !usedDirec)
    if (onetailed == FALSE) {
      power2 <- power2 + pValueDataFrame(as.numeric(cutoff1[[2]]), Data2, revDirec = !usedDirec)
      power1 <- power1 + pValueDataFrame(as.numeric(cutoff2[[2]]), Data1, revDirec = usedDirec)
    }
  } else {
    for (i in 1:length(power2)) {
      power2[i] <- pValueVariedCutoff(cutoff1[[1]][, i], Data2[, i],
        revDirec = usedDirec[i],
        x = condValue, xval = predictorVal
      )
      if (onetailed == FALSE) {
        power2[i] <- power2[i] + pValueVariedCutoff(cutoff1[[2]][, i], Data2[
          ,
          i
        ], revDirec = !usedDirec[i], x = condValue, xval = predictorVal)
      }
    }
    for (i in 1:length(power1)) {
      power1[i] <- pValueVariedCutoff(cutoff2[[1]][, i], Data1[, i],
        revDirec = !usedDirec[i],
        x = condValue, xval = predictorVal
      )
      if (onetailed == FALSE) {
        power1[i] <- power1[i] + pValueVariedCutoff(cutoff2[[2]][, i], Data1[
          ,
          i
        ], revDirec = usedDirec[i], x = condValue, xval = predictorVal)
      }
    }
  }
  names(power1) <- usedFit
  names(power2) <- usedFit
  return(list(reject1FromNull2 = power1, reject2FromNull1 = power2))
}

#' Sort Two Objects Stored in a List
#'
#' Swaps values between two objects in a list so that the first object
#' contains the smaller values and the second object contains the larger
#' values element-wise.
#'
#' @param object A list containing two objects (e.g., vectors or matrices)
#' of equal dimensions.
#'
#' @return A list containing the sorted objects.
#'
#' @keywords internal
sortList <- function(object) {
  object1 <- object[[1]]
  object2 <- object[[2]]
  result1 <- object[[1]]
  result2 <- object[[2]]
  temp <- object2 < object1
  result2[temp] <- object1[temp]
  result1[temp] <- object2[temp]
  return(list(result1, result2))
}
