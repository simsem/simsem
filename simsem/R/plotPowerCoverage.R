### Sunthud Pornprasertmanit
### Last updated: 6 March 2026
### Plot the power/coverage curve given one or two varying parameters

#' Plot statistical power curves for model parameters
#'
#' Plot statistical power for one or more parameters as a function of
#' varying simulation conditions (e.g., sample size, percent missing,
#' or randomly varying model parameters).
#'
#' Power is estimated by predicting whether each replication is
#' statistically significant using logistic regression and then
#' plotting the predicted probability of significance.
#'
#' @param object A \code{\linkS4class{SimResult}} object that includes at least
#' one randomly varying parameter (e.g., sample size, percent missing,
#' or model parameters).
#'
#' @param powerParam Vector of parameter names for which power should be
#' plotted (e.g., `"f1=~y2"` or `"f1~~f2"`).
#'
#' @param alpha Significance level used to determine statistical significance.
#'
#' @param contParam Vector of parameter names that vary across replications
#' and should be used as predictors in the power plot.
#'
#' @param contN Include varying sample size in the power plot if available.
#'
#' @param contMCAR Include varying percentage of missing completely at random
#' (MCAR) in the power plot if available.
#'
#' @param contMAR Include varying percentage of missing at random (MAR)
#' in the power plot if available.
#'
#' @param useContour If two varying parameters are plotted, a contour plot is
#' used when \code{TRUE}. If \code{FALSE}, a perspective plot is used.
#'
#' @details
#' The function predicts whether each replication is statistically significant
#' using logistic regression without interaction terms, then plots the predicted
#' probability of significance across varying simulation conditions.
#'
#' @return No value is returned. This function produces plots.
#'
#' @seealso
#' \itemize{
#' \item \code{\linkS4class{SimResult}} for simulation results with varying parameters.
#' \item \code{\link{getPower}} to obtain numerical power estimates.
#' }
#'
#' @examples
#' \dontrun{
#' loading <- matrix(0, 6, 1)
#' loading[1:6, 1] <- NA
#' LY <- bind(loading, 0.4)
#' RPS <- binds(diag(1))
#' RTE <- binds(diag(6))
#'
#' CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, modelType = "CFA")
#'
#' Output <- sim(NULL,
#'   n = seq(100, 200, 20),
#'   pmMCAR = c(0, 0.1, 0.2),
#'   model = CFA.Model
#' )
#'
#' plotPower(Output, "f1=~y1", contMCAR = FALSE)
#' plotPower(Output, "f1=~y1")
#' }
#'
#' @export
plotPower <- function(
  object, powerParam, alpha = 0.05, contParam = NULL, contN = TRUE,
  contMCAR = TRUE, contMAR = TRUE, useContour = TRUE
) {
  object <- clean(object)
  crit.value <- qnorm(1 - alpha / 2)
  sig <- 0 + (abs(object@coef / object@se) > crit.value)
  colnames(sig) <- colnames(object@coef)
  if (is.null(powerParam)) {
    stop("Please specify the parameter used to plot")
  }
  j <- match(powerParam, dimnames(sig)[[2]]) # Return column indices that start with 'param'
  if ((length(j) == 0) || (any(is.na(j)))) stop("The specified parameter does not match with any parameter names in the object.")
  sig <- as.matrix(sig[, j])

  # Create matrix of predictors (randomly varying params)
  xpred <- getXandPred(object, contParam, contN, contMCAR, contMAR)
  plotPowerSig(sig, xpred[[1]], xval = xpred[[2]], mainName = powerParam, useContour = useContour)
}

#' Plot confidence interval coverage rates
#'
#' Plot confidence interval coverage rates for parameters across varying
#' simulation conditions (e.g., sample size, percent missing, or randomly
#' varying parameters).
#'
#' Coverage is estimated by predicting whether the confidence interval
#' contains the target value for each replication and modeling the probability
#' of coverage using logistic regression.
#'
#' @param object A \code{\linkS4class{SimResult}} object that includes at least
#' one randomly varying parameter.
#'
#' @param coverParam Vector of parameter names for which coverage should
#' be plotted (e.g., `"f1=~y2"` or `"f1~~f2"`).
#'
#' @param coverValue Target value for coverage evaluation (e.g., 0).
#' If \code{NULL}, the true parameter value is used.
#'
#' @param contParam Vector of parameter names that vary across replications
#' and should be used as predictors in the plot.
#'
#' @param contN Include varying sample size in the coverage plot if available.
#'
#' @param contMCAR Include varying percentage of missing completely at random
#' (MCAR) in the coverage plot if available.
#'
#' @param contMAR Include varying percentage of missing at random (MAR)
#' in the coverage plot if available.
#'
#' @param useContour If two varying parameters are plotted, a contour plot is
#' used when \code{TRUE}. If \code{FALSE}, a perspective plot is used.
#'
#' @details
#' Logistic regression is used to model whether the confidence interval
#' covers the target value across varying simulation conditions.
#'
#' @return No value is returned. This function produces plots.
#'
#' @seealso
#' \itemize{
#' \item \code{\linkS4class{SimResult}} for simulation results with varying parameters.
#' \item \code{\link{getCoverage}} to obtain numerical coverage estimates.
#' }
#'
#' @examples
#' \dontrun{
#' loading <- matrix(0, 6, 1)
#' loading[1:6, 1] <- NA
#' LY <- bind(loading, 0.4)
#' RPS <- binds(diag(1))
#' RTE <- binds(diag(6))
#'
#' CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, modelType = "CFA")
#'
#' Output <- sim(NULL,
#'   n = seq(100, 200, 20),
#'   pmMCAR = c(0, 0.1, 0.2),
#'   model = CFA.Model
#' )
#'
#' plotCoverage(Output, "f1=~y1", contMCAR = FALSE)
#' plotCoverage(Output, "f1=~y1", coverValue = 0, contMCAR = FALSE)
#' plotCoverage(Output, "f1=~y1")
#' }
#'
#' @export
plotCoverage <- function(
  object, coverParam, coverValue = NULL, contParam = NULL, contN = TRUE,
  contMCAR = TRUE, contMAR = TRUE, useContour = TRUE
) {
  object <- clean(object)
  cover <- calcCoverMatrix(object, coverValue = coverValue)

  if (is.null(coverParam)) {
    stop("Please specify the parameter used to plot")
  }
  j <- match(coverParam, dimnames(cover)[[2]]) # Return column indices that start with 'param'
  if ((length(j) == 0) || (any(is.na(j)))) stop("The specified parameter does not match with any parameter names in the object.")
  cover <- as.matrix(cover[, j])

  # Create matrix of predictors (randomly varying params)
  xpred <- getXandPred(object, contParam, contN, contMCAR, contMAR)
  plotPowerSig(cover, xpred[[1]], xval = xpred[[2]], mainName = coverParam, useContour = useContour)
}

#' Construct predictor matrices for simulation plots
#'
#' Internal helper that extracts varying simulation parameters and
#' constructs predictor matrices used in power and coverage plots.
#'
#' @keywords internal
getXandPred <- function(object, contParam = NULL, contN = TRUE, contMCAR = TRUE, contMAR = TRUE) {
  x <- NULL
  pred <- NULL
  nrep <- dim(object@coef)[[1]]
  if ((length(unique(object@n)) > 1) && contN) {
    if (!length(object@n) == nrep) {
      stop("Number of random sample sizes is not the same as the number of replications, check to see if N varied across replications")
    }
    x <- cbind(x, object@n)
    pred$N <- min(object@n):max(object@n)
  }
  if ((length(unique(object@pmMCAR)) > 1) && contMCAR) {
    if (!length(object@pmMCAR) == nrep) {
      stop("Number of random pmMCARs is not the same as the number of replications, check to see if pmMCAR varied across replications")
    }
    x <- cbind(x, object@pmMCAR)
    pred$MCAR <- seq(min(object@pmMCAR), max(object@pmMCAR), by = 0.01)
  }
  if ((length(unique(object@pmMAR)) > 1) && contMAR) {
    if (!length(object@pmMAR) == nrep) {
      stop("Number of random pmMARs is not the same as the number of replications, check to see if pmMAR varied across replications")
    }
    x <- cbind(x, object@pmMAR)
    pred$MAR <- seq(min(object@pmMAR), max(object@pmMAR), by = 0.01)
  }
  if (!is.null(contParam)) {
    if (!(dim(object@paramValue)[[1]] == nrep)) {
      stop("Number of random parameters is not the same as the number of replications, check to see if parameters varied across replications")
    }
    j <- match(contParam, names(object@paramValue)) # Return column indices that start with 'contParam'
    x <- cbind(x, object@paramValue[, j])
    paramVal <- list()
    for (i in 1:length(contParam)) {
      temp <- seq(min(object@paramValue[, contParam[i]]), max(object@paramValue[
        ,
        contParam[i]
      ]), length.out = 50)
      paramVal[[i]] <- unique(temp)
    }
    names(paramVal) <- contParam
    pred <- c(pred, paramVal)
  }
  list(x, pred)
}


#' Plot power curves from significance indicators
#'
#' Internal function that plots predicted probabilities of significance
#' based on logistic regression given a matrix of significance indicators
#' and predictor variables.
#'
#' @importFrom graphics contour lines par persp
#' @keywords internal
plotPowerSig <- function(sig, x = NULL, xval = NULL, mainName = NULL, useContour = TRUE) {
  warnT <- as.numeric(options("warn"))
  options(warn = -1)
  if (is.null(x)) {
    stop("There is no varying parameter in this object.")
  }
  if (ncol(x) > 2) {
    stop("The number of independent variables cannot be over 2. Please reduce 'contParam' or specify some of 'contN', 'contMCAR', and 'contMAR' as FALSE.")
  }
  if (ncol(sig) == 2) {
    obj <- par(mfrow = c(1, 2))
  } else if (ncol(sig) == 3) {
    obj <- par(mfrow = c(1, 3))
  } else if (ncol(sig) > 3) {
    obj <- par(mfrow = c(2, ceiling(ncol(sig) / 2)))
  } else if (ncol(sig) == 1) {
    # Intentionally leaving as blank
  } else {
    stop("Some errors occur")
  }
  for (i in 1:ncol(sig)) {
    try(mod <- glm(sig[, i] ~ x, family = binomial(link = "logit")), silent = TRUE)
    if (ncol(x) == 1) {
      predVal <- apply(data.frame(1, xval), 1, predProb, mod)
      plot(xval[[1]], predVal[2, ],
        type = "n", xlab = names(xval)[1], ylab = "Power",
        main = mainName[i], ylim = c(0, 1)
      )
      lines(xval[[1]], predVal[1, ], col = "red")
      lines(xval[[1]], predVal[3, ], col = "red")
      lines(xval[[1]], predVal[2, ])
    } else if (ncol(x) == 2) {
      FUN <- function(x, y) {
        logi <- mod$coefficients[1] + mod$coefficients[2] * x + mod$coefficients[3] *
          y
        pp <- exp(logi) / (1 + exp(logi))
        return(pp)
      }
      zpred <- outer(xval[[1]], xval[[2]], FUN)
      if (useContour) {
        contour(xval[[1]], xval[[2]], zpred,
          xlab = names(xval)[1], ylab = names(xval)[2],
          main = mainName[i]
        )
      } else {
        persp(xval[[1]], xval[[2]], zpred,
          zlim = c(0, 1), theta = 30, phi = 30,
          expand = 0.5, col = "lightblue", ltheta = 120, shade = 0.75, ticktype = "detailed",
          xlab = names(xval)[1], ylab = names(xval)[2], main = mainName[i],
          zlab = "Power"
        )
      }
    } else {
      stop("Something is wrong!")
    }
  }
  if (ncol(sig) > 1) {
    par(obj)
  }
  options(warn = warnT)
}
