### Sunthud Pornprasertmanit
### Last updated: 6 March 2026
### Plot sampling distributions of difference in fit indices that visualize power

#' Plot power of rejecting a non-nested model based on a difference in fit index
#'
#' Plot the proportion of the differences in fit indices from one model that fall
#' outside the sampling distribution from another model (rejecting the hypothesis
#' that the dataset comes from the second model) or indicate worse fit than a
#' specified cutoff. The plot can also show the proportion in the second model
#' that falls outside the sampling distribution from the first model.
#'
#' @param dat2Mod1 A \code{\linkS4class{SimResult}} object containing the
#' simulation results from analyzing Model 1 using datasets generated from Model 2.
#' @param dat2Mod2 A \code{\linkS4class{SimResult}} object containing the
#' simulation results from analyzing Model 2 using datasets generated from Model 2.
#' @param dat1Mod1 A \code{\linkS4class{SimResult}} object containing the
#' simulation results from analyzing Model 1 using datasets generated from Model 1.
#' @param dat1Mod2 A \code{\linkS4class{SimResult}} object containing the
#' simulation results from analyzing Model 2 using datasets generated from Model 1.
#' @param cutoff A vector of a priori cutoffs for the differences in fit indices.
#' @param usedFit Vector of names of fit indices that researchers wish to plot.
#' @param alpha A priori alpha level.
#' @param contN Logical indicating whether varying sample size should be included
#' in the power plot when available.
#' @param contMCAR Logical indicating whether varying MCAR (missing completely at
#' random percentage) should be included in the power plot when available.
#' @param contMAR Logical indicating whether varying MAR (missing at random
#' percentage) should be included in the power plot when available.
#' @param useContour If two of sample size, percent missing completely at random,
#' and percent missing at random vary, a 3D plot is produced. By default, a
#' contour plot is used. If \code{FALSE}, a perspective plot is used instead.
#' @param logistic If \code{TRUE} and a varying parameter exists (e.g., sample
#' size or percent missing), the plot based on logistic regression predicting
#' significance from the varying parameters is used. If \code{FALSE}, an overlay
#' scatterplot with a cutoff line is plotted.
#' @param onetailed Logical indicating whether the cutoff should be based on a
#' one-tailed test. If \code{FALSE}, the cutoff is based on a two-tailed test.
#'
#' @return
#' None. The function produces plots of fit-index distributions or power curves.
#'
#' @seealso
#' \itemize{
#' \item \code{\linkS4class{SimResult}} for simulation results used in this function.
#' \item \code{\link{getCutoffNonNested}} to compute cutoffs for differences in
#' fit indices in non-nested model comparisons.
#' \item \code{\link{plotCutoffNonNested}} to visualize cutoffs for differences
#' in fit indices in non-nested model comparisons.
#' \item \code{\link{getPowerFitNonNested}} to compute statistical power for
#' rejecting a non-nested model based on fit-index cutoffs.
#' }
#'
#' @examples
#' \dontrun{
#' # Model A: Factor 1 on Items 1-3 and Factor 2 on Items 4-8
#' loading.A <- matrix(0, 8, 2)
#' loading.A[1:3, 1] <- NA
#' loading.A[4:8, 2] <- NA
#' LY.A <- bind(loading.A, 0.7)
#' latent.cor <- matrix(NA, 2, 2)
#' diag(latent.cor) <- 1
#' RPS <- binds(latent.cor, "runif(1, 0.7, 0.9)")
#' RTE <- binds(diag(8))
#' CFA.Model.A <- model(LY = LY.A, RPS = RPS, RTE = RTE, modelType = "CFA")
#'
#' # Model B: Factor 1 on Items 1-4 and Factor 2 on Items 5-8
#' loading.B <- matrix(0, 8, 2)
#' loading.B[1:4, 1] <- NA
#' loading.B[5:8, 2] <- NA
#' LY.B <- bind(loading.B, 0.7)
#' CFA.Model.B <- model(LY = LY.B, RPS = RPS, RTE = RTE, modelType = "CFA")
#'
#' # The actual number of replications should be greater than 10.
#' Output.A.A <- sim(10, n = 500, model = CFA.Model.A, generate = CFA.Model.A)
#' Output.A.B <- sim(10, n = 500, model = CFA.Model.B, generate = CFA.Model.A)
#' Output.B.A <- sim(10, n = 500, model = CFA.Model.A, generate = CFA.Model.B)
#' Output.B.B <- sim(10, n = 500, model = CFA.Model.B, generate = CFA.Model.B)
#'
#' # Plot the power based on the derived cutoff for both models
#' plotPowerFitNonNested(Output.B.A, Output.B.B,
#'   dat1Mod1 = Output.A.A, dat1Mod2 = Output.A.B
#' )
#'
#' # Plot the power based on AIC and BIC cutoffs
#' plotPowerFitNonNested(Output.B.A, Output.B.B, cutoff = c(AIC = 0, BIC = 0))
#' }
#'
#' @export
plotPowerFitNonNested <- function(
  dat2Mod1, dat2Mod2, dat1Mod1 = NULL, dat1Mod2 = NULL,
  cutoff = NULL, usedFit = NULL, alpha = 0.05, contN = TRUE, contMCAR = TRUE, contMAR = TRUE,
  useContour = TRUE, logistic = TRUE, onetailed = FALSE
) {
  if (is.null(cutoff) & is.null(dat1Mod1) & is.null(dat1Mod2)) {
    stop("Please specify result objects representing the simulation results for datasets from Model 1 ('dat1Mod1' and 'dat1Mod2') or cutoff")
  }
  usedFit <- cleanUsedFit(usedFit, colnames(dat2Mod1@fit), colnames(dat2Mod2@fit))
  mod2 <- clean(dat2Mod1, dat2Mod2)
  dat2Mod1 <- mod2[[1]]
  dat2Mod2 <- mod2[[2]]

  if (!isTRUE(all.equal(unique(dat2Mod1@paramValue), unique(dat2Mod2@paramValue)))) {
    stop("'dat2Mod1' and 'dat2Mod2' are based on different data and cannot be compared, check your random seed")
  }
  if (!is.null(dat1Mod1) & !is.null(dat1Mod2)) {
    mod1 <- clean(dat1Mod1, dat1Mod2)
    dat1Mod1 <- mod1[[1]]
    dat1Mod2 <- mod1[[2]]
    if (!isTRUE(all.equal(unique(dat1Mod1@paramValue), unique(dat1Mod2@paramValue)))) {
      stop("'dat1Mod1' and 'dat1Mod2' are based on different data and cannot be compared, check your random seed")
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
    if (!isTRUE(all.equal(unique(dat2Mod1@n), unique(dat2Mod2@n)))) {
      stop("Models are based on different values of sample sizes")
    }
    if (!isTRUE(all.equal(unique(dat2Mod1@pmMCAR), unique(dat2Mod2@pmMCAR)))) {
      stop("Models are based on different values of the percent completely missing at random")
    }
    if (!isTRUE(all.equal(unique(dat2Mod1@pmMAR), unique(dat2Mod2@pmMAR)))) {
      stop("Models are based on different values of the percent missing at random")
    }
  }


  nrep <- dim(dat2Mod1@fit)[[1]]
  if (is.null(usedFit)) {
    usedFit <- getKeywords()$usedFit
  }
  if (!is.null(cutoff)) {
    names(cutoff) <- cleanUsedFit(names(cutoff))
    usedFit <- intersect(usedFit, names(cutoff))
    cutoff <- cutoff[usedFit]
  }
  # Create matrix of predictors (randomly varying params)
  x <- NULL
  pred <- NULL

  if ((length(unique(dat2Mod1@n)) > 1) && contN) {
    if (!length(dat2Mod1@n) == nrep) {
      stop("Number of random sample sizes is not the same as the number of replications, check to see if N varied across replications")
    }
    x <- cbind(x, N = dat2Mod1@n)
    pred$N <- min(dat2Mod1@n):max(dat2Mod1@n)
  }
  if ((length(unique(dat2Mod1@pmMCAR)) > 1) && contMCAR) {
    if (!length(dat2Mod1@pmMCAR) == nrep) {
      stop("Number of random pmMCARs is not the same as the number of replications, check to see if pmMCAR varied across replications")
    }
    x <- cbind(x, pmMCAR = dat2Mod1@pmMCAR)
    pred$MCAR <- seq(min(dat2Mod1@pmMCAR), max(dat2Mod1@pmMCAR), by = 0.01)
  }
  if ((length(unique(dat2Mod1@pmMAR)) > 1) && contMAR) {
    if (!length(dat2Mod1@pmMAR) == nrep) {
      stop("Number of random pmMARs is not the same as the number of replications, check to see if pmMAR varied across replications")
    }
    x <- cbind(x, pmMAR = dat2Mod1@pmMAR)
    pred$MAR <- seq(min(dat2Mod1@pmMAR), max(dat2Mod1@pmMAR), by = 0.01)
  }

  Data1 <- NULL
  Data2 <- as.data.frame((dat2Mod1@fit - dat2Mod2@fit))
  if (!is.null(dat1Mod1) & !is.null(dat1Mod2)) {
    Data1 <- as.data.frame((dat1Mod1@fit - dat1Mod2@fit))
  }

  condition <- c(length(dat2Mod1@pmMCAR) > 1, length(dat2Mod1@pmMAR) > 1, length(dat2Mod1@n) >
    1)
  condValue <- cbind(dat2Mod1@pmMCAR, dat2Mod1@pmMAR, dat2Mod1@n)
  colnames(condValue) <- c("Percent MCAR", "Percent MAR", "N")

  cutoff1 <- cutoff
  cutoff2 <- NULL
  cutoff3 <- NULL
  if (!is.null(cutoff)) {
    cutoff3 <- -cutoff
  }
  cutoff4 <- NULL
  if (!is.null(alpha)) {
    if (is.null(x)) {
      if (is.null(cutoff)) {
        cutoff <- getCutoffNonNested(dat1Mod1, dat1Mod2, alpha = alpha, onetailed = onetailed)[[1]]
        cutoff1 <- cutoff[1, ]
        cutoff2 <- cutoff[2, ]
        if (!is.null(dat2Mod1) && !is.null(dat2Mod2)) {
          cutoff <- getCutoffNonNested(dat2Mod2, dat2Mod1,
            alpha = alpha,
            onetailed = onetailed
          )[[1]]
          cutoff3 <- -cutoff[1, ]
          cutoff4 <- -cutoff[2, ]
        }
      }
    }
  }

  if (is.null(x)) {
    if (!is.null(Data1)) {
      plotOverHist(Data2, Data1,
        cutoff = cutoff1, usedFit = usedFit, cutoff2 = cutoff2,
        cutoff3 = cutoff3, cutoff4 = cutoff4
      )
    } else {
      plotCutoffDataFrame(Data2, cutoff1, usedFit = usedFit, cutoff2 = cutoff2)
    }
  } else if (ncol(x) == 1) {
    if (logistic & (!is.null(Data1) | !is.null(cutoff))) {
      plotLogisticFit(Data2,
        nullObject = Data1, cutoff = cutoff, usedFit = usedFit,
        x = x, xval = pred, alpha = alpha, useContour = useContour
      )
    } else {
      plotScatter(Data2,
        nullObject = Data1, cutoff = cutoff, usedFit = usedFit,
        x = x, alpha = alpha
      )
      # Plot scatterplot if only one continuous; Optional for putting horizontal
      # cutoff If the cutoff exists, the power plot can be used.
    }
  } else if (ncol(x) == 2) {
    if (logistic & (!is.null(Data1) | !is.null(cutoff))) {
      plotLogisticFit(Data2,
        nullObject = Data1, cutoff = cutoff, usedFit = usedFit,
        x = x, xval = pred, alpha = alpha, useContour = useContour
      )
    } else {
      stop("Cannot make scatter plot with two or more varying variables")
    }
    # If the cutoff exists, the power 2/3D plot can be used.
  } else {
    stop("The varying parameter used cannot be over two dimensions.")
  }
}
