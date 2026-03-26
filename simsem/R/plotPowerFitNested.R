### Sunthud Pornprasertmanit
### Last updated: 6 March 2026
### Plot sampling distributions of difference in fit indices that visualize power

#' Plot power of rejecting a nested model in a nested model comparison by each fit index
#'
#' This function plots sampling distributions of the differences in fit indices
#' between parent and nested models. Two sampling distributions are compared:
#' when the nested model is \code{FALSE} (alternative model) and when the nested
#' model is \code{TRUE} (null model).
#'
#' @param altNested \code{\linkS4class{SimResult}} that saves the simulation
#' result of the nested model when the nested model is \code{FALSE}.
#'
#' @param altParent \code{\linkS4class{SimResult}} that saves the simulation
#' result of the parent model when the nested model is \code{FALSE}.
#'
#' @param nullNested \code{\linkS4class{SimResult}} that saves the simulation
#' result of the nested model when the nested model is \code{TRUE}. This argument
#' may not be specified if the \code{cutoff} is specified.
#'
#' @param nullParent \code{\linkS4class{SimResult}} that saves the simulation
#' result of the parent model when the nested model is \code{TRUE}. This argument
#' may not be specified if the \code{cutoff} is specified.
#'
#' @param cutoff A vector of a priori cutoffs for the differences in fit indices.
#'
#' @param usedFit Vector of names of fit indices that researchers wish to plot.
#'
#' @param alpha A priori alpha level.
#'
#' @param contN Include the varying sample size in the power plot if available.
#'
#' @param contMCAR Include the varying MCAR (missing completely at random
#' percentage) in the power plot if available.
#'
#' @param contMAR Include the varying MAR (missing at random percentage) in the
#' power plot if available.
#'
#' @param useContour If two of sample size, percent completely at random, and
#' percent missing at random are varying, the function will provide a 3D graph.
#' A contour graph is the default. If specified as \code{FALSE}, a perspective
#' plot is used.
#'
#' @param logistic If \code{TRUE} and the varying parameter exists (e.g., sample
#' size or percent missing), the plot based on logistic regression predicting
#' significance from the varying parameters is preferred. If \code{FALSE}, an
#' overlaying scatterplot with a cutoff line is plotted.
#'
#' @return
#' None. The function only produces plots of the fit index distributions.
#'
#' @seealso
#' \itemize{
#' \item \code{\linkS4class{SimResult}} for simulation results used in this function.
#' \item \code{\link{getCutoffNested}} to find the cutoffs of the differences in
#' fit indices.
#' \item \code{\link{plotCutoffNested}} to visualize the cutoffs of the
#' differences in fit indices.
#' \item \code{\link{getPowerFitNested}} to find the power in rejecting the
#' nested model based on fit-index difference cutoffs.
#' }
#'
#' @examples
#' \dontrun{
#' # Null model: One-factor model
#' loading.null <- matrix(0, 6, 1)
#' loading.null[1:6, 1] <- NA
#' LY.NULL <- bind(loading.null, 0.7)
#' RPS.NULL <- binds(diag(1))
#' RTE <- binds(diag(6))
#' CFA.Model.NULL <- model(LY = LY.NULL, RPS = RPS.NULL, RTE = RTE, modelType = "CFA")
#'
#' # Alternative model: Two-factor model
#' loading.alt <- matrix(0, 6, 2)
#' loading.alt[1:3, 1] <- NA
#' loading.alt[4:6, 2] <- NA
#' LY.ALT <- bind(loading.alt, 0.7)
#' latent.cor.alt <- matrix(NA, 2, 2)
#' diag(latent.cor.alt) <- 1
#' RPS.ALT <- binds(latent.cor.alt, 0.7)
#' CFA.Model.ALT <- model(LY = LY.ALT, RPS = RPS.ALT, RTE = RTE, modelType = "CFA")
#'
#' # In reality, more than 10 replications are needed
#' Output.NULL.NULL <- sim(10, n = 500, model = CFA.Model.NULL, generate = CFA.Model.NULL)
#' Output.ALT.NULL <- sim(10, n = 500, model = CFA.Model.NULL, generate = CFA.Model.ALT)
#' Output.NULL.ALT <- sim(10, n = 500, model = CFA.Model.ALT, generate = CFA.Model.NULL)
#' Output.ALT.ALT <- sim(10, n = 500, model = CFA.Model.ALT, generate = CFA.Model.ALT)
#'
#' # Plot the power based on the derived cutoff from the models analyzed on the null datasets
#' plotPowerFitNested(Output.ALT.NULL, Output.ALT.ALT,
#'   nullNested = Output.NULL.NULL,
#'   nullParent = Output.NULL.ALT
#' )
#'
#' # Plot the power by only CFI
#' plotPowerFitNested(Output.ALT.NULL, Output.ALT.ALT,
#'   nullNested = Output.NULL.NULL,
#'   nullParent = Output.NULL.ALT,
#'   usedFit = "CFI"
#' )
#'
#' # Example of continuously varying sample size
#' Output.NULL.NULL2 <- sim(NULL,
#'   n = seq(50, 500, 5),
#'   model = CFA.Model.NULL,
#'   generate = CFA.Model.NULL
#' )
#'
#' Output.ALT.NULL2 <- sim(NULL,
#'   n = seq(50, 500, 5),
#'   model = CFA.Model.NULL,
#'   generate = CFA.Model.ALT
#' )
#'
#' Output.NULL.ALT2 <- sim(NULL,
#'   n = seq(50, 500, 5),
#'   model = CFA.Model.ALT,
#'   generate = CFA.Model.NULL
#' )
#'
#' Output.ALT.ALT2 <- sim(NULL,
#'   n = seq(50, 500, 5),
#'   model = CFA.Model.ALT,
#'   generate = CFA.Model.ALT
#' )
#'
#' # Logistic power curve across sample sizes
#' plotPowerFitNested(Output.ALT.NULL2, Output.ALT.ALT2,
#'   nullNested = Output.NULL.NULL2,
#'   nullParent = Output.NULL.ALT2
#' )
#'
#' # Scatter plot instead of logistic regression
#' plotPowerFitNested(Output.ALT.NULL2, Output.ALT.ALT2,
#'   nullNested = Output.NULL.NULL2,
#'   nullParent = Output.NULL.ALT2,
#'   logistic = FALSE
#' )
#'
#' # Scatter plot using an advanced CFI cutoff
#' plotPowerFitNested(Output.ALT.NULL2, Output.ALT.ALT2,
#'   cutoff = c(CFI = -0.1),
#'   logistic = FALSE
#' )
#' }
#'
#' @export
plotPowerFitNested <- function(
  altNested, altParent, nullNested = NULL, nullParent = NULL,
  cutoff = NULL, usedFit = NULL, alpha = 0.05, contN = TRUE, contMCAR = TRUE, contMAR = TRUE,
  useContour = TRUE, logistic = TRUE
) {
  if (is.null(nullNested) & is.null(nullParent)) {
    mod <- clean(altNested, altParent)
    altNested <- mod[[1]]
    altParent <- mod[[2]]
    if (!isTRUE(all.equal(unique(altNested@paramValue), unique(altParent@paramValue)))) {
      stop("Models are based on different data and cannot be compared, check your random seed")
    }
    if (!isTRUE(all.equal(unique(altNested@n), unique(altParent@n)))) {
      stop("Models are based on different values of sample sizes")
    }
    if (!isTRUE(all.equal(unique(altNested@pmMCAR), unique(altParent@pmMCAR)))) {
      stop("Models are based on different values of the percent completely missing at random")
    }
    if (!isTRUE(all.equal(unique(altNested@pmMAR), unique(altParent@pmMAR)))) {
      stop("Models are based on different values of the percent missing at random")
    }
  } else if (!is.null(nullNested) & !is.null(nullParent)) {
    mod <- clean(altNested, altParent, nullNested, nullParent)
    altNested <- mod[[1]]
    altParent <- mod[[2]]
    nullNested <- mod[[3]]
    nullParent <- mod[[4]]
    if (!isTRUE(all.equal(unique(altNested@paramValue), unique(altParent@paramValue)))) {
      stop("'altNested' and 'altParent' are based on different data and cannot be compared, check your random seed")
    }
    if (!isTRUE(all.equal(unique(nullNested@paramValue), unique(nullParent@paramValue)))) {
      stop("'nullNested' and 'nullParent' are based on different data and cannot be compared, check your random seed")
    }
    if (!multipleAllEqual(
      unique(altNested@n), unique(altParent@n), unique(nullNested@n),
      unique(nullParent@n)
    )) {
      stop("Models are based on different values of sample sizes")
    }
    if (!multipleAllEqual(
      unique(altNested@pmMCAR), unique(altParent@pmMCAR),
      unique(nullNested@pmMCAR), unique(nullParent@pmMCAR)
    )) {
      stop("Models are based on different values of the percent completely missing at random")
    }
    if (!multipleAllEqual(
      unique(altNested@pmMAR), unique(altParent@pmMAR), unique(nullNested@pmMAR),
      unique(nullParent@pmMAR)
    )) {
      stop("Models are based on different values of the percent missing at random")
    }
  } else {
    stop("The nullNested and nullParent arguments should be both specified.")
  }
  nrep <- dim(altNested@fit)[[1]]
  usedFit <- cleanUsedFit(usedFit, colnames(altNested@fit), colnames(altParent@fit))
  if (!is.null(cutoff)) {
    names(cutoff) <- cleanUsedFit(names(cutoff))
    usedFit <- intersect(usedFit, names(cutoff))
    cutoff <- cutoff[usedFit]
  }
  # Create matrix of predictors (randomly varying params)
  x <- NULL
  pred <- NULL

  if ((length(unique(altNested@n)) > 1) && contN) {
    if (!length(altNested@n) == nrep) {
      stop("Number of random sample sizes is not the same as the number of replications, check to see if N varied across replications")
    }
    x <- cbind(x, N = altNested@n)
    pred$N <- min(altNested@n):max(altNested@n)
  }
  if ((length(unique(altNested@pmMCAR)) > 1) && contMCAR) {
    if (!length(altNested@pmMCAR) == nrep) {
      stop("Number of random pmMCARs is not the same as the number of replications, check to see if pmMCAR varied across replications")
    }
    x <- cbind(x, pmMCAR = altNested@pmMCAR)
    pred$MCAR <- seq(min(altNested@pmMCAR), max(altNested@pmMCAR), by = 0.01)
  }
  if ((length(unique(altNested@pmMAR)) > 1) && contMAR) {
    if (!length(altNested@pmMAR) == nrep) {
      stop("Number of random pmMARs is not the same as the number of replications, check to see if pmMAR varied across replications")
    }
    x <- cbind(x, pmMAR = altNested@pmMAR)
    pred$MAR <- seq(min(altNested@pmMAR), max(altNested@pmMAR), by = 0.01)
  }
  nullMod <- NULL
  altMod <- as.data.frame(altNested@fit - altParent@fit)
  if (!is.null(nullNested)) {
    nullMod <- as.data.frame(nullNested@fit - nullParent@fit)
  }
  plotPowerFitDf(altMod,
    nullObject = nullMod, cutoff = cutoff, usedFit = usedFit,
    alpha = alpha, x = x, xval = pred, useContour = useContour, logistic = logistic
  )
}
