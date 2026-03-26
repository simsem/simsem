### Sunthud Pornprasertmanit & Terrence D. Jorgensen
### Last updated: 6 March 2026
### Fit index p-value utilities used in dynamic cutoff evaluation for a nested model comparison

#' Find p-values (1 - percentile) for a nested model comparison
#'
#' This function provides \emph{p}-values by comparing the differences in
#' fit indices between nested models with the simulation results of both
#' the parent and nested models when the nested model is true.
#'
#' In comparing fit indices, the \emph{p}-value is the proportion of
#' replications that provide less preference for the nested model
#' (e.g., larger negative difference in CFI values or larger positive
#' difference in RMSEA values) than the analysis result from the
#' observed data.
#'
#' @param outNested A \code{lavaan}-class object saving the analysis result
#' of the nested model from the target dataset.
#' @param outParent A \code{lavaan}-class object saving the analysis result
#' of the parent model from the target dataset.
#' @param simNested A \code{\linkS4class{SimResult}} object containing the
#' analysis results of the nested model from multiple replications.
#' @param simParent A \code{\linkS4class{SimResult}} object containing the
#' analysis results of the parent model from multiple replications.
#' @param usedFit Vector of names of fit indices that researchers wish to
#' obtain cutoffs from. The default is to use all available fit indices.
#' @param nVal The sample size value for which researchers wish to find
#' the \emph{p}-value.
#' @param pmMCARval The percent missing completely at random value for which
#' researchers wish to find the \emph{p}-value.
#' @param pmMARval The percent missing at random value for which researchers
#' wish to find the \emph{p}-value.
#' @param df The degree of freedom used in spline methods for predicting fit
#' indices by predictors. If \code{df = 0}, the spline method is not applied.
#'
#' @return
#' A vector of \emph{p}-values based on the comparison of the difference in
#' fit indices from the observed data with the simulation results.
#'
#' The \emph{p}-values of the requested fit indices are returned along with
#' two additional values:
#'
#' \itemize{
#' \item \code{andRule}: The proportion of replications in which all fit
#' indices indicate a better model than the observed data. This represents
#' the most stringent rule for retaining a hypothesized model.
#'
#' \item \code{orRule}: The proportion of replications in which at least one
#' fit index indicates a better model than the observed data. This represents
#' the most lenient rule for retaining a hypothesized model.
#' }
#'
#' @details
#' The \emph{p}-value is computed as the proportion of replications that
#' provide less preference for the nested model compared with the observed
#' data. For example, this may correspond to larger negative differences
#' in CFI values or larger positive differences in RMSEA values.
#'
#' @seealso
#' \code{\linkS4class{SimResult}}
#'
#' @examples
#' \dontrun{
#' library(lavaan)
#'
#' # Nested Model: Linear growth curve model
#' LY <- matrix(1, 4, 2)
#' LY[, 2] <- 0:3
#' PS <- matrix(NA, 2, 2)
#' TY <- rep(0, 4)
#' AL <- rep(NA, 2)
#' TE <- diag(NA, 4)
#' nested <- estmodel(
#'   LY = LY, PS = PS, TY = TY, AL = AL, TE = TE,
#'   modelType = "CFA", indLab = paste("t", 1:4, sep = "")
#' )
#'
#' # Parent Model: Unconditional growth curve model
#' LY2 <- matrix(1, 4, 2)
#' LY2[, 2] <- c(0, NA, NA, 3)
#' parent <- estmodel(
#'   LY = LY2, PS = PS, TY = TY, AL = AL, TE = TE,
#'   modelType = "CFA", indLab = paste("t", 1:4, sep = "")
#' )
#'
#' # Analyze the output
#' outNested <- analyze(nested, Demo.growth)
#' outParent <- analyze(parent, Demo.growth)
#'
#' # Create data template from the nested model with small misfit
#' loadingMis <- matrix(0, 4, 2)
#' loadingMis[2:3, 2] <- "runif(1, -0.1, 0.1)"
#' datamodel <- model.lavaan(outNested, LY = loadingMis)
#'
#' # Get the sample size
#' n <- nrow(Demo.growth)
#'
#' # The actual replications should be much greater than 30
#' simNestedNested <- sim(30, n = n, nested, generate = datamodel)
#' simNestedParent <- sim(30, n = n, parent, generate = datamodel)
#'
#' # Compare observed fit indices against simulated distributions
#' pValueNested(outNested, outParent, simNestedNested, simNestedParent)
#' }
#'
#' @export
pValueNested <- function(
  outNested, outParent, simNested, simParent, usedFit = NULL,
  nVal = NULL, pmMCARval = NULL, pmMARval = NULL, df = 0
) {
  mod <- clean(simNested, simParent)
  simNested <- mod[[1]]
  simParent <- mod[[2]]
  usedFit <- cleanUsedFit(usedFit, colnames(simNested@fit), colnames(simParent@fit))
  revDirec <- (usedFit %in% getKeywords()$reversedFit) # CFA --> FALSE, RMSEA --> TRUE

  if (!isTRUE(all.equal(unique(simNested@paramValue), unique(simParent@paramValue)))) {
    stop("Models are based on different data and cannot be compared, check your random seed")
  }
  if (!isTRUE(all.equal(unique(simNested@n), unique(simParent@n)))) {
    stop("Models are based on different values of sample sizes")
  }
  if (!isTRUE(all.equal(unique(simNested@pmMCAR), unique(simParent@pmMCAR)))) {
    stop("Models are based on different values of the percent completely missing at random")
  }
  if (!isTRUE(all.equal(unique(simNested@pmMAR), unique(simParent@pmMAR)))) {
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
  Data <- as.data.frame((simNested@fit - simParent@fit)[, usedFit])
  condition <- c(length(unique(simNested@pmMCAR)) > 1, length(unique(simNested@pmMAR)) >
    1, length(unique(simNested@n)) > 1)
  condValue <- cbind(simNested@pmMCAR, simNested@pmMAR, simNested@n)
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
    ifelse(is.null(pmMCARval), stop("Please specify the percent of missing completely at random, 'pmMCARval', because the percent of missing completely at random in the result object is varying"),
      predictorVal[1] <- pmMCARval
    )
  }
  if (condition[2]) {
    ifelse(is.null(pmMARval), stop("Please specify the percent of missing at random, 'pmMARval', because the percent of missing at random in the result object is varying"),
      predictorVal[2] <- pmMARval
    )
  }
  predictorVal <- predictorVal[condition]
  if (inherits(outNested, "MxModel") & inherits(outParent, "MxModel")) {
    cutoff <- fitMeasuresMx(outNested)[usedFit] - fitMeasuresMx(outParent)[usedFit]
  } else if (inherits(outNested, "lavaan") & inherits(outParent, "lavaan")) {
    cutoff <- lavaan::fitMeasures(outNested, fit.measures = usedFit) -
      lavaan::fitMeasures(outParent, fit.measures = usedFit)
  } else if (inherits(outNested, "lavaan.mi") & inherits(outParent, "lavaan.mi")) {
    cutoff <- getMethod("fitMeasures", "lavaan.mi")(outNested, fit.measures = usedFit) -
      getMethod("fitMeasures", "lavaan.mi")(outParent, fit.measures = usedFit)
  } else {
    stop("The 'outNested' and 'outParent' arguments must be both lavaan objects or MxModel objects.")
  }
  if (any(condition)) {
    result <- pValueDataFrame(cutoff, Data, revDirec,
      x = condValue, xval = predictorVal,
      df = df, asLogical = FALSE
    )
    names(result) <- usedFit
    return(result)
  } else {
    logicalMat <- pValueDataFrame(cutoff, Data, revDirec, asLogical = TRUE)
    result <- apply(logicalMat, 2, mean, na.rm = TRUE)
    names(result) <- usedFit
    andRule <- mean(apply(logicalMat, 1, all), na.rm = TRUE)
    orRule <- mean(apply(logicalMat, 1, any), na.rm = TRUE)
    return(c(result, andRule = andRule, orRule = orRule))
  }
}
