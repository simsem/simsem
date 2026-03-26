### Sunthud Pornprasertmanit; with contributions by Terrence D. Jorgensen
### Last updated: 5 March 2026
### combine multiple SimResult objects into a single result object

#' Combine simulation results
#'
#' Combines multiple \code{SimResult} objects into a single object by
#' stacking results from multiple simulation replications. This is useful when simulation
#' results are generated in separate runs (e.g., parallel jobs on an HPC
#' cluster) and need to be merged into a single result object for
#' analysis.
#'
#' The function checks that all input objects are of class
#' \code{SimResult} and that they correspond to the same model type.
#' Simulation outputs (parameter estimates, fit statistics, convergence
#' indicators, and other stored results) are concatenated across
#' replications.
#'
#' @param ... One or more \code{SimResult} objects to combine.
#'
#' @details
#' This function is typically used when simulation replications are
#' distributed across multiple jobs and saved separately. The resulting
#' objects can then be combined into a single \code{SimResult} object for
#' summarization and analysis.
#'
#' The function performs several consistency checks:
#' \itemize{
#' \item All inputs must be \code{SimResult} objects.
#' \item All models must have the same \code{modelType}.
#' \item Seeds are checked to warn about potential overlap.
#' }
#'
#' Parameter estimates, standard errors, fit indices, convergence
#' indicators, and other stored values are concatenated across
#' replications. Timing information is also combined appropriately.
#'
#' @return
#' A single \code{SimResult} object containing all replications from the
#' input objects.
#'
#' @seealso
#' \code{\link{SimResult-class}}, \code{\link{summary}}, \code{\link{summaryShort}}
#'
#' @examples
#' \dontrun{
#' result1 <- sim(...)
#' result2 <- sim(...)
#'
#' combined <- combineSim(result1, result2)
#' }
#'
#' @export
combineSim <- function(...) {
  s4list <- list(...)
  if (!all(sapply(s4list, is, "SimResult"))) {
    stop("This function only combines objects of S4 class 'SimResult' ")
  }
  ## check whether seeds are all unique, remind user it might invalidate results
  if (multipleAnyEqualList(lapply(s4list, slot, name = "seed"))) {
    warning("Some result objects have common seed number.")
  }

  ## check that all models are the same type
  mT <- sapply(s4list, function(dat) slot(dat, "modelType"))
  if (length(unique(mT)) > 1) {
    stop("Model Types are not identical. Do not combine SimResults from structurally different models.")
  } else {
    mT <- mT[1]
  }

  nRep <- sum(sapply(s4list, function(dat) dat@nRep))
  paramOnly <- any(sapply(s4list, function(dat) dat@paramOnly))

  ## function to stack data frames that may have nrows == 0 in some conditions
  stackEm <- function(dat, mySlot) {
    if (nrow(slot(dat, mySlot)) == 0) {
      DF <- data.frame()
    } else {
      DF <- slot(dat, mySlot)
    }
    return(DF)
  }

  ## function to combine data frames
  coef <- do.call("rbind", lapply(s4list, stackEm, "coef"))
  se <- do.call("rbind", lapply(s4list, stackEm, "se"))
  fit <- do.call("rbind", lapply(s4list, stackEm, "fit"))
  misspecValue <- do.call("rbind", lapply(s4list, stackEm, "misspecValue"))
  popFit <- do.call("rbind", lapply(s4list, stackEm, "popFit"))
  FMI1 <- do.call("rbind", lapply(s4list, stackEm, "FMI1"))
  FMI2 <- do.call("rbind", lapply(s4list, stackEm, "FMI2"))
  cilower <- do.call("rbind", lapply(s4list, stackEm, "cilower"))
  ciupper <- do.call("rbind", lapply(s4list, stackEm, "ciupper"))
  stdCoef <- do.call("rbind", lapply(s4list, stackEm, "stdCoef"))
  stdSe <- do.call("rbind", lapply(s4list, stackEm, "stdSe"))
  nobs <- do.call("rbind", lapply(s4list, stackEm, "nobs"))

  if (length(misspecValue) > 0 && all(is.na(misspecValue))) {
    misspecValue <- data.frame(V1 = NA)
  }
  if (all(is.na(popFit))) popFit <- data.frame(V1 = NA)

  ## function to stack paramValues so nrows == nReps, (unless it already is, e.g. random parameters)
  stackParams <- function(dat) {
    if (nrow(dat@paramValue) == 1) {
      paramVec <- dat@paramValue
      for (i in 2:dat@nRep) paramVec <- rbind(paramVec, dat@paramValue)
      return(paramVec)
    } else {
      return(dat@paramValue)
    }
  }
  pv <- do.call("rbind", lapply(s4list, stackParams))
  if (nrow(unique(pv)) == 1) pv <- unique(pv)

  ## SP: FIX BUGS: stdpv should be the stacked standardized parameters
  stackStdParams <- function(dat) {
    if (nrow(dat@stdParamValue) == 1) {
      paramVec <- dat@stdParamValue
      for (i in 2:dat@nRep) paramVec <- rbind(paramVec, dat@stdParamValue)
      return(paramVec)
    } else {
      return(dat@stdParamValue)
    }
  }
  stdpv <- do.call("rbind", lapply(s4list, stackStdParams))
  if (nrow(unique(stdpv)) == 1) stdpv <- unique(stdpv)

  ## save vectors. If single values, save them as vectors to match nReps rows in data.frames
  converged <- do.call("c", lapply(s4list, function(dat) dat@converged))
  seed <- s4list[[length(s4list)]]@seed
  n <- do.call("c", lapply(s4list, function(dat) rep(dat@n, length.out = dat@nRep)))
  pmMCAR <- do.call("c", lapply(s4list, function(dat) rep(dat@pmMCAR, length.out = dat@nRep)))
  pmMAR <- do.call("c", lapply(s4list, function(dat) rep(dat@pmMAR, length.out = dat@nRep)))

  ## combine lists

  ### need nRep empty slots
  extraOut <- do.call("c", lapply(s4list, function(dat) dat@extraOut))

  ### add list elements (always same order)
  FUN <- function(timing1, timing2) mapply("+", timing1, timing2, SIMPLIFY = FALSE)
  timing <- Reduce(FUN, lapply(s4list, function(dat) dat@timing[-which(names(dat@timing) %in% c("StartTime", "EndTime"))]))

  inreps <- lapply(s4list, function(dat) dat@timing$InReps)
  nreps <- lapply(s4list, function(dat) dat@nRep)
  totaltime <- mapply("*", inreps, nreps, SIMPLIFY = TRUE)
  totaltime <- apply(totaltime, 1, sum)
  timing$InReps <- totaltime / nRep
  timing$StartTime <- Reduce(min, lapply(s4list, function(dat) dat@timing$StartTime))
  timing$EndTime <- Reduce(max, lapply(s4list, function(dat) dat@timing$EndTime))

  ## store in single S4 SimResult object, which is the return value of this function
  output <- new("SimResult",
    modelType = mT, nRep = nRep, coef = coef, se = se,
    fit = fit, converged = converged, paramValue = pv, stdParamValue = stdpv,
    misspecValue = misspecValue, popFit = popFit, FMI1 = FMI1,
    FMI2 = FMI2, cilower = cilower, ciupper = ciupper, stdCoef = stdCoef, stdSe = stdSe, seed = seed, n = n, nobs = nobs,
    pmMCAR = pmMCAR, pmMAR = pmMAR, extraOut = extraOut, timing = timing, paramOnly = paramOnly
  )
  # nobs, paramOnly
  output
}
