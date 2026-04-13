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
#' @importFrom utils head
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
  ## check whether seeds are all unique. If not, check any indices are run twice.
  seeds <- lapply(s4list, slot, name = "seed")
  repRuns <- lapply(s4list, slot, name = "repRun")

  ## check whether all seeds are identical (HPC split case)
  sameSeed <- length(unique(lapply(seeds, paste, collapse = ","))) == 1

  ## check whether repRun are identical across objects
  repRunStrings <- lapply(repRuns, paste, collapse = ",")
  sameRepRun <- length(unique(repRunStrings)) == 1

  if (!sameSeed && !sameRepRun) {
    stop(
      "Seeds differ and repRun also differ across SimResult objects. ",
      "This is unexpected: unequal repRun is only allowed when all objects share the same seed ",
      "(i.e., an HPC split of one common simulation run)."
    )
  }

  if (sameSeed) {
    allRepRun <- unlist(repRuns)

    ## 1. no overlap
    if (anyDuplicated(allRepRun)) {
      stop("Overlapping replication indices (repRun) detected among objects with identical seeds.")
    }

    ## 2. each object must have internally valid repRun
    invalid <- sapply(repRuns, function(r) {
      if (length(r) <= 1) return(FALSE)

      r_sorted <- sort(r)
      !identical(r, r_sorted) || any(diff(r_sorted) != 1)
    })

    if (any(invalid)) {
      stop("Each SimResult must have repRun sorted and increasing by 1 (e.g., 1:50, 51:100).")
    }

    ## 3. sort objects
    s4list <- s4list[order(sapply(s4list, function(dat) min(dat@repRun)))]
  }

  ## check that all models are the same type
  mT <- sapply(s4list, function(dat) slot(dat, "modelType"))
  if (length(unique(mT)) > 1) {
    stop("Model Types are not identical. Do not combine SimResults from structurally different models.")
  } else {
    mT <- mT[1]
  }

  allRepRun <- unlist(repRuns)
  ## nRep = total number of pooled replications
  ## (duplicates allowed when seeds differ)
  nRep <- length(allRepRun)

  if (sameSeed) {
    nRepDesign <- unique(sapply(s4list, function(dat) dat@nRep))

    if (length(nRepDesign) != 1) {
      stop("Objects with identical seeds have different nRep values.")
    }

    allRepRun_sorted <- sort(allRepRun)
    expected <- seq_len(nRepDesign)
    missingRep <- setdiff(expected, allRepRun_sorted)

    if (length(missingRep) > 0) {
      warning(
        sprintf(
          "Some replications are missing from the pooled result. Expected repRun 1:%d, but %d replication(s) are missing (e.g., %s).",
          nRepDesign,
          length(missingRep),
          paste(head(missingRep, 5), collapse = ", ")
        )
      )
    }
  }

  paramOnly <- any(sapply(s4list, function(dat) dat@paramOnly))

  ## function to stack data frames that may have nrows == 0 in some conditions
  stackEm <- function(dat, mySlot) {
    DF <- slot(dat, mySlot)

    if (is.null(DF) || nrow(DF) == 0) {
      return(data.frame())
    }

    DF
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

  if (nrow(misspecValue) > 0 && all(is.na(misspecValue))) {
    misspecValue <- data.frame()
  }
  if (all(is.na(popFit))) popFit <- data.frame()

  ## function to stack paramValues so nrows == nReps, (unless it already is, e.g. random parameters)
  stackParams <- function(dat) {
    nLocal <- length(dat@repRun)
    DF <- dat@paramValue

    if (nrow(DF) == 1) {
      DF <- DF[rep(1, nLocal), , drop = FALSE]
    }

    DF
  }

  pv <- do.call("rbind", lapply(s4list, stackParams))

  if (nrow(unique(pv)) == 1) pv <- unique(pv)

  ## same as stackParams
  stackStdParams <- function(dat) {
    nLocal <- length(dat@repRun)
    DF <- dat@stdParamValue

    if (nrow(DF) == 1) {
      DF <- DF[rep(1, nLocal), , drop = FALSE]
    }

    DF
  }

  stdpv <- do.call("rbind", lapply(s4list, stackStdParams))

  if (nrow(unique(stdpv)) == 1) stdpv <- unique(stdpv)

  ## save vectors. If single values, save them as vectors to match nReps rows in data.frames
  converged <- do.call("c", lapply(s4list, function(dat) dat@converged))
  seed <- s4list[[length(s4list)]]@seed # SP: Intentional use 'seed' of the last object (if runRep is used, seed is the same and it is okay to use this.)

  # Account for repRun, single/multiple value of n/pmMCAR/pmMAR
  expandVec <- function(x, nLocal) {
    if (length(x) == 1) {
      rep(x, nLocal)
    } else if (length(x) == nLocal) {
      x
    } else {
      stop("Length of vector does not match repRun.")
    }
  }

  n <- NULL
  len_n <- sapply(s4list, function(dat) length(dat@n))
  if(all(len_n == nRep)) {
    all_same_n <- all(sapply(s4list[-1], function(dat) {
      isTRUE(all.equal(dat@n, s4list[[1]]@n))
    }))
    if(all_same_n) {
      n <- s4list[[1]]@n
    } else {
      stop("The 'n' slots are not equal across all SimResult objects.")
    }
  } else {
    n <- do.call("c", lapply(s4list, function(dat) {
      expandVec(dat@n, length(dat@repRun))
    }))
  }


  nobs <- NULL
  len_nobs <- sapply(s4list, function(dat) nrow(dat@nobs))
  if(all(len_nobs == nRep)) {
    all_same_nobs <- all(sapply(s4list[-1], function(dat) {
      isTRUE(all.equal(dat@nobs, s4list[[1]]@nobs))
    }))
    if(all_same_nobs) {
      nobs <- s4list[[1]]@nobs
    } else {
      stop("The 'nobs' slots are not equal across all SimResult objects.")
    }
  } else {
    nobs <- do.call("rbind", lapply(s4list, stackEm, "nobs"))
  }

  pmMCAR <- do.call("c", lapply(s4list, function(dat) {
    expandVec(dat@pmMCAR, length(dat@repRun))
  }))

  pmMAR <- do.call("c", lapply(s4list, function(dat) {
    expandVec(dat@pmMAR, length(dat@repRun))
  }))

  allEmpty <- all(sapply(s4list, function(dat) length(dat@extraOut) == 0))

  if (allEmpty) {
    extraOut <- list()
  } else {
    extraOut <- do.call("c", lapply(s4list, function(dat) {
      if (length(dat@extraOut) != length(dat@repRun)) {
        stop("Length of extraOut does not match repRun.")
      }
      dat@extraOut
    }))
  }

  ### add list elements (always same order)
  FUN <- function(timing1, timing2) mapply("+", timing1, timing2, SIMPLIFY = FALSE)
  timing <- Reduce(FUN, lapply(s4list, function(dat) dat@timing[setdiff(names(dat@timing), c("StartTime", "EndTime"))]))
  inreps <- lapply(s4list, function(dat) dat@timing$InReps)
  nrepseach <- sapply(s4list, function(dat) length(dat@repRun))
  totaltime <- Reduce(`+`, Map(function(x, n) x * n, inreps, nrepseach))
  timing$InReps <- totaltime / sum(nrepseach)
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
