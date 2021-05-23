# this is based on the anova function in the lmer/lavaan package

anova.SimResult <- function(object, ..., usedFit = NULL) {
  usedFit <- cleanUsedFit(usedFit, colnames(object@fit))
  if ("df" %in% colnames(object@fit) & "chisq" %in% colnames(object@fit)) {
    usedFit <- c("chisq", "df", setdiff(usedFit, c("chisq", "df")))
  }
  mcall <- match.call(expand.dots = TRUE)
  mod <- clean(object, ...)
  object <- mod[[1]]
  dots <- mod[2:length(mod)]
  modp <- if (length(dots))
    sapply(dots, is, "SimResult") else logical(0)

  # single argument version is not supported (what should be display?)
  if (!any(modp))
    stop("simSEM ERROR: need two models to compare")

  # list of models
  mods <- c(list(object), dots[modp])
  names(mods)[ 1] <- as.character(as.list(mcall)[2])
  names(mods)[-1] <- sapply(substitute(list(...))[-1], deparse)

  # Make sure models come from the same seed else stop and give warning
  nseed <- mods[[1]]@seed[1]
  for (i in 2:length(mods)) {
    nseed <- rbind(nseed, mods[[1]]@seed[1])
  }
  if (any(!duplicated(nseed)[2:length(mods)]))
    stop("simSEM ERROR: Models are based on different data and cannont be compared, check you random seed")

  # put them in order (using number of free parameters) nfreepar <-
  # sapply(lapply(mods, logLik), attr, 'df')
  nfreepar <- mods[[1]]@fit$df[1]
  if(!is.null(nfreepar)) {
    for (i in 2:length(mods)) {
      nfreepar <- c(nfreepar, mods[[i]]@fit$df[1])
    }


    if (any(duplicated(nfreepar)))
      stop("simSEM ERROR: Two models have the same degrees of freedom and cannot be nested")
    ## FIXME: what to do here?  call nonnest2::vuongtest()?

    # what, same number of free parameters?

    # right now we stop things and give a warning.

    # stop('simSEM ERROR: Two models have the same degrees of freedom and cannot be
    # nested')

    # ORDERING DOES NOT WORK RIGHT NOW. Why??
    mods <- mods[order(nfreepar, decreasing = FALSE)]
  }

  if (!multipleAllEqualList(lapply(mods, function(x, name) unique(slot(x, name)),
                                   name = "n")))
    stop("Models are based on different values of sample sizes")
  if (!multipleAllEqualList(lapply(mods, function(x, name) unique(slot(x, name)),
                                   name = "pmMCAR")))
    stop("Models are based on different values of the percent completely missing at random")
  if (!multipleAllEqualList(lapply(mods, function(x, name) unique(slot(x, name)),
                                   name = "pmMAR")))
    stop("Models are based on different values of the percent missing at random")

  nrep <- dim(object@fit)[[1]]
  x <- NULL
  pred <- NULL

  if (length(unique(object@n)) > 1) {
    if (!length(object@n) == nrep) {
      stop("Number of random sample sizes is not the same as the number of replications, check to see if N varied across replications")
    }
    x <- cbind(x, object@n)
    pred$N <- unique(round(seq(min(object@n), max(object@n), length.out = 20)))
  }
  if (length(unique(object@pmMCAR)) > 1) {
    if (!length(object@pmMCAR) == nrep) {
      stop("Number of random pmMCARs is not the same as the number of replications, check to see if pmMCAR varied across replications")
    }
    x <- cbind(x, object@pmMCAR)
    pred$MCAR <- seq(min(object@pmMCAR), max(object@pmMCAR), length.out = 20)

  }
  if (length(unique(object@pmMAR)) > 1) {
    if (!length(object@pmMAR) == nrep) {
      stop("Number of random pmMARs is not the same as the number of replications, check to see if pmMAR varied across replications")
    }
    x <- cbind(x, object@pmMAR)
    pred$MAR <- seq(min(object@pmMAR), max(object@pmMAR), length.out = 20)

  }
  # Need to pull fit statistics from each model, compare each one...

  # Use apply and diff function to get differneces for each rows

  # collect statistics for each model
  modsout <- lapply(mods, function(x) slot(x, "fit")[,usedFit])
  mat <- list()
  for(i in seq_along(usedFit)) {
    mat[[i]] <- sapply(modsout, function(x) x[,usedFit[i]])
  }
  names(mat) <- usedFit
  matDelta <- lapply(mat, function(x) apply(x, 1, diff))

  val <- sapply(mat, colMeans)
  # rownames(val) <- paste("Object", 1:nrow(val))
  if (length(mods) == 2) {
    diffStats <- sapply(matDelta, mean)
    names(diffStats) <- paste(names(diffStats), "diff")

    # Power of test. 0 = not significant, 1 = sig.
    if ("chisq" %in% names(mat) && "df" %in% names(mat)) {
      Chi.delta <- matDelta$chisq
      Df.delta <- matDelta$df
      diffStats <- diffStats[!(names(mat) %in% c("chisq", "df"))]
      Power.delta <- pchisq(Chi.delta, Df.delta, lower.tail = FALSE) < 0.05
      diffStats <- c("chisq diff" = mean(Chi.delta), "df diff" = mean(Df.delta), "power.05 diff" = mean(Power.delta), diffStats)
    }

  } else if (length(mods) > 2) {
    diffStats <-  sapply(matDelta, rowMeans)
    colnames(diffStats) <- paste(colnames(diffStats), "diff")
    rownames(diffStats) <- paste(names(mods)[2:length(mods)], "vs.",
                                 names(mods)[-length(mods)])

    # Power of test at alpha=.05
    # 0 = not significant, 1 = sig.
    if ("chisq" %in% names(mat) && "df" %in% names(mat)) {
      Power.delta <- numeric(nrow(diffStats))
      for (pair in 1:nrow(diffStats)) {
        Chi.delta <- matDelta$chisq[pair,]
        Df.delta <- matDelta$df[pair,]
        Power.delta[pair] <- mean(pchisq(Chi.delta, Df.delta, lower.tail = FALSE) < 0.05)
      }
      diffStats <- cbind(diffStats[,c("chisq diff", "df diff")],
                         "power.05 diff" = Power.delta,
                         diffStats[,!(names(mat) %in% c("chisq", "df"))])
    }
  } else stop('anova() requires >= 2 SimResult objects')


  varyResult <- NULL

  if (!is.null(x) && ("chisq" %in% names(mat) & "df" %in% names(mat))) {
    Chi.delta <- as.matrix(matDelta$chisq) #FIXME: with > 2 models, already a matrix
    Df.delta <- as.matrix(matDelta$df)
    temp <- list()
    # Varying parameters
    ivVal <- expand.grid(pred)
    powVal <- as.list(data.frame(t(ivVal)))
    for (i in 1:ncol(Chi.delta)) {
      temp[[i]] <- sapply(powVal, pValueVariedCutoff,
                          cutoff = rep(qchisq(0.95, df = Df.delta[1, i])),
                          obtainedValue = Chi.delta[, i], revDirec = FALSE, x = x)
    }
    temp <- data.frame(temp)
    varyResult <- data.frame(ivVal, temp)
    colnames(varyResult) <- c(names(pred), paste("power.", 1:ncol(temp), sep = ""))
    rownames(varyResult) <- NULL
  }

  val <- as.data.frame(val)
  class(val) <- c("lavaan.data.frame","data.frame")
  attr(val, "header") <- "Fit Measures per Model:"

  diffStats <- as.data.frame(diffStats)
  class(diffStats) <- c("lavaan.data.frame","data.frame")
  attr(diffStats, "header") <- "Nested Model Comparisons:"

  result <- list(summary = val, diff = diffStats, varyParam = varyResult)
  return(result)

  # Return List If n, pmMCAR, pmMAR are varying, return the additional arguments
  # in the list that provide the conditional median of the varying parameter.
}

setMethod("anova", "SimResult", anova.SimResult)
