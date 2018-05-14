### Sunthud Pornprasertmanit & Terrence D. Jorgensen (anyone else?)
### Last updated: 14 May 2018
### Primary engines for simulation.  Everything else is added details.

sim <- function(nRep = NULL, model = NULL, n = NULL, generate = NULL, ...,
                rawData = NULL, miss = NULL, datafun = NULL, lavaanfun = "lavaan",
                outfun = NULL, outfundata = NULL, pmMCAR = NULL, pmMAR = NULL,
                facDist = NULL, indDist = NULL, errorDist = NULL, sequential = FALSE,
                saveLatentVar = FALSE, modelBoot = FALSE, realData = NULL,
                covData = NULL, maxDraw = 50, misfitType = "f0", misfitBounds = NULL,
                averageNumMisspec = FALSE, optMisfit = NULL, optDraws = 50,
                createOrder = c(1, 2, 3), aux = NULL, group = NULL, mxFit = FALSE,
                mxMixture = FALSE, citype = NULL, cilevel = 0.95, seed = 123321,
                silent = FALSE, multicore = options('simsem.multicore')[[1]],
                cluster = FALSE, numProc = NULL, paramOnly = FALSE, dataOnly = FALSE,
                smartStart = FALSE, previousSim = NULL, completeRep = FALSE,
                stopOnError = FALSE) {
  mc <- match.call()
	## Update function. Takes results object. Or takes model object.
	## Speed things: functions in C or C++ (maybe drawparam), look at sugar functions
	## Difference percent missing in different groups
  start.time0 <- start.time <- proc.time()[3]
  timing <- list()
	timing$StartTime <- Sys.time()
  RNGkind("L'Ecuyer-CMRG")
	if (is.null(previousSim)) {
		set.seed(seed)
	} else {
		assign(".Random.seed", as.integer(previousSim@seed[-1]), envir = .GlobalEnv)
	}

	isPopulation <- FALSE
	popData <- NULL
	if(!is.null(rawData)) {
		if(!is.null(nRep) & !is.null(n)) {
			isPopulation <- TRUE
			popData <- rawData
			rawData <- NULL
		}
	}

	lavaanGenerate <- FALSE
	mxGenerate <- FALSE
	functionGenerate <- FALSE
	if (!is.null(generate)) {
		if (is.character(generate)) {
			generate <- list(model = generate)
			lavaanGenerate <- TRUE
		} else if (is.partable(generate)) {
			generate <- list(model = generate)
			lavaanGenerate <- TRUE
		} else if (is.lavaancall(generate)) {
			lavaanGenerate <- TRUE
		} else if (is(generate, "lavaan")) {
			temp <- parTable(generate)
			temp$ustart <- temp$est
			generate <- list(model = temp)
			lavaanGenerate <- TRUE
		} else if (is(generate, "MxModel")) {
			mxGenerate <- TRUE
		} else if (is(generate, "function")) {
			functionGenerate <- TRUE
		} else if (is(generate, "SimSem")) {
			## Do nothing
		} else {
			stop("Please specify an appropriate object for the 'generate' argument: ",
			     "simsem model template, lavaan script, lavaan parameter table, ",
			     "OpenMx object, a list of options for the 'simulateData' function, ",
			     "or a function that takes sample size and provides data as an output.")
		}
	}

	if ((lavaanGenerate | mxGenerate | functionGenerate) & (sequential | saveLatentVar)) {
		stop("The sequential or saveLatentVar features are supported by only the ",
		     "data generation using the simsem template.")
	}

	lavaanAnalysis <- FALSE
	mxAnalysis <- FALSE
	functionAnalysis <- FALSE
	if (is.character(model)) {
		model <- list(model = model)
		lavaanAnalysis <- TRUE
	} else if (is.partable(model)) {
		model <- list(model = model)
		lavaanAnalysis <- TRUE
	} else if (is.lavaancall(model)) {
		lavaanAnalysis <- TRUE
	} else if (is(model, "lavaan")) {
		model <- list(model = parTable(model))
		lavaanAnalysis <- TRUE
	} else if (is(model, "MxModel")) {
		mxAnalysis <- TRUE
	} else if (is(model, "SimSem")) {
		## Do nothing
	} else if (is(model, "function")) {
		functionAnalysis <- TRUE
	} else {
		stop("Please specify an appropriate object for the 'model' argument: ",
		     "simsem model template, lavaan script, lavaan parameter table, list of ",
		     "options for the 'lavaan' function, or a function written to analyze data.")
	}

	if (lavaanAnalysis) {
		model <- c(model, list(...))
		if (!("group" %in% names(model)) & "group" %in% names(mc)) model$group <- group
	}

	if (mxAnalysis) {
		if (length(model@submodels) > 0) {
			if (!is(model@submodels[[1]]@expectation, "MxExpectationRAM") && all(is.na(model@submodels[[1]]@expectation@means))) {
			  stop("The expected means must be specified in the objective of the 'model' argument.")
			}
		} else {
			if (!is(model@expectation, "MxExpectationRAM") && all(is.na(model@expectation@means))) {
			  stop("The expected means must be specified in the objective of the 'model' argument.")
			}
		}
	}

	## Save arguments for completeRep = TRUE
	if (is.logical(completeRep)) {
		if (completeRep) {
			completeRep <- nRep
		} else {
			completeRep <- 0L
		}
	}
	nInitial <- n
	pmMCARInitial <- pmMCAR
	pmMARInitial <- pmMAR
	nRepInitial <- nRep

  #### 1. Set up correct data generation template (move inside the runRep).


	## Find the number of groups
	## Change in draw param so we always have a nested list (even with 1 group).
	## Then get rid of the if/else.
	ngroups <- 1L
	if (!is.null(generate)) {
		if (lavaanGenerate) {
			if (is.partable(generate$model)) {
				ngroups <- max(generate$model$group)
			} else {
				if (is.list(n)) ngroups <- length(n)
			}
		} else if (mxGenerate) {
			ngroups <- length(generate@submodels)
			if(ngroups == 0) ngroups <- 1
		} else if (functionGenerate) {
			if(is.list(n)) ngroups <- length(n)
		} else {
			ngroups <- max(generate@pt$group)
		}
	} else {
		if (lavaanAnalysis) {
			if (is.partable(model$model)) {
				ngroups <- max(model$model$group)
			} else {
				if (!is.null(rawData) && !is.null(model$group)) {
					if (is.data.frame(rawData) || is.matrix(rawData)) {
						ngroups <- length(unique(rawData[ , model$group]))
					} else {
						ngroups <- length(unique(rawData[[1]][ , model$group]))
					}
				} else {
					if (is.list(n)) ngroups <- length(n)
				}
			}
		} else if (mxAnalysis) {
			ngroups <- length(model@submodels)
			if (ngroups == 0) ngroups <- 1L
		} else if (functionAnalysis) {
			ngroups <- 1L # 1 because to get this far, only raw data are provided. The number of group will does not matter.
		} else {
			ngroups <- max(model@pt$group)
		}
	}

  timing$SimulationParams <- proc.time()[3] - start.time0
  start.time <- proc.time()[3]

  #### 2. Compute full-factorial combinations of simulation parameters (MAR, MCAR, n)


	if (is.null(rawData)) {
		## If the rawData is not specified, the n value must be valid.
		if (!is.list(n)) {
			## If multiple groups then...
			## Make n as a list to represent sample size of each group
			if (length(n) == 1L && !is.null(nRep)) {
			## For a single specified value of n, each rep has the same n
				n <- rep(n, nRep)
			}
			## Make as a list, then the same sample size for all groups
			n <- list(n)
			n <- rep(n, ngroups)
		} else {
			## If n is a list, thus multiple groups, make sure that the length is equal.
			## Length for each group can be 1 or nRep
			if (length(unique(sapply(n, length))) != 1L) {
				stop("You must specify the same number of sample sizes for each group; ",
				     "each list component in the 'n' argument must have the same length.")
			} else {
			## This is for when there is a single sample size for all reps.
				if (length(n[[1]]) == 1L && !is.null(nRep)) {
					n <- lapply(n, rep, nRep)
				}
			}
		}
		## If the nRep is not NULL, the length of sample size currently must equal nRep
		if (!is.null(nRep) && (length(n[[1]]) != nRep)) {
		  stop("When the number of replications argument 'nRep' is used, sample ",
		       "size 'n' must be a single value, a vector with length equal to ",
		       "'nRep', or a vector with length equal to a multiple of 'nRep'")
		}
		if (is.null(nRep)) {
			if (!is.null(pmMCAR) && !is.vector(pmMCAR))
				stop("Please specify the number of replications 'nRep'")
			if (!is.null(pmMAR) && !is.vector(pmMAR))
				stop("Please specify the number of replications 'nRep'")

			usedMCAR <- NULL
			usedMAR <- NULL
			usedMCAR <- if (is.null(pmMCAR)) 0 else pmMCAR
			usedMAR <- if (is.null(pmMAR)) 0 else pmMAR

			out <- expand.grid(1:length(n[[1]]), usedMCAR, usedMAR)

			if (!is.null(pmMCAR)) pmMCAR <- out[, 2]
			if (!is.null(pmMAR)) pmMAR <- out[, 3]
			n <- lapply(n, function(x, y) x[y], y = out[ , 1])
			nRep <- nrow(out)
		}
	} else {
		nRep <- length(rawData)
		if (!is.null(n)) {
			warning("The sample size argument 'n' is suppressed when 'rawData' is specified")
			n <- NULL
		}
	}

  #### 3. Adjust pmMCAR and pmMAR

	if (!is.null(pmMCAR)) {
		## If there is one sample size, pmMCAR and pmMAR specified
		if (length(pmMCAR) == 1L) {
			pmMCAR <- rep(pmMCAR, nRep)
		} else if (length(pmMCAR) == nRep) {
			## Do nothing
		} else {
			stop("When the 'rawData' argument is specified, the percent missing ",
			     "completely at random 'pmMCAR' must either be a single value, or ",
			     "a vector with length equal to the number of datasets")
		}
	}
	if (!is.null(pmMAR)) {
		if(length(pmMAR) == 1L) {
			pmMAR <- rep(pmMAR, nRep)
		} else if (length(pmMAR) == nRep) {
			## Do nothing
		} else {
			stop("When the 'rawData' argument is specified, the percent missing ",
			     "at random 'pmMCAR' must either be a single value, or a vector ",
			     "with length equal to the number of datasets")
		}
	}

  timing$RandomSimParams <- (proc.time()[3] - start.time)
  start.time <- proc.time()[3]

  #### 4. Build list of simulation conditions. Each element of simConds is a rep.

  simConds <- list()

  numseed <- list()
  s <- .Random.seed
  for (i in 1:nRep) {
    numseed[[i]] <- s
    s <- parallel::nextRNGStream(s)
  }
	## Save last seed numseed[[nRep]] to result object to use for update method?

  if (is.null(rawData)) {

    for (i in seq_len(nRep)) {
      simConds[[i]] <- list()
      simConds[[i]][[1]] <- NULL 				      # Rawdata
      simConds[[i]][[2]] <- sapply(n, "[", i) # Sample Size
      simConds[[i]][[3]] <- pmMCAR[i] 		    # % Missing Completely at Random
      simConds[[i]][[4]] <- pmMAR[i] 			    # % Missing at random
      simConds[[i]][[5]] <- numseed[[i]] 		  # L'Ecuyer random seed
      simConds[[i]][[6]] <- FALSE				      # Skip impose missing values
    }
    if (isPopulation) {
      if (is.matrix(popData)) {
        popData <- data.frame(popData)
      }

      simConds[[nRep + 1]] <- list()
      simConds[[nRep + 1]][[1]] <- NULL
      simConds[[nRep + 1]][[2]] <- NULL
      simConds[[nRep + 1]][[3]] <- 0
      simConds[[nRep + 1]][[4]] <- 0
      simConds[[nRep + 1]][[5]] <- s
      simConds[[nRep + 1]][[6]] <- TRUE
    }
  } else if (is.list(rawData)) { ## FIXME: what if rawData is a single data.frame (also a list)?
    if (is.data.frame(rawData[[1]])) {
      ## Do nothing
    } else if (is.matrix(rawData[[1]])) {
      rawData <- lapply(rawData, data.frame)
    } else {
      stop("Check the list object specified in the 'rawData' argument; ",
           "list must either contain matrices or data frames")
    }

    for (i in seq_along(rawData)) {
      simConds[[i]] <- list()
      simConds[[i]][[1]] <- rawData[[i]]
      simConds[[i]][[2]] <- NA
      simConds[[i]][[3]] <- pmMCAR[i]
      simConds[[i]][[4]] <- pmMAR[i]
      simConds[[i]][[5]] <- numseed[[i]]
      simConds[[i]][[6]] <- FALSE
    }
  } else {
    stop("Check the object specified in 'rawData' argument; object must ",
         "either be a SimData class or a list of data frames.")
  }

  timing$SimConditions <- (proc.time()[3] - start.time)
  start.time <- proc.time()[3]

  ## Suppress warnings starting from here!
  warnT <- as.numeric(options("warn"))
  if (silent) options(warn = -1)

  #### 5. Run replications
  if (multicore) {
    if (!silent) cat("Progress tracker is not available when 'multicore' is TRUE.\n")
    sys <- .Platform$OS.type
    if (is.null(numProc)) numProc <- parallel::detectCores() ## FIXME: subtract 1 for master node?
    if (sys == "windows") {
      cl <- parallel::makeCluster(rep("localhost", numProc), type = "SOCK")
      ########### FIXME: models don't converge using next line
      Result.l <- parallel::clusterApplyLB(cl, simConds, runRep, model = model,
                                           generateO = generate, miss = miss,
                                           datafun = datafun, lavaanfun = lavaanfun,
                                           outfun = outfun, outfundata = outfundata,
                                           silent = silent, facDist = facDist,
                                           indDist = indDist, errorDist = errorDist,
                                           sequential = sequential,
                                           saveLatentVar = saveLatentVar,
                                           realData = realData, covData = covData,
                                           maxDraw = maxDraw, misfitBounds = misfitBounds,
                                           averageNumMisspec = averageNumMisspec,
                                           optMisfit = optMisfit, optDraws = optDraws,
                                           createOrder = createOrder,
                                           misfitType = misfitType, aux = aux,
                                           paramOnly = paramOnly, dataOnly = dataOnly,
                                           smartStart = smartStart, popData = popData,
                                           group = group, mxFit = mxFit,
                                           mxMixture = mxMixture, citype = citype,
                                           cilevel = cilevel, stopOnError = stopOnError, ...)
      parallel::stopCluster(cl)
    } else {
      Result.l <- parallel::mclapply(simConds, runRep, model = model,
                                     generateO = generate, miss = miss,
                                     datafun = datafun, lavaanfun = lavaanfun,
                                     outfun = outfun, outfundata = outfundata,
                                     silent = silent, facDist = facDist,
                                     indDist = indDist, errorDist = errorDist,
                                     sequential = sequential,
                                     saveLatentVar = saveLatentVar,
                                     realData = realData, covData = covData,
                                     maxDraw = maxDraw, misfitBounds = misfitBounds,
                                     averageNumMisspec = averageNumMisspec,
                                     optMisfit = optMisfit, optDraws = optDraws,
                                     createOrder = createOrder,
                                     misfitType = misfitType, aux = aux,
                                     paramOnly = paramOnly, dataOnly = dataOnly,
                                     smartStart = smartStart, popData = popData,
                                     group = group, mxFit = mxFit,
                                     mxMixture = mxMixture, citype = citype,
                                     cilevel = cilevel, stopOnError = stopOnError,
                                     mc.cores = numProc, ...)
    }
  } else {
    numJobs <- length(simConds)

    Result.l <- lapply(1:length(simConds), function(i, ...) {
      ## Write progress
      if (!silent) cat("Progress:", i, "/", numJobs, "\n")
      runRep(simConds[[i]], ...)
    }, model = model, generateO = generate, miss = miss, datafun = datafun,
    lavaanfun = lavaanfun, outfun = outfun, outfundata = outfundata,
    silent = silent, facDist = facDist, indDist = indDist, errorDist = errorDist,
    sequential = sequential, saveLatentVar = saveLatentVar, realData = realData,
    covData = covData, maxDraw = maxDraw, misfitBounds = misfitBounds,
    averageNumMisspec = averageNumMisspec, optMisfit = optMisfit,
    optDraws = optDraws, createOrder = createOrder, misfitType = misfitType,
    aux = aux, paramOnly = paramOnly, dataOnly = dataOnly, smartStart = smartStart,
    popData = popData, group = group, mxFit = mxFit, mxMixture = mxMixture,
    citype = citype, cilevel = cilevel, stopOnError = stopOnError, ...)
  }

  ################## Extract out popData ##################################

  popResult <- NULL
  if (isPopulation) {
    popResult <- Result.l[[nRep + 1]]
    Result.l[[nRep + 1]] <- NULL
  }

  ## Return data when dataOnly is requested
  if (dataOnly) return(Result.l)

  ## Now we create a SimResult object
  timing.l <- lapply(Result.l, function(x) x$timing)
  timing$InReps <- colMeans(matrix(unlist(timing.l), nrow = nRep, byrow = TRUE))
  names(timing$InReps) <- names(timing.l[[1]])

  timing$RunReplications <- (proc.time()[3] - start.time)
  start.time <- proc.time()[3]


  #### 6. Extract results from replication lists
  fit.l       <- lapply(Result.l, function(rep) rep$fit)
  coef.l      <- lapply(Result.l, function(rep) rep$coef)
  se.l        <- lapply(Result.l, function(rep) rep$se)
  converged.l <- lapply(Result.l, function(rep) rep$converged)
  param.l     <- lapply(Result.l, function(rep) rep$param)
  stdparam.l  <- lapply(Result.l, function(rep) rep$stdparam)
  FMI1.l      <- lapply(Result.l, function(rep) rep$FMI1)
  FMI2.l      <- lapply(Result.l, function(rep) rep$FMI2)
  cilower.l   <- lapply(Result.l, function(rep) rep$cilower)
  ciupper.l   <- lapply(Result.l, function(rep) rep$ciupper)
  std.l       <- lapply(Result.l, function(rep) rep$std)
  stdse.l     <- lapply(Result.l, function(rep) rep$stdse)
  extra <- list()
  if (!is.null(outfun) || !is.null(outfundata)) {
    extra     <- lapply(Result.l, function(rep) rep$extra)
  }
  popMis.l    <- lapply(Result.l, function(rep) rep$popMis)
  misfitOut.l <- lapply(Result.l, function(rep) rep$misfitOut)

  coef <- as.data.frame(do.call(rbind, coef.l))
  se   <- as.data.frame(do.call(rbind, se.l))
  fit  <- as.data.frame(do.call(rbind, fit.l))
  converged <- as.vector(unlist(converged.l))

  if (paramOnly) converged <- rep(TRUE, length(converged))
  param <- data.frame()
  stdparam <- data.frame()
  FMI1 <- NULL
  FMI2 <- NULL
  cilower <- NULL
  ciupper <- NULL
  popMis <- NULL
  misfitOut <- NULL
  if (!is.null(param.l[[1]])) {
    param <- as.data.frame(do.call(rbind, param.l))
    if (nrow(unique(param)) == 1) param <- unique(param)
  }
  if (!is.null(stdparam.l[[1]])) {
    stdparam <- as.data.frame(do.call(rbind, stdparam.l))
    if (nrow(unique(stdparam)) == 1) stdparam <- unique(stdparam)
  }
  if (isPopulation) {
    param <- as.data.frame(t(popResult$coef))
    if (!is.null(popResult$std)) stdparam <- as.data.frame(t(popResult$std))
  }

  if (lavaanGenerate || (is.null(generate) && lavaanAnalysis && is.null(rawData))) {
    if (is.null(generate) && lavaanAnalysis) generate <- model
    generate2 <- generate
    generate2$sample.nobs <- simConds[[1]][[2]]
    generate2$return.fit <- TRUE
    lavaanfit <- attr(do.call(lavaan::simulateData, generate2), "fit")
    pt <- parTable(lavaanfit)
    if (is.partable(generate$model)) pt <- generate$model
    stdpt <- lavaan::standardizedSolution(lavaanfit, remove.eq = FALSE,
                                          remove.ineq = FALSE, remove.def = FALSE)
    if (!("group" %in% colnames(stdpt))) stdpt <- data.frame(stdpt, group = 1)
    extraParamIndex <- pt$op %in% c(">", "<", "==", ":=")
    if (any(extraParamIndex)) {
      con <- list(lhs = pt$lhs[extraParamIndex],
                  op = pt$op[extraParamIndex],
                  rhs = pt$rhs[extraParamIndex])
      hasLab <- pt$label != ""
      extraVal <- applyConScript(pt$label[hasLab][!duplicated(pt$label[hasLab])],
                                 pt$ustart[hasLab][!duplicated(pt$label[hasLab])],
                                 con, refpt = pt)
      toBeFill <- which(is.na(pt$ustart) & hasLab)
      for (i in seq_along(toBeFill)) {
        pt$ustart[toBeFill[i]] <- extraVal[[2]][extraVal[[1]] == pt$label[toBeFill[i]]]
      }
    }
    param <- pt$ustart
    changparampt <- changeDupLab(pt)
    paramNames <- lavaan::lav_partable_labels(lapply(changparampt, "[",
                                                     !(changparampt$op %in% c("==", ">", "<", ":="))))
    if (any(extraParamIndex)) {
      paramNames <- c(paramNames, renameExtraParam(pt$lhs[extraParamIndex],
                                                   pt$op[extraParamIndex],
                                                   pt$rhs[extraParamIndex],
                                                   refpt = pt))
    }
    names(param) <- paramNames
    param <- as.data.frame(t(param))
    stdparam <- stdpt$est.std
    names(stdparam) <- paramNames #lavaan::lav_partable_labels(stdpt)
    stdparam <- as.data.frame(t(stdparam))
  } else if (mxGenerate | (is.null(generate) & is(model, "MxModel"))) {
    if (is.null(generate)) generate <- model
    param <- vectorizeMx(generate)
    param <- as.data.frame(t(param))

    findStd <- FALSE
    if (length(generate@submodels) > 0) {
      defVars <- lapply(generate@submodels, findDefVars)
      defVars <- do.call(c, defVars)
      if (is(generate@submodels[[1]]@expectation, "MxExpectationRAM") && !(length(defVars) > 0)) findStd <- TRUE
    } else {
      defVars <- findDefVars(generate)
      if (is(generate@expectation, "MxExpectationRAM") && !(length(defVars) > 0)) findStd <- TRUE
    }
    if (findStd) {
      try({stdparam <- standardizeMx(generate, free = TRUE) ; stdparam <- as.data.frame(t(stdparam))},
          silent = silent)
    }
  }

  if (!is.null(std.l[[1]])) {
    std <- as.data.frame(do.call(rbind, std.l))
    if (sum(dim(std)) == 0)
      std <- data.frame()
  } else {
    std <- data.frame()
  }

  if (!is.null(stdse.l[[1]])) {
    stdse <- as.data.frame(do.call(rbind, stdse.l))
    if (sum(dim(stdse)) == 0) stdse <- data.frame()
  } else {
    stdse <- data.frame()
  }

  if (!is.null(FMI1.l[[1]])) {
    FMI1 <- as.data.frame(do.call(rbind, FMI1.l))
    if (sum(dim(FMI1)) == 0) FMI1 <- data.frame()
    if (all(is.na(FMI1))) FMI1 <- data.frame()
  } else {
    FMI1 <- data.frame()
  }

  if (!is.null(FMI2.l[[1]])) {
    FMI2 <- as.data.frame(do.call(rbind, FMI2.l))
    if (sum(dim(FMI2)) == 0) FMI2 <- data.frame()
    if (all(is.na(FMI2))) FMI2 <- data.frame()
  } else {
    FMI2 <- data.frame()
  }

  if (!is.null(cilower.l[[1]])) {
    cilower <- as.data.frame(do.call(rbind, cilower.l))
    if (sum(dim(cilower)) == 0) cilower <- data.frame()
    if (all(is.na(cilower))) cilower <- data.frame()
  } else {
    cilower <- data.frame()
  }

  if (!is.null(ciupper.l[[1]])) {
    ciupper <- as.data.frame(do.call(rbind, ciupper.l))
    if (sum(dim(ciupper)) == 0) ciupper <- data.frame()
    if (all(is.na(ciupper))) ciupper <- data.frame()
  } else {
    ciupper <- data.frame()
  }

  if (!is.null(popMis.l[[1]])) {
    popMis <- as.data.frame(do.call(rbind, popMis.l))
    if (nrow(unique(popMis)) == 1) popMis <- unique(popMis)
    if (all(is.na(popMis))) popMis <- data.frame()
  } else {
    popMis <- data.frame()
  }

  if (!is.null(misfitOut.l[[1]])) {
    misfitOut <- as.data.frame(do.call(rbind, misfitOut.l))
    if (nrow(unique(misfitOut)) == 1) misfitOut <- unique(misfitOut)
    if (all(is.na(misfitOut))) misfitOut <- data.frame()
  } else {
    misfitOut <- data.frame()
  }

  if (is.null(pmMCAR)) pmMCAR <- if (is.null(miss)) 0 else miss@pmMCAR
  if (is.null(pmMAR)) pmMAR <- if (is.null(miss)) 0 else miss@pmMAR

  if (!is.null(rawData) & !isPopulation) {
    if (ngroups > 1) {
      if (lavaanAnalysis) {
        groupLab <- model$group
      } else if (mxAnalysis) {
        groupLab <- group
      } else {
        groupLab <- model@groupLab
      }
      if (is.null(groupLab)) groupLab <- "group"
      nobs <- as.data.frame(t(sapply(rawData, function(x, col) table(x[,col]),
                                     col = groupLab)))
    } else {
      nobs <- as.data.frame(sapply(rawData, nrow))
    }
    n <- apply(nobs, 1, sum)
    colnames(nobs) <- 1:ngroups
  } else {
    nobs <- as.data.frame(n)
    n <- Reduce("+", n)
    colnames(nobs) <- 1:ngroups
  }

  modelType <- "lavaan"
  if (!lavaanAnalysis) {
    if (mxAnalysis) {
      modelType <- "OpenMx"
    } else if (functionAnalysis) {
      modelType <- "function"
    } else {
      modelType <- model@modelType
    }
  }

  colnames(fit) <- tolower(colnames(fit))

  timing$CombineResults <- proc.time()[3] - start.time
  start.time <- proc.time()[3]
  timing$EndTime <- Sys.time()
  Result <- new("SimResult", modelType = modelType, nRep = nRep, coef = coef,
                se = se, fit = fit, converged = converged, seed = c(seed, s),
                paramValue = param, stdParamValue = stdparam,
                misspecValue = popMis, popFit = misfitOut,
                FMI1 = FMI1, FMI2 = FMI2, cilower = cilower, ciupper = ciupper,
                stdCoef = std, stdSe = stdse, n = n, nobs = nobs,
                pmMCAR = pmMCAR, pmMAR = pmMAR, extraOut = extra,
                paramOnly = paramOnly, timing = timing)

  if (!is.null(previousSim)) {
    Result <- combineSim(previousSim, Result)
  }

  ## If completeRep = TRUE, check whether the number of converged results
  if (completeRep > 0 & !is.null(nRepInitial)) {
    success <- sum(Result@converged == 0)
    pSuccess <- success / Result@nRep
    if (success < completeRep) {
      nRepNew <- ceiling((completeRep - success) / pSuccess)
      Result <- sim(nRep = nRepNew, model = model, n = nInitial, generate = generate,
                    rawData = rawData, miss = miss, datafun = datafun,
                    lavaanfun = lavaanfun, outfun = outfun, outfundata = outfundata,
                    pmMCAR = pmMCARInitial, pmMAR = pmMARInitial, facDist = facDist,
                    indDist = indDist, errorDist = errorDist, sequential = sequential,
                    saveLatentVar = saveLatentVar, modelBoot = modelBoot,
                    realData = realData, covData = covData, maxDraw = maxDraw,
                    misfitType = misfitType, misfitBounds = misfitBounds,
                    averageNumMisspec = averageNumMisspec, optMisfit = optMisfit,
                    optDraws = optDraws, createOrder = createOrder, aux = aux,
                    group = group, mxFit = mxFit, mxMixture = mxMixture,
                    citype = citype, cilevel = cilevel, seed = seed,
                    silent = silent, multicore = multicore, cluster = cluster,
                    numProc = numProc, paramOnly = paramOnly, dataOnly = dataOnly,
                    smartStart = smartStart, previousSim = Result,
                    completeRep = completeRep, stopOnError = stopOnError, ...)
    }
  }

  if (silent) options(warn = warnT)
  return <- Result
}


## runRep: Run one replication

runRep <- function(simConds, model, generateO = NULL, miss = NULL, datafun = NULL,
                   lavaanfun = NULL, outfun = NULL, outfundata = NULL,
                   facDist = NULL, indDist = NULL, indLab = NULL, errorDist = NULL,
                   sequential = FALSE, saveLatentVar = FALSE, realData = NULL,
                   covData = NULL, silent = FALSE, modelBoot = FALSE, maxDraw = 50,
                   misfitType = "f0", misfitBounds = NULL, averageNumMisspec = NULL,
                   optMisfit = NULL, optDraws = 50, createOrder = c(1, 2, 3),
                   aux = NULL, paramOnly = FALSE, dataOnly = FALSE, smartStart = TRUE,
                   popData = NULL, group = NULL, mxFit = FALSE, mxMixture = FALSE,
                   citype = NULL, cilevel = 0.95, stopOnError = FALSE, ...) {
  start.time <- proc.time()[3]
  timing <- list()
  coef <- NA
  se <- NA
  fit <- NA
  std <- NA
  stdse <- NA
  extra <- NULL
  extra2 <- NULL
  FMI1 <- NA
  FMI2 <- NA
  cilower <- NA
  ciupper <- NA
  popParam <- NA  # Real Data
  stdPopParam <- NA # Real Data Standardized
  paramSet <- NULL
  converged <- 1
  n <- simConds[[2]]
  pmMCAR <- simConds[[3]]
  pmMAR <- simConds[[4]]
  RNGkind("L'Ecuyer-CMRG")
  assign(".Random.seed", simConds[[5]], envir = .GlobalEnv)
  skipMiss <- simConds[[6]]
  specifiedGenerate <- TRUE

  if (is.null(generateO)) {
    generateO <- model
    specifiedGenerate <- FALSE
  }

  ## Two things to think about:
  ## 1. Only do this when the generating and analysis models are different
  ## 2. Why do we do this for each rep?  Move up to sim/model and pass values
  ##    to runRep. Add 2 slots to SimSem class, put it there.
  ## 3. Also don't do this with rawData


  ## 1. Create a missing-data template from simulation parameters.
  ## Creates SimMissing object if none exists (but pmMCAR or pmMAR are specified)
  ## If a SimMissing object does exist it sets pmMCAR and pmMAR based on the SimMissing object
  if (is.null(miss)) {
    if (!is.null(pmMAR) | !is.null(pmMCAR)) {
      if (is.null(pmMCAR)) pmMCAR <- 0
      if (is.null(pmMAR)) pmMAR <- 0
      miss <- miss(pmMCAR = pmMCAR, pmMAR = pmMAR, ignoreCols = "group")
    }
  } else {
    if (is.null(pmMCAR)) pmMCAR <- miss@pmMCAR
    if (is.null(pmMAR)) pmMAR <- miss@pmMAR
  }

  ## 2. Generate data (data) & store parameter values (paramSet)
  data <- simConds[[1]]  # either a paramSet or raw data
  if (!is.null(popData)) {
    if (is.null(n)) {
      data <- popData
    } else {
      groupLab <- NULL
      if (is(model, "SimSem")) {
        groupLab <- model@groupLab
      } else if (is(model, "function")) {
        ## Intentionally leave as blank
      } else {
        groupLab <- model$group
      }
      if (!is.null(groupLab) && (groupLab %in% colnames(popData))) {
        data <- split(popData, popData[ , groupLab])
        data <- mapply(function(dat, ss) dat[sample(nrow(dat), ss), ],
                       dat = data, ss = n, SIMPLIFY = FALSE)
        data <- data.frame(do.call(rbind, data))
      } else {
        data <- popData[sample(nrow(popData), n), ]
      }
    }
  }

  if (is.null(data) && is(generateO, "function")) {
    if (stopOnError) {
      data <- generateO(n)
    } else if (silent) {
      invisible(capture.output(suppressMessages(try(data <- generateO(n), silent = TRUE))))
    } else {
      try(data <- generateO(n))
    }
    if (!is.null(attr(data, "param"))) popParam <- attr(data, "param")
    if (!is.null(attr(data, "stdparam"))) stdPopParam <- attr(data, "stdparam")
  }

  if (is.null(data) && is.lavaancall(generateO)) {
    generateO$sample.nobs <- n

    if (!is.null(indDist)) {
      generateO$return.fit <- TRUE
      generateO$skewness <- indDist@skewness
      generateO$kurtosis <- indDist@kurtosis
      data <- do.call(lavaan::simulateData, generateO) # Change to simulateData when the bug is fixed ### FIXME: was the bug fixed?
      # implied <- lavaan::fitted(attr(data, "fit"))
      # if (length(n) == 1L) implied <- list(implied)
      # datinddist <- NULL
      # for (i in seq_along(n)) datinddist <- rbind(datinddist, dataGen(indDist, n[i], implied[[i]]$mean, implied[[i]]$cov))
      # datinddist <- as.data.frame(datinddist)
      # datinddist$group <- rep(1:length(n), times = n)
    } else {
      data <- do.call(lavaan::simulateData, generateO) # Change to simulateData when the bug is fixed
    }
  }

  if (is.null(data) && is(generateO, "MxModel")) {
    data <- generateMx(generateO, n = n, indDist = indDist, covData = covData)
  }

  if (is.null(data)) {
    ## Label variables for creating labels later
    indLabGen <- NULL
    if (generateO@modelType == "path") {
      indLabGen <- unique(generateO@pt$lhs)
    } else {
      indLabGen <- unique(generateO@pt$rhs[generateO@pt$op == "=~"])
    }
    facLabGen <- NULL
    if (generateO@modelType != "path") {
      facLabGen <- unique(generateO@pt$lhs[generateO@pt$op == "=~"])
    }
    covLabGen <- generateO@pt$lhs[generateO@pt$op == "~1" & generateO@pt$exo == 1L]
    if (length(covLabGen) == 0L) covLabGen <- NULL

    ## Need to draw parameters
    genout <- generateSimSem(model = generateO, n = n, maxDraw = maxDraw,
                             misfitBounds = misfitBounds, misfitType = misfitType,
                             averageNumMisspec = averageNumMisspec,
                             optMisfit = optMisfit, optDraws = optDraws,
                             createOrder = createOrder, indDist = indDist,
                             sequential = sequential, saveLatentVar = saveLatentVar,
                             facDist = facDist, errorDist = errorDist,
                             indLab = indLab, modelBoot = modelBoot,
                             realData = realData, covData = covData, params = TRUE)
    data <- genout[[1]]
    psl <- genout[[2]] # these are randomly drawn parameters

    ## We need the real parameter values regardless of having model misspecification
    ## because the real parameter values are what we really need to infer
    paramSet <- lapply(psl, "[[", 1)

    ## Save parameter values in the parameter tables
    generatedgen <- generateO@dgen
    popParam <- NULL
    stdPopParam <- NULL
    if (!is.list(generatedgen[[1]])) generatedgen <- list(generatedgen)

    covLab <- unique(generateO@pt$lhs[generateO@pt$op == "~1" & generateO@pt$exo == 1])
    ngroups <- length(paramSet)
    covDataGroup <- rep(list(NULL), ngroups)

    if (length(covLab) > 0L) {
      if (ngroups > 1L) covLab <- c(covLab, model@groupLab)
      if (!is.null(realData) && is.null(covData)) covData <- realData[ , covLab, drop = FALSE]
      covData <- covData[ , covLab, drop = FALSE]
      if (ngroups > 1L) {
        covDataGroup <- split(covData[ , setdiff(covLab, model@groupLab), drop = FALSE],
                              f = covData[ , model@groupLab])
      } else {
        covDataGroup <- list(covData)
      }
    }
    for (i in seq_along(paramSet)) {
      popParam <- c(popParam, parsePopulation(generatedgen[[i]], paramSet[[i]], group = i))
      stdPopParam <- c(stdPopParam,
                       parsePopulation(generatedgen[[i]], paramSet[[i]],
                                       group = i, std = TRUE,
                                       covData = covDataGroup[[i]]))
    }
    if (smartStart) {
      ## Once indLab and facLab are in the model object, sub them in for indLabGen and facLabGen
      if (specifiedGenerate) stop("The smartStart option is unavailable when the ",
                                  "analysis and data-generation models differ")
      model@pt$ustart <- c(popParam, rep(NA, length(model@pt$ustart) - length(popParam)))
    }
  }  # else: do nothing. Raw data.

  timing$GenerateData <- (proc.time()[3] - start.time)
  start.time <- proc.time()[3]
  #### 3. Impose Missing (if any)
  if (!is.null(miss) & !skipMiss) data <- impose(miss, data)

  timing$ImposeMissing <- (proc.time()[3] - start.time)
  start.time <- proc.time()[3]
  #### 4. Call user function (if exists)
  if (!is.null(datafun)) data <- datafun(data)
  timing$UserFun <- (proc.time()[3] - start.time)
  start.time <- proc.time()[3]

  #### 5. Call lavaan using simsem template and generated data from 2.
  out <- NULL
  mxAnalysis <- FALSE
  if (!paramOnly & !dataOnly) {
    ## Impute missing and run results
    ## Will use analyze either when there is a missing object or auxiliary variables specified.
    ## If users provide their own data there may be a case with auxiliary variables and no missing object
    if (is(model, "function")) {
      if (stopOnError) {
        out <- model(data, ...)
      } else if (silent) {
        invisible(capture.output(suppressMessages(try(out <- model(data, ...),
                                                      silent = TRUE))))
      } else {
        try(out <- model(data, ...))
      }
    } else if (is.lavaancall(model)) {
      model$data <- data
      if (stopOnError) {
        out <- analyzeLavaan(model, lavaanfun, miss, aux)
      } else if (silent) {
        invisible(capture.output(suppressMessages(try(out <- analyzeLavaan(model, lavaanfun, miss, aux),
                                                      silent = TRUE))))
      } else {
        try(out <- analyzeLavaan(model, lavaanfun, miss, aux))
      }
    } else if (is(model, "MxModel")) {
      mxAnalysis <- TRUE
      multigroup <- length(model@submodels) > 0L
      if (multigroup) {
        if (is.null(group)) group <- "group"
      } else {
        group <- NULL
      }
      if (stopOnError) {
        out <- analyzeMx(model, data, groupLab = group, ...)
      } else if (silent) {
        invisible(capture.output(suppressMessages(try(out <- analyzeMx(model, data, groupLab = group, ...),
                                                      silent = TRUE))))
      } else {
        try(out <- analyzeMx(model, data, groupLab = group, mxMixture = mxMixture, ...))
      }
    } else {
      if (!is.null(miss) | !is.null(aux)) {
        if (stopOnError) {
          out <- analyzeSimSem(model, data, aux = aux, miss = miss, ...)
        } else if (silent) {
          invisible(capture.output(suppressMessages(try(out <- analyzeSimSem(model, data, aux = aux, miss = miss, ...),
                                                        silent = TRUE))))
        } else {
          try(out <- analyzeSimSem(model, data, aux = aux, miss = miss, ...))
        }
      } else {
        if (stopOnError) {
          out <- anal(model, data, ...)
        } else if (silent) {
          invisible(capture.output(suppressMessages(try(out <- anal(model, data, ...),
                                                        silent = TRUE))))
        } else {
          try(out <- anal(model, data, ...))
        }
      }
    }
  }
  timing$Analyze <- (proc.time()[3] - start.time)
  start.time <- proc.time()[3]

  #### 6. Parse Lavaan Output
  if (dataOnly) return(data)
  if (!is.null(out)) {
    if (is(model, "function")) {
      converged <- out$converged
      if (is.null(converged)) stop("In the function for data analysis, ",
                                   "please specify the 'converged' in the ",
                                   "resulting list")
      if (is.logical(converged)) {
        if (converged) {
          converged <- 0L
        } else {
          converged <- 1L
        }
      }
    } else if (mxAnalysis) {
      try(converged.l <- out@output$status)
      converged <- 0L
      if (converged.l[[2]] != 0L) converged <- 1L
      if (converged.l[[1]] == 1L) {
        converged <- 0L
      } else if (converged.l[[1]] == 6L) {
        converged <- 7L
      } else if (converged.l[[1]] == 0L) {
        converged <- 0L
      } else {
        converged <- 1L
      }
      if (converged == 0L) {
        seTemp <- out@output$standardErrors
        improperSE <- any(unlist(seTemp) < 0) | any(is.na(unlist(seTemp))) | all(unlist(seTemp) == 0)
        if (improperSE) converged <- 3L
      }
    } else  if (is(out, "lavaan.mi")) {
      if (mean(sapply(out@convergence, "[[", "converged")) < miss@convergentCutoff) {
        converged <- 2L
      } else if (mean(sapply(out@convergence, "[[", "SE"), na.rm = TRUE) < miss@convergentCutoff) {
        converged <- 3L
      } else if (mean(sapply(out@convergence, "[[", "Heywood.lv"), na.rm = TRUE) < miss@convergentCutoff) {
        converged <- 4L
      } else if (mean(sapply(out@convergence, "[[", "Heywood.ov"), na.rm = TRUE) < miss@convergentCutoff) {
        converged <- 5L
      } else converged <- 0L
    } else {
      try(converged <- as.numeric(!lavInspect(out, "converged")))
      if (converged == 0L) {
        seTemp <- lavInspect(out, "se")
        improperSE <- any(unlist(seTemp) < 0) | any(is.na(unlist(seTemp))) | all(unlist(seTemp) == 0)
        if (improperSE) converged <- 3L
        if (checkVar(out)) {
          converged <- 4L
        } else if(checkCov(out)) {
          converged <- 5L
        } else if(checkCovLv(out)) {
          converged <- 6L
        }
      }
    }
  }

  if (converged %in% c(0L, 3:7)) {

    if (is(model, "function")) {
      fit <- out$fit
      coef <- out$coef
      se <- out$se
      std <- out$std
      stdse <- out$stdse
      extra <- out$extra
      FMI1 <- out$FMI1
      FMI2 <- out$FMI2
      cilower <- out$cilower
      ciupper <- out$ciupper
    } else if (mxAnalysis) {
      if (mxFit) {
        fit <- NA
        try(fit <- fitMeasuresMx(out), silent = silent)
        if (!all(is.na(fit))) {
          availfit <- names(fit)
          if ("saturate.status" %in% availfit) {
            set1 <- intersect(c("logl", "npar", "aic", "bic"), availfit)
            set2 <- intersect(c("cfi", "tli", "nnfi", "pnfi", "rfi", "nfi", "ifi",
                                "rni", "baseline.chisq", "baseline.pvalue"), availfit)
            set3 <- setdiff(availfit, c(set1, set2))
            if (!(fit["saturate.status"] %in% c(0, 1))) fit[c(set2, set3)] <- NA
            if (!(fit["null.status"] %in% c(0, 1))) fit[set2] <- NA
          }
        }
      } else {
        fit <- easyFitMx(out, mxMixture = mxMixture)
      }
      coef <- out@output$estimate
      se <- as.vector(out@output$standardErrors)
      name <- names(coef)
      name <- gsub(paste0(out@name, "."), "", name)
      names(coef) <- name
      names(se) <- name
      findStd <- FALSE
      ci <- out@output$confidenceIntervals
      if (!is.null(ci) && nrow(ci) > 0) {
        cilower <- ci[,1]
        ciupper <- ci[,2]
        nameci <- gsub(paste0(out@name, "."), "", rownames(ci))
        names(cilower) <- nameci
        names(ciupper) <- nameci
      }
      if (length(out@submodels) > 0) {
        defVars <- lapply(out@submodels, findDefVars)
        defVars <- do.call(c, defVars)
        if (is(out@submodels[[1]]@expectation, "MxExpectationRAM") && !(length(defVars) > 0)) findStd <- TRUE
      } else {
        defVars <- findDefVars(out)
        if (is(out@expectation, "MxExpectationRAM") && !(length(defVars) > 0)) findStd <- TRUE
      }
      if (findStd) {
        std <- NA
        try(std <- standardizeMx(out, free = TRUE), silent = silent)
      }

    } else {

      if (is.null(citype)) citype <- formals(lavaan::parameterEstimates)$boot.ci.type
      outpt <- parTable(out)
      extraParamIndex <- outpt$op %in% c(">", "<", "==", ":=")
      index <- ((outpt$free != 0) & !(duplicated(outpt$free))) | extraParamIndex

      ## lavaan.mi results come from different source than lavaan
      if (is(out, "lavaan.mi")) {
        fit <- semTools::fitMeasures(out)
        result <- getMethod("summary", "lavaan.mi")(out, standardized = "std.all",
                                                    level = cilevel, fmi = TRUE,
                                                    add.attributes = FALSE)
        outpt$se <- result$se
        stdse <- NULL
        if (converged %in% c(0L, 3:5)) {
          FMI1 <- result$fmi[index] # result$fmi1[index]
          FMI2 <- NULL              # result$fmi2[index]
        }
      } else {
        fit <- lavaan::fitMeasures(out)
        result <- lavaan::parameterEstimates(out, standardized = TRUE,
                                             boot.ci.type = citype,
                                             level = cilevel, remove.eq = FALSE,
                                             remove.ineq = FALSE, remove.def = FALSE,
                                             fmi = !is.null(miss))
        errstdse <- try(resultstd <- lavaan::standardizedSolution(out, remove.eq = FALSE,
                                                                  remove.ineq = FALSE, remove.def = FALSE))
        if(!is(errstdse, "try-error")) stdse <- resultstd$se[index]
        FMI1 <- if (is.null(miss)) NULL else result$fmi[index]
        FMI2 <- NULL
      }

      changept <- changeDupLab(outpt)
      lab <- lavaan::lav_partable_labels(lapply(changept, "[", changept$free > 0 | (outpt$user == 1 & outpt$start !=0 & outpt$se != 0)))

      coef <- result$est[index]
      se <- result$se[index]
      std <- result$std.all[index]
      cilower <- result$ci.lower[index]
      ciupper <- result$ci.upper[index]

      if (any(extraParamIndex)) {
        if (!is.lavaancall(model)) {
          lab <- union(lab, renameExtraParam(model@pt$lhs[extraParamIndex],
                                         model@pt$op[extraParamIndex],
                                         model@pt$rhs[extraParamIndex], refpt = outpt))
        } else {
          ## 27 July 2017: Terry changed c() to union() above and below to
          ## prevent adding duplicates of := parameters
          lab <- union(lab, renameExtraParam(outpt$lhs[extraParamIndex],
                                         outpt$op[extraParamIndex],
                                         outpt$rhs[extraParamIndex], refpt = outpt))
           }
      }
      names(coef) <- lab
      names(se) <- lab
      names(std) <- lab
      if (!(length(stdse) == 1L && is.na(stdse)) && !is.null(stdse)) names(stdse) <- lab
      if (!is.null(cilower)) names(cilower) <- lab
      if (!is.null(ciupper)) names(ciupper) <- lab
      if (!is.null(FMI1)) names(FMI1) <- lab
      if (!is.null(FMI2)) names(FMI2) <- lab
    }
  } else {
    if (is(model, "function")) {
      try(fit <- out$fit, silent = TRUE)
      try(coef <- out$coef, silent = TRUE)
      try(se <- out$se, silent = TRUE)
      try(std <- out$std, silent = TRUE)
      try(stdse <- out$stdse, silent = TRUE)
      try(extra <- out$extra, silent = TRUE)
      try(FMI1 <- out$FMI1, silent = TRUE)
      try(FMI2 <- out$FMI2, silent = TRUE)
      try(cilower <- out$cilower, silent = TRUE)
      try(ciupper <- out$ciupper, silent = TRUE)
    }
  }

  ## Run outfun regardless of convergence. May want to process non-convergent sets

  if (!is.null(outfun)) {
    try(extra <- outfun(out), silent = TRUE)
  }
  if (!is.null(outfundata)) {
    try(extra2 <- outfundata(out, data), silent = TRUE)
  }

  ## Keep parameters regardless of convergence. May want to examine non-convergent sets
  if (!is.null(paramSet)) {
    if (!is.null(psl[[1]]$misspec)) {
      misParamSet <- lapply(psl, "[[", 3)
      popMis <- reduceMisspecSet(misParamSet, generateO@modelType != "path",
                                 indLabGen, facLabGen, covLab = covLabGen)
      p <- length(indLabGen)
      nElements <- (p + (p * (p + 1)/2)) * length(psl)
      dfParam <- nElements - max(generateO@pt$free)
      misfitOut <- popMisfitParams(psl, df = dfParam, covData = covData)
    } else {
      popMis <- NA
      misfitOut <- NA
    }

    changgenept <- changeDupLab(generateO@pt)
    paramNames <- lavaan::lav_partable_labels(lapply(changgenept, "[", changgenept$free > 0L))

    # paramNames <- names(coef(lavaan::lavaan(changeDupLab(generate@pt), sample.nobs=rep(200, max(generate@pt$group)))))
    extraParamIndex <- generateO@pt$op %in% c(">", "<", "==", ":=")
    extraParamName <- NULL
    if (any(extraParamIndex)) {
      extraparam <- collapseExtraParam(paramSet, generateO@dgen, fill = TRUE, con = generateO@con)
      extraParamName <- renameExtraParam(generateO@con$lhs, generateO@con$op, generateO@con$rhs)
      popParam[extraParamIndex] <- extraparam
    }
    indexstd <- (generateO@pt$free != 0) & !(duplicated(generateO@pt$free))
    index <- indexstd | extraParamIndex
    popParam <- popParam[index]
    stdPopParam <- stdPopParam[indexstd]
    names(popParam) <- c(paramNames, extraParamName)
    names(stdPopParam) <- paramNames
  } else {
    popMis <- NA  # Misspecfication
    misfitOut <- NA  # Misfit indices for misspecification
  }



  timing$ParseOutput <- (proc.time()[3] - start.time)
  start.time <- proc.time()[3]
  if (is.null(extra) & !is.null(extra2)) {
    extra <- extra2
  } else if (!is.null(extra) & !is.null(extra2)) {
    extra <- list(extra, extra2)
  }

  Result <- list(coef = coef, se = se, fit = fit, converged = converged,
                 param = popParam, stdparam = stdPopParam,
                 FMI1 = FMI1, FMI2 = FMI2, std = std, stdse = stdse,
                 timing = timing, extra = extra, popMis = popMis,
                 cilower = cilower, ciupper = ciupper, misfitOut = misfitOut)
  return(Result)
}

## MispecSet is still needed but paramSet above can be replaced
reduceMisspecSet <- function(misspecSet, latent, indLab = NULL, facLab = NULL, covLab = NULL) {

    final <- NULL
    psLab <- NULL
    if (latent) {
        psLab <- facLab
    } else {
        psLab <- indLab
    }
	ngroups <- length(misspecSet)

    for (g in seq_along(misspecSet)) {
		temp <- NULL

        tpset <- misspecSet[[g]]
        tpset <- lapply(tpset, function(x) {
            if (is.null(x) || (is.vector(x) && length(x) == 0)) {
                return(NULL)
            } else {
                return(x)
            }
        })

        if (!is.null(tpset$LY)) {
            free <- tpset$LY != 0
            lab <- outer(indLab, facLab[1:ncol(free)], function(x, y, op) paste0(y,
                op, x), op = "=~")
            param <- tpset$LY[free]
            names(param) <- lab[free]
            temp <- c(temp, param)
        }
        if (!is.null(tpset$PS)) {
            free <- tpset$PS != 0 & lower.tri(tpset$PS, diag = TRUE)
            lab <- outer(psLab[1:ncol(free)], psLab[1:ncol(free)], function(x, y, op) paste0(x, op,
                y), op = "~~")
            param <- tpset$PS[free]
            names(param) <- lab[free]
            temp <- c(temp, param)
        }
        if (!is.null(tpset$RPS)) {
            free <- tpset$RPS != 0 & lower.tri(tpset$RPS, diag = FALSE)
            lab <- outer(psLab[1:ncol(free)], psLab[1:ncol(free)], function(x, y, op) paste0(x, op,
                y), op = "~~*")
            param <- tpset$RPS[free]
            names(param) <- lab[free]
            temp <- c(temp, param)
        }
        if (!is.null(tpset$VPS)) {
            free <- tpset$VPS != 0
            lab <- paste0(psLab[1:length(free)], "~~", psLab)
            param <- tpset$VPS[free]
            names(param) <- lab[free]
            temp <- c(temp, param)
        }
        if (!is.null(tpset$VE)) {
            free <- tpset$VE != 0
            lab <- paste0(psLab[1:length(free)], "~~*", psLab)
            param <- tpset$VE[free]
            names(param) <- lab[free]
            temp <- c(temp, param)
        }
        if (!is.null(tpset$TE)) {
            free <- tpset$TE != 0 & lower.tri(tpset$TE, diag = TRUE)
            lab <- outer(indLab, indLab, function(x, y, op) paste0(x,
                op, y), op = "~~")
            param <- tpset$TE[free]
            names(param) <- lab[free]
            temp <- c(temp, param)
        }
        if (!is.null(tpset$RTE)) {
            free <- tpset$RTE != 0 & lower.tri(tpset$RTE, diag = FALSE)
            lab <- outer(indLab, indLab, function(x, y, op) paste0(x,
                op, y), op = "~~*")
            param <- tpset$RTE[free]
            names(param) <- lab[free]
            temp <- c(temp, param)
        }
        if (!is.null(tpset$VTE)) {
            free <- tpset$VTE != 0
            lab <- paste0(indLab, "~~", indLab)
            param <- tpset$VTE[free]
            names(param) <- lab[free]
            temp <- c(temp, param)
        }
        if (!is.null(tpset$VY)) {
            free <- tpset$VY != 0
            lab <- paste0(indLab, "~~*", indLab)
            param <- tpset$VY[free]
            names(param) <- lab[free]
            temp <- c(temp, param)
        }
        if (!is.null(tpset$BE)) {
            free <- tpset$BE != 0
            lab <- outer(psLab[1:ncol(free)], psLab[1:ncol(free)], function(x, y, op) paste0(x, op,
                y), op = "~")
            param <- tpset$BE[free]
            names(param) <- lab[free]
            temp <- c(temp, param)
        }
        if (!is.null(tpset$AL)) {
            free <- tpset$AL != 0
            lab <- paste0(psLab[1:length(free)], "~1")
            param <- tpset$AL[free]
            names(param) <- lab[free]
            temp <- c(temp, param)
        }
        if (!is.null(tpset$ME)) {
            free <- tpset$ME != 0
            lab <- paste0(psLab[1:length(free)], "~1*")
            param <- tpset$ME[free]
            names(param) <- lab[free]
            temp <- c(temp, param)
        }
        if (!is.null(tpset$TY)) {
            free <- tpset$TY != 0
            lab <- paste0(indLab, "~1")
            param <- tpset$TY[free]
            names(param) <- lab[free]
            temp <- c(temp, param)
        }
        if (!is.null(tpset$MY)) {
            free <- tpset$MY != 0
            lab <- paste0(indLab, "~1*")
            param <- tpset$MY[free]
            names(param) <- lab[free]
            temp <- c(temp, param)
        }
        if (!is.null(tpset$GA)) {
            free <- tpset$GA != 0
            lab <- outer(psLab[1:nrow(free)], covLab[1:ncol(free)], function(x, y, op) paste0(x, op,
                y), op = "~")
            param <- tpset$GA[free]
            names(param) <- lab[free]
            temp <- c(temp, param)
        }
        if (!is.null(tpset$KA)) {
            free <- tpset$KA != 0
            lab <- outer(indLab[1:nrow(free)], covLab[1:ncol(free)], function(x, y, op) paste0(x, op,
                y), op = "~")
            param <- tpset$KA[free]
            names(param) <- lab[free]
            temp <- c(temp, param)
        }

		if(ngroups > 1) names(temp) <- paste0(g, ".", names(temp))
		final <- c(final, temp)
	}
    final
}

## Check to see if random parameters are in the model. Can use to simplfy runRep
is.random <- function(dat) {
    dat[is.empty(dat)] <- "0"
    isRandom <- sapply(dat, FUN = function(x) {
        x <- suppressWarnings(is.na(as.numeric(x)))
    })
    return(isRandom)
}

checkVar <- function(object) {
    GLIST <- object@Model@GLIST
    covGLIST <- GLIST[names(GLIST) %in% c("theta", "psi")]
    return(any(sapply(covGLIST, function(x) any(diag(x) < 0))))
}

checkCov <- function(object) {
    GLIST <- object@Model@GLIST
    covGLIST <- GLIST[names(GLIST) %in% c("theta", "psi")]
	reducedCov <- lapply(covGLIST, function(x) {improper <- (diag(x) > 0); x[improper, improper, drop=FALSE]})
    return(any(sapply(reducedCov, function(x) {
		if(nrow(x) > 1) {
		y <- lower.tri(cov2cor(x)); any((y < -1) | (y > 1))
		} else {
		return(FALSE)
		}
	})))
}

checkCovLv <- function(object) {
	covlvs <- lavInspect(object, "cov.lv")
	if (!is(covlvs, "list")) {
		covlvs <- list(covlvs)
	}
	result <- rep(FALSE, length(covlvs))
	for (i in seq_along(covlvs)) {
		if (nrow(covlvs[[i]]) >= 1) {
			ev <- eigen(covlvs[[i]])$values
			if (any(ev < 0)) result[i] <- TRUE
		}
	}
	any(result)
}

collapseExtraParam <- function(pls, dgen, fill = TRUE, con = NULL) {
	## Collapse all labels
	if (length(pls) == 1 & length(pls) != 1) dgen <- list(dgen) # FIX CONTRADICTION: length(pls) can never both == 1 and != 1
	temp <- extractLab(pls, dgen, fill = fill, con = con)
	target <- temp[[1]]
	realval <- temp[[2]]
	#oldop <- con$op
	for (i in 1:length(con[[1]])) {
		if (con[[2]][i] %in% c(">", "==")) {
			con[[3]][i] <- paste("(", con[[1]][i], ")", "-", "(", con[[3]][i], ")")
			con[[1]][i] <- paste0("diff", i)
			con[[2]][i] <- ":="
		} else if (con[[2]][i] == "<") {
			con[[3]][i] <- paste("(", con[[3]][i], ")", "-", "(", con[[1]][i], ")")
			con[[1]][i] <- paste0("diff", i)
			con[[2]][i] <- ":="
		}
	}
	temp <- applyConScript(target, realval, con)
	target <- temp[[1]]
	realval <- temp[[2]]
	pos <- match(con$lhs, target)
	result <- realval[pos]
	names(result) <- target[pos]
	result
}

renameExtraParam <- function(lhs, op, rhs, refpt = NULL) {
	for(i in 1:length(lhs)) {
		lablhs <- lhs[i]
		labrhs <- rhs[i]
		if(!is.null(refpt)) {
			matchlhs <- match(lablhs, refpt$plabel)
			matchrhs <- match(labrhs, refpt$plabel)
			if(!is.na(matchlhs)) lablhs <- refpt$label[matchlhs]
			if(!is.na(matchrhs)) labrhs <- refpt$label[matchrhs]
		}
		if(op[i] %in% c(">", "==")) {
			lhs[i] <- paste("[", lablhs, "]", "-", "[", labrhs, "]")
		} else if (op[i] == "<") {
			lhs[i] <- paste("[", labrhs, "]", "-", "[", lablhs, "]")
		}
	}
	dup <- duplicated(lhs) | duplicated(lhs, fromLast = TRUE)
	lhs[dup] <- paste(lhs[dup], "#", 1:sum(dup))
	lhs
}



## The steps follows the buildPT function.
parsePopulation <- function(paramSet, draws, group = 1, std = FALSE, covData = NULL) {
	if(std) {
		temp <- draws

		vareta <- draws$PS
		if (!is.null(draws$BE)) {
			numrow <- nrow(draws$BE)
			ID <- diag(rep(1, numrow))
			vareta <- solve(ID - draws$BE) %*% vareta %*% solve(t(ID - draws$BE))
		}
		if(!is.null(covData)) {
			sigmax <- cov(covData)
			tempvareta <- draws$GA %*% sigmax %*% t(draws$GA)
			if (!is.null(draws$BE)) {
				numrow <- nrow(draws$BE)
				ID <- diag(rep(1, numrow))
				tempvareta <- solve(ID - draws$BE) %*% tempvareta %*% solve(t(ID - draws$BE))
			}
			vareta <- vareta + tempvareta
		}
		vary <- vareta
		if (!is.null(draws$LY)) {
			vary <- draws$LY %*% vareta %*% t(draws$LY) + draws$TE
			if(!is.null(covData)) {
				sigmax <- cov(covData)
				vary <- vary + draws$KA %*% sigmax %*% t(draws$KA)
			}
		}
		deta <- sqrt(diag(vareta))
		ifelse(length(deta) == 1, deta <- as.matrix(deta), deta <- diag(deta))
		dy <- sqrt(diag(vary))
		ifelse(length(dy) == 1, dy <- as.matrix(dy), dy <- diag(dy))
		if (!is.null(draws$LY)) draws$LY <- solve(dy) %*% temp$LY %*% deta
		if (!is.null(draws$PS)) draws$PS <- solve(deta) %*% temp$PS %*% solve(deta)
		if (!is.null(draws$TE)) draws$TE <- solve(dy) %*% temp$TE %*% solve(dy)
		if (!is.null(draws$BE)) draws$BE <- solve(deta) %*% temp$BE %*% deta
		if (!is.null(draws$AL)) draws$AL <- solve(deta) %*% temp$AL
		if (!is.null(draws$TY)) draws$TY <- solve(dy) %*% temp$TY
		if(!is.null(covData)) {
			sigmax <- cov(covData)
			dx <- sqrt(diag(sigmax))
			ifelse(length(dx) == 1, dx <- as.matrix(dx), dx <- diag(dx))
			if (!is.null(draws$GA)) draws$GA <- solve(deta) %*% temp$GA %*% dx
			if (!is.null(draws$KA)) draws$KA <- solve(dy) %*% temp$KA %*% dx
		}
	}
    ustart <- NULL
    if (!is.null(paramSet$LY)) {
        ustart <- c(ustart, startingVal(paramSet$LY@free, draws$LY, smart = TRUE, symm = FALSE))
    }

    ## PS - factor covariance: Symmetric
    if (!is.null(paramSet$PS)) {
		ustart <- c(ustart, startingVal(paramSet$PS@free, draws$PS, smart = TRUE, symm = TRUE))
    }

    ## RPS - factor correlation (same as PS): Symmetric
    if (!is.null(paramSet$RPS)) {
        # Step 1: parse variance information to the RPS
        if (!is.null(paramSet$VPS)) {
            diag(paramSet$RPS@free) <- paramSet$VPS@free
        } else if (!is.null(paramSet$VE)) {
            # Intentionally use else if to select either VPS or VE
            diag(paramSet$RPS@free) <- paramSet$VE@free
        }

        # Step 2: create pt
        ustart <- c(ustart, startingVal(paramSet$RPS@free, draws$PS, smart = TRUE, symm = TRUE))
    }


    ## TE - Covariance of measurement error: Symmetric
    if (!is.null(paramSet$TE)) {
        ustart <- c(ustart, startingVal(paramSet$TE@free, draws$TE, smart = TRUE, symm = TRUE))
    }

    ## RTE - Correlation of measurment error: Symmetric
    if (!is.null(paramSet$RTE)) {
        # Step 1: parse variance information to the RTE
        if (!is.null(paramSet$VTE)) {
            diag(paramSet$RTE@free) <- paramSet$VTE@free
        } else if (!is.null(paramSet$VY)) {
            # Intentionally use else if to select either VPS or VE
            diag(paramSet$RTE@free) <- paramSet$VY@free
        }

        # Step 2: create pt
        ustart <- c(ustart, startingVal(paramSet$RTE@free, draws$TE, smart = TRUE, symm = TRUE))
    }

    ## BE - Regressions among factors
    if (!is.null(paramSet$BE)) {
        ustart <- c(ustart, startingVal(paramSet$BE@free, draws$BE, smart = TRUE, symm = FALSE))
    }


    ## AL - factor intercept

    # if ME is not null but AL is null
    if (!is.null(paramSet$ME) && is.null(paramSet$AL)) {
        paramSet$AL <- paramSet$ME
    }

    # Create pt
    if (!is.null(paramSet$AL)) {
        ustart <- c(ustart, startingVal(paramSet$AL@free, draws$AL, smart = TRUE, symm = FALSE))
    }

    ## TY - indicator intercept

    # if MY is not null but TY is null
    if (!is.null(paramSet$MY) && is.null(paramSet$TY)) {
        paramSet$TY <- paramSet$MY
    }

    # Create pt
    if (!is.null(paramSet$TY)) {
        ustart <- c(ustart, startingVal(paramSet$TY@free, draws$TY, smart = TRUE, symm = FALSE))
    }


	nz <- NULL # Save for create covariances and means among covariates

    ## GA - Regressions of factors on covariates
    if (!is.null(paramSet$GA)) {
		nz <- ncol(paramSet$GA@free)
        ustart <- c(ustart, startingVal(paramSet$GA@free, draws$GA, smart = TRUE, symm = FALSE))
    }

    ## KA - Regressions of factors on indicators
    if (!is.null(paramSet$KA)) {
		nz <- ncol(paramSet$KA@free)
        ustart <- c(ustart, startingVal(paramSet$KA@free, draws$KA, smart = TRUE, symm = FALSE))
    }

	# Create parameter table for covariates
	if(!is.null(nz)) {
		ustart <- c(ustart, startingVal(matrix(0, nz, nz), matrix(0, nz, nz), smart = TRUE, symm = TRUE))
		ustart <- c(ustart, startingVal(rep(0, nz), rep(0, nz), smart = TRUE, symm = FALSE))
	}
    return(ustart)
}

is.partable <- function(object) {
  ALLNAMES <- c("id","lhs","op","rhs","user","group","free","ustart","exo",
                "label","plabel","start","est","se","eq.id","unco")
  # leave eq.id and unco for reverse comptability.
  ## 14 April 2016:  Terry added new names "est" and "se"
	is.list(object) && all(names(object) %in% ALLNAMES)
}

is.lavaancall <- function(object) {
	is.list(object) && ("model" %in% names(object))
}

changeDupLab <- function(pt) {
	temppt <- pt
	temppt$label <- rep("", length(temppt$label))
	templab <- lavaan::lav_partable_labels(temppt)
	uniquelab <- setdiff(pt$label, "")
	for(i in seq_along(uniquelab)) {
		pos <- which(uniquelab[i] == pt$label)
		if(length(pos) > 1) {
			for(j in pos) {
				nameparam <- paste0("<- (", templab[j], ")")
				pt$label[j] <- paste(pt$label[j], nameparam)
			}
		}
	}
	pt
}
