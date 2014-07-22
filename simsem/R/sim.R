
sim <- function(nRep = NULL, model = NULL, n = NULL, generate = NULL, ..., rawData = NULL, miss = NULL, datafun = NULL, 
	lavaanfun = "lavaan", outfun = NULL, outfundata = NULL, pmMCAR = NULL, pmMAR = NULL, facDist = NULL, indDist = NULL, errorDist = NULL, 
    sequential = FALSE, saveLatentVar = FALSE, modelBoot = FALSE, realData = NULL, covData = NULL, maxDraw = 50, misfitType = "f0", 
    misfitBounds = NULL, averageNumMisspec = FALSE, optMisfit = NULL, optDraws = 50, createOrder = c(1, 2, 3), 
    aux = NULL, group = NULL, mxFit = FALSE, mxMixture = FALSE, citype = NULL, cilevel = 0.95,
	seed = 123321, silent = FALSE, multicore = options('simsem.multicore')[[1]], cluster = FALSE, 
    numProc = NULL, paramOnly = FALSE, dataOnly = FALSE, smartStart = FALSE, previousSim = NULL, completeRep = FALSE,
                stopOnError = FALSE) {
	
  mc <- match.call()
	#Add inspect function for anything
	#Update function. Takes results object. Or takes model object.
	#Speed things: functions in c or c++ (maybe drawparam), look at sugar functions
	#Difference percent missing in different groups
    start.time0 <- start.time <- proc.time()[3]
    timing <- list()
	timing$StartTime <- Sys.time()
    require(parallel)
    RNGkind("L'Ecuyer-CMRG")
	if(is.null(previousSim)) {
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
	if(!is.null(generate)) {
		if(is.character(generate)) {
			generate <- list(model = generate)
			lavaanGenerate <- TRUE
		} else if (is.partable(generate)) {
			generate <- list(model = generate)
			lavaanGenerate <- TRUE
		} else if (is.lavaancall(generate)) {
			lavaanGenerate <- TRUE
		} else if (is(generate, "lavaan")) {
			temp <- generate@ParTable
			temp$ustart <- generate@Fit@est
			generate <- list(model = temp)
			lavaanGenerate <- TRUE
		} else if (is(generate, "MxModel")) {
			mxGenerate <- TRUE
		} else if (is(generate, "function")) {
			functionGenerate <- TRUE
		} else if (is(generate, "SimSem")) {
			# Do nothing
		} else {
			stop("Please specify an appropriate object for the 'generate' argument: simsem model template, lavaan script, lavaan parameter table, OpenMx object, or list of options for the 'simulateData' function.")
		}
	}
	
	lavaanAnalysis <- FALSE
	mxAnalysis <- FALSE
	functionAnalysis <- FALSE
	if(is.character(model)) {
		model <- list(model = model)
		lavaanAnalysis <- TRUE
	} else if (is.partable(model)) {
		model <- list(model = model)
		lavaanAnalysis <- TRUE
	} else if (is.lavaancall(model)) {
		lavaanAnalysis <- TRUE
	} else if (is(model, "lavaan")) {
		model <- list(model = model@ParTable)
		lavaanAnalysis <- TRUE
	} else if (is(model, "MxModel")) {
		mxAnalysis <- TRUE
	} else if (is(model, "SimSem")) {
		# Do nothing
	} else if (is(model, "function")) {
		functionAnalysis <- TRUE
	} else {
		stop("Please specify an appropriate object for the 'model' argument: simsem model template, lavaan script, lavaan parameter table, or list of options for the 'lavaan' function.")
	}
	
	if(lavaanAnalysis) {
		model <- c(model, list(...))
		if(!("group" %in% names(model)) & "group" %in% names(mc)) model$group <- group
	}
	
	if(mxAnalysis) {
		if(length(model@submodels) > 0) {
			if(!is(model@submodels[[1]]@objective, "MxRAMObjective") && all(is.na(model@submodels[[1]]@objective@means))) stop("The expected means must be specified in the objective of the 'model' argument.")
		} else {
			if(!is(model@objective, "MxRAMObjective") && all(is.na(model@objective@means))) stop("The expected means must be specified in the objective of the 'model' argument.")
		}
	}
	
	## Save arguments for completeRep = TRUE
	if(is.logical(completeRep)) {
		if(completeRep) {
			completeRep <- nRep
		} else {
			completeRep <- 0
		}
	}
	nInitial <- n
	pmMCARInitial <- pmMCAR
	pmMARInitial <- pmMAR
	nRepInitial <- nRep
	
    ## 1. Set up correct data generation template (move inside the runRep).

	
	# Find the number of groups
	#Change in draw param so we always have a nested list (even with 1 group).
	#Then get rid of the if/else.
	ngroups <- 1
	if(!is.null(generate)) {
		if(lavaanGenerate) {
			if(is.partable(generate$model)) {
				ngroups <- max(generate$model$group)
			} else {
				if(is.list(n)) ngroups <- length(n)
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
		if(lavaanAnalysis) {
			if(is.partable(model$model)) {
				ngroups <- max(model$model$group)
			} else {
				if(!is.null(rawData) && !is.null(model$group)) {
					if(is.data.frame(rawData) || is.matrix(rawData)) {
						ngroups <- length(unique(rawData[,model$group]))
					} else {
						ngroups <- length(unique(rawData[[1]][,model$group]))
					}
				} else {
					if(is.list(n)) ngroups <- length(n)
				}
			}
		} else if (mxAnalysis) {
			ngroups <- length(model@submodels)
			if(ngroups == 0) ngroups <- 1
		} else if (functionAnalysis) {
			ngroups <- 1 # Intentionally use 1 because, to get at this point, only raw data are provided. The number of group will does not matter.
		} else {
			ngroups <- max(model@pt$group)
		}
	}

    timing$SimulationParams <- (proc.time()[3] - start.time0)
    start.time <- proc.time()[3]

    ## 2. Compute combinations of simulation parameters (MAR, MCAR, n): complete
    ## factorial
	
	
	if(is.null(rawData)) {
		# If the rawData is not specified, the n value must be valid.
		if(!is.list(n)) {
			#If multiple groups then...
			# Make n as a list to represent sample size of each group
			if (length(n) == 1 && !is.null(nRep)) { 
			#For a single specified value of n, each rep has the same n
				n <- rep(n, nRep)
			} 
			#Make as a list, then the same sample size for all groups
			n <- list(n)
			n <- rep(n, ngroups)
		} else {
			# If n is a list, thus multiple groups, make sure that the length is equal.
			#Length for each group can be 1 or nRep
			if(length(unique(sapply(n, length))) != 1) {
				stop("You must specify the same number of sample sizes for each group; each list component in the 'n' argument must have the same length.")
			} else {
			#This is for when there is a single sample size for all reps.
				if (length(n[[1]]) == 1 && !is.null(nRep)) { 
					n <- lapply(n, rep, nRep)
				}
			}
		}
		# If the nRep is not NULL, the length of sample size currently must equal to the number of replications.
		if (!is.null(nRep) && (length(n[[1]]) != nRep)) stop("When the number of replications argument 'nRep' is used, sample size 'n' must be a single value, a vector with length equal to 'nRep', or a vector with length equal to a multiple of 'nRep'")
		if (is.null(nRep)) {
			if (!is.null(pmMCAR) && !is.vector(pmMCAR)) 
				stop("Please specify the number of replications 'nRep'")
			if (!is.null(pmMAR) && !is.vector(pmMAR)) 
				stop("Please specify the number of replications 'nRep'")
			
			usedMCAR <- NULL
			usedMAR <- NULL
			ifelse(is.null(pmMCAR), usedMCAR <- 0, usedMCAR <- pmMCAR)
			ifelse(is.null(pmMAR), usedMAR <- 0, usedMAR <- pmMAR)
			
			out <- expand.grid(1:length(n[[1]]), usedMCAR, usedMAR)
			
			if (!is.null(pmMCAR)) 
				pmMCAR <- out[, 2]
			if (!is.null(pmMAR)) 
				pmMAR <- out[, 3]
			n <- lapply(n, function(x, y) x[y], y=out[,1])
			nRep <- nrow(out)
		}
	} else {
		nRep <- length(rawData)
		if(!is.null(n)) {
			warning("The sample size argument 'n' is suppressed when 'rawData' is specified")
			n <- NULL
		}
	}
	
    ## 3. Adjust pmMCAR and pmMAR

	if(!is.null(pmMCAR)) {
		#If there is one sample size, pmMCAR and pmMAR specified
		if(length(pmMCAR) == 1) {
			pmMCAR <- rep(pmMCAR, nRep)
		} else if (length(pmMCAR) == nRep) {
			# Do nothing
		} else {
			stop("When the 'rawData' argument is specified, the percent missing completely at random 'pmMCAR' must either be a single value, or a vector with length equal to the number of datasets")
		}
	}
	if(!is.null(pmMAR)) {
		if(length(pmMAR) == 1) {
			pmMAR <- rep(pmMAR, nRep)
		} else if (length(pmMAR) == nRep) {
			# Do nothing
		} else {
			stop("When the 'rawData' argument is specified, the percent missing at random 'pmMCAR' must either be a single value, or a vector with length equal to the number of datasets")
		}		
	}
    
    timing$RandomSimParams <- (proc.time()[3] - start.time)
    start.time <- proc.time()[3]
    
    ## 4. Build list of simulation conditions. Each element of simConds is a
    ## replication.
    
    simConds <- list()
    
    
    numseed <- list()
    s <- .Random.seed
    for (i in 1:nRep) {
        numseed[[i]] <- s
        s <- nextRNGStream(s)
    }
	#Save last seed to the result object? numseed[[nRep]]
	#Use for update method
    
    if (is.null(rawData)) {
        
        for (i in seq_len(nRep)) {
            simConds[[i]] <- list()
            simConds[[i]][[1]] <- NULL 				# Rawdata
            simConds[[i]][[2]] <- sapply(n, "[", i) # Sample Size
            simConds[[i]][[3]] <- pmMCAR[i] 		# Percent Missing Completely at Random
            simConds[[i]][[4]] <- pmMAR[i] 			# Percent Missing at random
            simConds[[i]][[5]] <- numseed[[i]] 		# L'Ecuyer random seed
			simConds[[i]][[6]] <- FALSE				# Skip impose missing values
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
    } else if (is.list(rawData)) {
		if (is.data.frame(rawData[[1]])) {
			# Do nothing
		} else if (is.matrix(rawData[[1]])) {
			rawData <- lapply(rawData, data.frame)
		} else {
			stop("Check the list object specified in the 'rawData' argument; list must either contain matrices or data frames")
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
        stop("Check the object specified in 'rawData' argument; object must either be a SimData class or a list of data frames.")
    }
    
    timing$SimConditions <- (proc.time()[3] - start.time)
    start.time <- proc.time()[3]
    
	# Suppress warnings starting from here!
    warnT <- as.numeric(options("warn"))
    if (silent) 
        options(warn = -1)
    
    ## 5. Run replications
    if (multicore) {
		if(!silent) cat("Progress tracker is not available when 'multicore' is TRUE.\n")
        sys <- .Platform$OS.type
        if (is.null(numProc)) 
            numProc <- detectCores()
        if (sys == "windows") {
            cl <- makeCluster(rep("localhost", numProc), type = "SOCK")
            Result.l <- clusterApplyLB(cl, simConds, runRep, model = model, generate = generate, 
                miss = miss, datafun = datafun, lavaanfun = lavaanfun, outfun = outfun, outfundata = outfundata, silent = silent, 
                facDist = facDist, indDist = indDist, errorDist = errorDist, sequential = sequential, saveLatentVar = saveLatentVar,
                realData = realData, covData = covData, maxDraw = maxDraw, misfitBounds = misfitBounds, 
                averageNumMisspec = averageNumMisspec, optMisfit = optMisfit, optDraws = optDraws, createOrder = createOrder, misfitType = misfitType, aux = aux, paramOnly = paramOnly, dataOnly = dataOnly, smartStart = smartStart, popData = popData, group = group, mxFit = mxFit, mxMixture = mxMixture, citype = citype, cilevel = cilevel, stopOnError = stopOnError, ...)
            stopCluster(cl)
        } else {
            Result.l <- mclapply(simConds, runRep, model = model, generate = generate, 
                miss = miss, datafun = datafun, lavaanfun = lavaanfun, outfun = outfun, outfundata = outfundata, silent = silent, 
                facDist = facDist, indDist = indDist, errorDist = errorDist, sequential = sequential, saveLatentVar = saveLatentVar,
                realData = realData, covData = covData, maxDraw = maxDraw, misfitBounds = misfitBounds, 
                averageNumMisspec = averageNumMisspec, optMisfit = optMisfit, optDraws = optDraws, createOrder = createOrder, 
                misfitType = misfitType, aux = aux, mc.cores = numProc, paramOnly = paramOnly, dataOnly = dataOnly, smartStart = smartStart, popData = popData, group = group, mxFit = mxFit, mxMixture = mxMixture, citype = citype, cilevel = cilevel, stopOnError = stopOnError, ...)
        }
    } else {
        numJobs <- length(simConds)

        Result.l <- lapply(1:length(simConds), function(i, ...){
          # Write progress
          if(!silent) cat("Progress:", i, "/", numJobs, "\n")
          runRep(simConds[[i]], ...)
        },  model = model, generate = generate, 
            miss = miss, datafun = datafun, lavaanfun = lavaanfun, outfun = outfun, outfundata = outfundata, silent = silent, facDist = facDist, 
            indDist = indDist, errorDist = errorDist, sequential = sequential, saveLatentVar = saveLatentVar, realData = realData, covData = covData, 
            maxDraw = maxDraw, misfitBounds = misfitBounds, averageNumMisspec = averageNumMisspec, 
            optMisfit = optMisfit, optDraws = optDraws,  createOrder = createOrder, misfitType = misfitType, 
            aux = aux, paramOnly = paramOnly, dataOnly = dataOnly, smartStart = smartStart, popData = popData, group = group, mxFit = mxFit, mxMixture = mxMixture, citype = citype, cilevel = cilevel, stopOnError = stopOnError, ...)
    }

	################## Extract out popData ##################################
	
	popResult <- NULL
	if(isPopulation) {
		popResult <- Result.l[[nRep + 1]]
		Result.l[[nRep + 1]] <- NULL
	}
	
	
	
	if(dataOnly) {
		## Return data when dataOnly is requested
		return(Result.l)
	} else {
	#Now we create a SimResult object
		timing.l <- lapply(Result.l, function(x) {
			x$timing
		})
		timing$InReps <- colMeans(matrix(unlist(timing.l), nrow = nRep, byrow = TRUE))
		names(timing$InReps) <- names(timing.l[[1]])
		
		timing$RunReplications <- (proc.time()[3] - start.time)
		start.time <- proc.time()[3]
    	
		## 6. Extract results from replication lists
		fit.l <- lapply(Result.l, function(rep) {
			rep$fit
		})
		coef.l <- lapply(Result.l, function(rep) {
			rep$coef
		})
		se.l <- lapply(Result.l, function(rep) {
			rep$se
		})
		converged.l <- lapply(Result.l, function(rep) {
			rep$converged
		})
		param.l <- lapply(Result.l, function(rep) {
			rep$param
		})
		FMI1.l <- lapply(Result.l, function(rep) {
			rep$FMI1
		})
		FMI2.l <- lapply(Result.l, function(rep) {
			rep$FMI2
		})
		cilower.l <- lapply(Result.l, function(rep) {
			rep$cilower
		})
		ciupper.l <- lapply(Result.l, function(rep) {
			rep$ciupper
		})
		std.l <- lapply(Result.l, function(rep) {
			rep$std
		})
		extra <- list()
		if (!is.null(outfun) || !is.null(outfundata)) {
			extra <- lapply(Result.l, function(rep) {
				rep$extra
			})
		}
		popMis.l <- lapply(Result.l, function(rep) {
			rep$popMis
		})
		misfitOut.l <- lapply(Result.l, function(rep) {
			rep$misfitOut
		})
		
		coef <- as.data.frame(do.call(rbind, coef.l))
		se <- as.data.frame(do.call(rbind, se.l))
		fit <- as.data.frame(do.call(rbind, fit.l))
		converged <- as.vector(unlist(converged.l))
		
		if(paramOnly) converged <- rep(TRUE, length(converged))
		param <- data.frame()
		FMI1 <- NULL
		FMI2 <- NULL
		cilower <- NULL
		ciupper <- NULL
		popMis <- NULL
		misfitOut <- NULL
		if (!is.null(param.l[[1]])) {
			param <- as.data.frame(do.call(rbind, param.l))
			if (nrow(unique(param)) == 1) 
				param <- unique(param)
		}
		
		if(isPopulation) {
			param <- as.data.frame(t(popResult$coef))
		}
		
		if(lavaanGenerate || (is.null(generate) && lavaanAnalysis && is.null(rawData))) {
			if(is.null(generate) && lavaanAnalysis) generate <- model
			if(!is.partable(generate$model)) {
				generate2 <- generate
				generate2$sample.nobs <- simConds[[1]][[2]]
				generate2$return.fit <- TRUE
				pt <- parTable(attr(do.call(lavaan::simulateData, generate2), "fit"))
			} else {
				pt <- generate$model
			}
			extraParamIndex <- pt$op %in% c(">", "<", "==", ":=")
			if(any(extraParamIndex)) {
				con <- list(lhs = pt$lhs[extraParamIndex], op = pt$op[extraParamIndex], rhs = pt$rhs[extraParamIndex])
				hasLab <- pt$label != ""
				extraVal <- applyConScript(pt$label[hasLab][!duplicated(pt$label[hasLab])],  pt$ustart[hasLab][!duplicated(pt$label[hasLab])], con)
				toBeFill <- which(is.na(pt$ustart) & hasLab)
				for(i in seq_along(toBeFill)) {
					pt$ustart[toBeFill[i]] <- extraVal[[2]][extraVal[[1]] == pt$label[toBeFill[i]]]
				}
			}
			param <- pt$ustart
			names(param) <- names(coef(lavaan(pt, sample.nobs=rep(200, max(pt$group))), type="user"))
			param <- as.data.frame(t(param))
		} else if (mxGenerate | (is.null(generate) & is(model, "MxModel"))) {
			if(is.null(generate)) generate <- model
			param <- vectorizeMx(generate)
			param <- as.data.frame(t(param))
		}
		
		if (!is.null(std.l[[1]])) {
			std <- as.data.frame(do.call(rbind, std.l))
			if (sum(dim(std)) == 0) 
				std <- data.frame()
		} else {
			std <- data.frame()
		}

		if (!is.null(FMI1.l[[1]])) {
			FMI1 <- as.data.frame(do.call(rbind, FMI1.l))
			if (sum(dim(FMI1)) == 0) 
				FMI1 <- data.frame()
			if (all(is.na(FMI1))) FMI1 <- data.frame()			
		} else {
			FMI1 <- data.frame()
		}
		
		if (!is.null(FMI2.l[[1]])) {
			FMI2 <- as.data.frame(do.call(rbind, FMI2.l))
			if (sum(dim(FMI2)) == 0) 
				FMI2 <- data.frame()
			if (all(is.na(FMI2))) FMI2 <- data.frame()			
		} else {
			FMI2 <- data.frame()
		}
		
		if (!is.null(cilower.l[[1]])) {
			cilower <- as.data.frame(do.call(rbind, cilower.l))
			if (sum(dim(cilower)) == 0) 
				cilower <- data.frame()
			if (all(is.na(cilower))) cilower <- data.frame()	
		} else {
			cilower <- data.frame()
		}
		
		if (!is.null(ciupper.l[[1]])) {
			ciupper <- as.data.frame(do.call(rbind, ciupper.l))
			if (sum(dim(ciupper)) == 0) 
				ciupper <- data.frame()
			if (all(is.na(ciupper))) ciupper <- data.frame()	
		} else {
			ciupper <- data.frame()
		}
		
		if (!is.null(popMis.l[[1]])) {
			popMis <- as.data.frame(do.call(rbind, popMis.l))
			if (nrow(unique(popMis)) == 1) 
				popMis <- unique(popMis)
			if (all(is.na(popMis))) popMis <- data.frame()
		} else {
		 	popMis <- data.frame()
		}
		
		if (!is.null(misfitOut.l[[1]])) {
			misfitOut <- as.data.frame(do.call(rbind, misfitOut.l))
			if (nrow(unique(misfitOut)) == 1) 
				misfitOut <- unique(misfitOut)
			if (all(is.na(misfitOut))) misfitOut <- data.frame()
		} else {
			misfitOut <- data.frame()
		}
		
		if (is.null(pmMCAR)) 
			ifelse(is.null(miss), pmMCAR <- 0, pmMCAR <- miss@pmMCAR)
		if (is.null(pmMAR)) 
			ifelse(is.null(miss), pmMAR <- 0, pmMAR <- miss@pmMAR)
		
		if(!is.null(rawData) & !isPopulation) {
			if(ngroups > 1) {
				if(lavaanAnalysis) {
					groupLab <- model$group
				} else if (mxAnalysis) {
					groupLab <- group
				} else {
					groupLab <- model@groupLab
				}
				if(is.null(groupLab)) groupLab <- "group"
				nobs <- as.data.frame(t(sapply(rawData, function(x, col) table(x[,col]), col = groupLab)))
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
		if(!lavaanAnalysis) {
			if(mxAnalysis) {
				modelType <- "OpenMx"
			} else if (functionAnalysis) {
				modelType <- "function"
			} else {
				modelType <- model@modelType
			}
		}
		
		colnames(fit) <- tolower(colnames(fit))

		timing$CombineResults <- (proc.time()[3] - start.time)
		start.time <- proc.time()[3]
		timing$EndTime <- Sys.time()
		
		Result <- new("SimResult", modelType = modelType, nRep = nRep, coef = coef, 
			se = se, fit = fit, converged = converged, seed = c(seed, s), paramValue = param, 
			misspecValue = popMis, popFit = misfitOut, FMI1 = FMI1, FMI2 = FMI2, cilower = cilower, ciupper = ciupper,
			stdCoef = std, n = n, nobs=nobs, pmMCAR = pmMCAR, pmMAR = pmMAR, extraOut = extra,
			paramOnly=paramOnly, timing = timing)
		
		if(!is.null(previousSim)) {
			Result <- combineSim(previousSim, Result)
		} 
		
		# If completeRep = TRUE, check whether the number of converged results
		if(completeRep > 0 & !is.null(nRepInitial)) {
			success <- sum(Result@converged == 0)
			pSuccess <- success / Result@nRep
			if(success < completeRep) {
				nRepNew <- ceiling((completeRep - success) / pSuccess) 
				Result <- sim(nRep = nRepNew, model = model, n = nInitial, generate = generate, rawData = rawData, miss = miss, datafun = datafun, lavaanfun = lavaanfun, outfun = outfun, outfundata = outfundata, pmMCAR = pmMCARInitial, pmMAR = pmMARInitial, facDist = facDist, indDist = indDist, errorDist = errorDist, sequential = sequential, saveLatentVar = saveLatentVar, modelBoot = modelBoot, realData = realData, covData = covData, maxDraw = maxDraw, misfitType = misfitType, misfitBounds = misfitBounds, averageNumMisspec = averageNumMisspec, optMisfit = optMisfit, optDraws = optDraws, createOrder = createOrder, aux = aux, group = group, mxFit = mxFit, mxMixture = mxMixture, citype = citype, cilevel = cilevel, seed = seed, silent = silent, multicore = multicore, cluster = cluster, numProc = numProc, paramOnly = paramOnly, dataOnly = dataOnly, smartStart = smartStart, previousSim = Result, completeRep = completeRep, stopOnError = stopOnError, ...)
			}
		}
		
		if (silent) 
			options(warn = warnT)
		return <- Result
	}
}

# runRep: Run one replication

runRep <- function(simConds, model, generate = NULL, miss = NULL, datafun = NULL, lavaanfun = NULL, 
    outfun = NULL, outfundata = NULL, facDist = NULL, indDist = NULL, indLab = NULL, errorDist = NULL, 
    sequential = FALSE, saveLatentVar = FALSE, realData = NULL, covData = NULL, silent = FALSE, modelBoot = FALSE, maxDraw = 50, 
    misfitType = "f0", misfitBounds = NULL, averageNumMisspec = NULL, optMisfit = NULL, 
    optDraws = 50, createOrder = c(1, 2, 3), aux = NULL, paramOnly = FALSE, dataOnly = FALSE, smartStart = TRUE, 
	popData = NULL, group = NULL, mxFit = FALSE, mxMixture = FALSE, citype = NULL, cilevel = 0.95, stopOnError = FALSE, ...) {
    start.time0 <- start.time <- proc.time()[3]
    timing <- list()
    
	param <- NA
    coef <- NA
    se <- NA
    fit <- NA
    std <- NA
    extra <- NULL
	extra2 <- NULL
    FMI1 <- NA
    FMI2 <- NA
	cilower <- NA
	ciupper <- NA
	
	labelParam <- NULL
	paramSet <- NULL
    converged <- 1
    n <- simConds[[2]]
    pmMCAR <- simConds[[3]]
    pmMAR <- simConds[[4]]
    RNGkind("L'Ecuyer-CMRG")
    assign(".Random.seed", simConds[[5]], envir = .GlobalEnv)
	skipMiss <- simConds[[6]]
    specifiedGenerate <- TRUE
	
    if (is.null(generate)) {
        generate <- model
		specifiedGenerate <- FALSE
    }
	
	#Two things to think about: 1. Only do this when the generating and analysis models are different
	#2. Why do we do this for each rep? Move up to sim/model and pass values to runRep. Add 2 slots to SimSem class, put it there.
	#3. Also don't do this with rawData

    ## 1. Create a missing data template from simulation parameters.
	##Creates SimMissing object if none exists (but pmMCAR or pmMAR are specified)
	##If a SimMissing object does exist it sets pmMCAR and pmMAR based on the SimMissing object
    if (is.null(miss)) {
        if (!is.null(pmMAR) | !is.null(pmMCAR)) {
            if (is.null(pmMCAR)) 
                pmMCAR <- 0
            if (is.null(pmMAR)) 
                pmMAR <- 0
            miss <- miss(pmMCAR = pmMCAR, pmMAR = pmMAR, ignoreCols = "group")
        }
    } else {
        if (is.null(pmMCAR)) 
            pmMCAR <- miss@pmMCAR
        if (is.null(pmMAR)) 
            pmMAR <- miss@pmMAR
    }
    ## 2. Generate data (data) & store parameter values (paramSet)
    data <- simConds[[1]]  # either a paramSet or raw data
	if (!is.null(popData)) {
		if(is.null(n)) {
			data <- popData
		} else {
			groupLab <- NULL
			if(is(model, "SimSem")) {
				groupLab <- model@groupLab
			} else if (is(model, "function")) {
				# Intentionally leave as blank
			} else {
				groupLab <- model$group
			}
			if(!is.null(groupLab) && (groupLab %in% colnames(popData))) {
				data <- split(popData, popData[,groupLab])
				data <- mapply(function(dat, ss) dat[sample(nrow(dat), ss),], dat=data, ss=n, SIMPLIFY=FALSE)
				data <- data.frame(do.call(rbind, data))
			} else {
				data <- popData[sample(nrow(popData), n),]
			}
		}
	}

	if (is.null(data) && is(generate, "function")) {
		if(stopOnError){
			data <- generate(n)
		} else if (silent) {
			invisible(capture.output(suppressMessages(try(data <- generate(n), silent = TRUE))))
		} else {
			try(data <- generate(n))
		}
	}

	if (is.null(data) && is.lavaancall(generate)) {
		generate$sample.nobs <- n
		generate$indDist <- indDist
		data <- do.call("lavaanSimulateData", generate) # Change to simulateData when the bug is fixed
	}

	if (is.null(data) && is(generate, "MxModel")) {
		data <- generateMx(generate, n=n, indDist=indDist, covData=covData)
	}
	
    if (is.null(data)) {
		# Label variables for creating labels later
		indLabGen <- NULL
		if (generate@modelType == "path") {
			indLabGen <- unique(generate@pt$lhs)
		} else {
			indLabGen <- unique(generate@pt$rhs[generate@pt$op == "=~"])
		}
		facLabGen <- NULL
		if (generate@modelType != "path") {
			facLabGen <- unique(generate@pt$lhs[generate@pt$op == "=~"])
		}
		covLabGen <- generate@pt$lhs[generate@pt$op == "~1" & generate@pt$exo == 1]
		if (length(covLabGen) == 0) covLabGen <- NULL
	
		# Need to draw parameters
		genout <- generateSimSem(model = generate, n = n, maxDraw = maxDraw, misfitBounds = misfitBounds, 
			misfitType = misfitType, averageNumMisspec = averageNumMisspec, optMisfit = optMisfit, 
			optDraws = optDraws, createOrder = createOrder, indDist = indDist, sequential = sequential, saveLatentVar = saveLatentVar, 
			facDist = facDist, errorDist = errorDist, indLab = indLab, modelBoot = modelBoot, 
			realData = realData, covData = covData, params = TRUE)
		data <- genout[[1]]
		psl <- genout[[2]]  
		
		# We need the real parameter values regardless of having model misspecification 
		# because the real parameter values are what we really need to infer
		paramSet <- lapply(psl, "[[", 1) 
		
		# Save parameter values in the parameter tables
		generatedgen <- generate@dgen
		popParam <- NULL
		if (!is.list(generatedgen[[1]])) {
			generatedgen <- list(generatedgen)
		}
		for (i in seq_along(paramSet)) {
			popParam <- c(popParam, parsePopulation(generatedgen[[i]], paramSet[[i]], group = i))
		}
		
		if(smartStart) {
		#Once indLab and facLab are in the model object, sub them in for indLabGen and facLabGen
			if(specifiedGenerate) stop("The smartStart option is not available when the analysis model and data-generation model are different")
			model@pt$ustart <- c(popParam, rep(NA, length(model@pt$ustart) - length(popParam)))
		}
    }  # else: do nothing. Raw data.
	
    timing$GenerateData <- (proc.time()[3] - start.time)
    start.time <- proc.time()[3]
    ## 3. Impose Missing (if any)
    if (!is.null(miss) & !skipMiss) {
        data <- impose(miss, data)
    }
	
    timing$ImposeMissing <- (proc.time()[3] - start.time)
    start.time <- proc.time()[3]
    ## 4. Call user function (if exists)
    if (!is.null(datafun)) {
        data <- datafun(data)
    }
    timing$UserFun <- (proc.time()[3] - start.time)
    start.time <- proc.time()[3]
    ## 5. Call lavaan using simsem template and generated data from 2.
    out <- NULL
	mxAnalysis <- FALSE
	if(!paramOnly & !dataOnly) {
		# Impute missing and run results
		# Will use analyze either when there is a missing object or auxiliary variables specified. 
		# If users provide their own data there maybe a case with auxiliary variables and no missing object
		if(is(model, "function")) {
			if(stopOnError){
			out <- model(data)
			} else if (silent) {
				invisible(capture.output(suppressMessages(try(out <- model(data), silent = TRUE))))
			} else {
				try(out <- model(data))
			}
		} else if (is.lavaancall(model)) {
			model$data <- data
			if(stopOnError){
			  out <- analyzeLavaan(model, lavaanfun, miss, aux)
			} else if (silent) {
				invisible(capture.output(suppressMessages(try(out <- analyzeLavaan(model, lavaanfun, miss, aux), silent = TRUE))))
			} else {
				try(out <- analyzeLavaan(model, lavaanfun, miss, aux))
			}
		} else if (is(model, "MxModel")) {
			mxAnalysis <- TRUE
			multigroup <- length(model@submodels) > 0
			if(multigroup) {
				if(is.null(group)) group <- "group"
			} else {
				group <- NULL
			}
			if(stopOnError){
			  out <- analyzeMx(model, data, groupLab = group, ...)
			} else if (silent) {
				invisible(capture.output(suppressMessages(try(out <- analyzeMx(model, data, groupLab = group, ...), silent = TRUE))))
			} else {
				try(out <- analyzeMx(model, data, groupLab = group, mxMixture = mxMixture, ...))
			}
		} else {
			if (!is.null(miss) | !is.null(aux)) {
			  if(stopOnError){
			    out <- analyzeSimSem(model, data, aux = aux, miss = miss, ...)
			  } else if (silent) {
				invisible(capture.output(suppressMessages(try(out <- analyzeSimSem(model, data, aux = aux, miss = miss, ...), silent = TRUE))))
			  } else {
				try(out <- analyzeSimSem(model, data, aux = aux, miss = miss, ...))
			  }
			} else {
			  if(stopOnError){
			    out <- anal(model, data, ...)
			  } else if (silent) {
				invisible(capture.output(suppressMessages(try(out <- anal(model, data, ...), silent = TRUE))))
			  } else {
				try(out <- anal(model, data, ...))
			  }
			}
		}
    }
    timing$Analyze <- (proc.time()[3] - start.time)
    start.time <- proc.time()[3]
    
    ## 6. Parse Lavaan Output
	if (!dataOnly) {
		if (!is.null(out)) {
			if(is(model, "function")) {
				converged <- out$converged
				if(is.null(converged)) stop("In the function for data analysis, please specify the 'converged' in the resulting list")
				if(is.logical(converged)) {
					if(converged) {
						converged <- 0
					} else {
						converged <- 1
					}
				} 
			} else if (mxAnalysis) {
				try(converged.l <- out@output$status)
				converged <- 0
				if(converged.l[[2]] != 0) {
					converged <- 1
				}
				if(converged.l[[1]] == 1) {
					converged <- 0
				} else if (converged.l[[1]] == 6) {
					converged <- 6
				} else if (converged.l[[1]] == 0) {
					converged <- 0
				} else {
					converged <- 1
				}
				if(converged == 0) {
					seTemp <- out@output$standardErrors
					improperSE <- any(unlist(seTemp) < 0) | any(is.na(unlist(seTemp))) | all(unlist(seTemp) == 0)
					if (improperSE) {
						converged <- 3
					}
				}
			} else {
				try(converged <- as.numeric(!inspect(out, "converged")))
				#Below is only for multiple imputation. If the number of converged imputations is below the threshold then converged = 2
				if(is(out, "lavaanStar") && length(out@imputed) > 0) {
					if(out@imputed[[1]][1] < miss@convergentCutoff) converged <- 2
				}
				if(converged == 0) {
					seTemp <- inspect(out, "se")
					improperSE <- any(unlist(seTemp) < 0) | any(is.na(unlist(seTemp))) | all(unlist(seTemp) == 0)
					if (improperSE) {
						converged <- 3
					}
					if (checkVar(out)) {
						converged <- 4
					} else if(checkCov(out)) {
						converged <- 5
					}
				}			
			}
		}
		
		#reminder: out=lavaan object
		if (converged %in% c(0, 3:6)) {
			if(is(model, "function")) {
				fit <- out$fit
				coef <- out$coef
				se <- out$se
				std <- out$std
				extra <- out$extra
				FMI1 <- out$FMI1
				FMI2 <- out$FMI2
				cilower <- out$cilower
				ciupper <- out$ciupper
			} else if(mxAnalysis) {
				if(mxFit) {
					fit <- NA
					try(fit <- semTools::fitMeasuresMx(out), silent = silent)
					if(!all(is.na(fit))) {
						availfit <- names(fit)
						if("saturate.status" %in% availfit) {
							set1 <- intersect(c("logl", "npar", "aic", "bic"), availfit)
							set2 <- intersect(c("cfi", "tli", "nnfi", "pnfi", "rfi", "nfi", "ifi", "rni", "baseline.chisq", "baseline.pvalue"), availfit)
							set3 <- setdiff(availfit, c(set1, set2))
							if(!(fit["saturate.status"] %in% c(0, 1))) fit[c(set2, set3)] <- NA
							if(!(fit["null.status"] %in% c(0, 1))) fit[set2] <- NA
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
				if(nrow(ci) > 0) {
					cilower <- ci[,1]
					ciupper <- ci[,2]
					nameci <- gsub(paste0(out@name, "."), "", rownames(ci))
					names(cilower) <- nameci
					names(ciupper) <- nameci
				}
				
				if(length(out@submodels) > 0) {
					defVars <- lapply(out@submodels, findDefVars)
					defVars <- do.call(c, defVars)
					if(is(out@submodels[[1]]@objective, "MxRAMObjective") && !(length(defVars) > 0)) findStd <- TRUE
				} else {
					defVars <- findDefVars(out)	
					if(is(out@objective, "MxRAMObjective") && !(length(defVars) > 0)) findStd <- TRUE
				}
				if(findStd) {
					std <- NA
					try(std <- semTools::standardizeMx(out, free = TRUE), silent = silent)
				}
				if (!is.null(outfun)) {
					extra <- outfun(out)
				}
				if (!is.null(outfundata)) {
					extra2 <- outfundata(out, data)
				}
			} else {
				fit <- inspect(out, "fit") # Avoid fitMeasures function becuase the runMI function does not support the fitMeasures function.
				
				if(is.null(citype)) citype <- formals(parameterEstimates)$boot.ci.type
				#redo with parameterEstimate function in lavaan (for coef se, std) all the way to 526

				result <- parameterEstimates(out, standardized=TRUE, boot.ci.type=citype, level = cilevel)
				outpt <- out@ParTable
				extraParamIndex <- outpt$op %in% c(">", "<", "==", ":=")
				index <- ((outpt$free != 0) & !(duplicated(outpt$free))) | extraParamIndex
				coef <- result$est[index]
				se <- out@Fit@se[index]
				std <- result$std.all[index]
				cilower <- result$ci.lower[index]
				ciupper <- result$ci.upper[index]
				FMI1 <- result$fmi[index]
				lab <- names(coef(lavaan(outpt, sample.nobs=rep(200, max(outpt$group)))))
				if(any(extraParamIndex)) {
					if(!is.lavaancall(model)) {
						lab <- c(lab, renameExtraParam(model@con$lhs, model@con$op, model@con$rhs))
					} else {
						lab <- c(lab, renameExtraParam(outpt$lhs[extraParamIndex], outpt$op[extraParamIndex], outpt$rhs[extraParamIndex]))
					}
				}
				names(coef) <- lab
				names(se) <- lab
				names(std) <- lab
				if(!is.null(cilower)) names(cilower) <- lab
				if(!is.null(ciupper)) names(ciupper) <- lab
				if(!is.null(FMI1)) names(FMI1) <- lab
				
				## 6.1. Call output function (if exists)
				if (!is.null(outfun)) {
					extra <- outfun(out)
				}
				if (!is.null(outfundata)) {
					extra2 <- outfundata(out, data)
				}
				
				if(!is.null(miss) && miss@m > 0) {
					if (converged %in% c(0, 3:5)) {
						fmiOut <- out@imputed[[2]]
						FMI1 <- fmiOut[,5][index]
						FMI2 <- fmiOut[,6][index]
						names(FMI1) <- lab
						names(FMI2) <- lab
					} 	
				}
				#Finish FIML FMI below! from parameterEstimates
				if(!is.null(miss) && miss@m == 0){
				}
			}
		} 
		
		## Keep parameters regardless of convergence - may want to examine
		## non-convergent sets
		if (!is.null(paramSet)) {
			if (!is.null(psl[[1]]$misspec)) {
				misParamSet <- lapply(psl, "[[", 3)
				popMis <- reduceMisspecSet(misParamSet, generate@modelType != "path", 
					indLabGen, facLabGen, covLab = covLabGen)
				p <- length(indLabGen)
				nElements <- (p + (p * (p + 1)/2)) * length(psl)
				dfParam <- nElements - max(generate@pt$free)
				misfitOut <- popMisfitParams(psl, df = dfParam, covData = covData)
			} else {
				popMis <- NA
				misfitOut <- NA
			}
			
			extraParamIndex <- generate@pt$op %in% c(">", "<", "==", ":=")
			extraParamName <- NULL
			if(any(extraParamIndex)) {
				extraparam <- collapseExtraParam(paramSet, generate@dgen, fill=TRUE, con=generate@con)
				extraParamName <- renameExtraParam(generate@con$lhs, generate@con$op, generate@con$rhs)
				popParam[extraParamIndex] <- extraparam
			}
			index <- ((generate@pt$free != 0)& !(duplicated(generate@pt$free))) | extraParamIndex
			popParam <- popParam[index]
			names(popParam) <- c(names(coef(lavaan(generate@pt, sample.nobs=rep(200, max(generate@pt$group))))), extraParamName)
		} else {
			popParam <- NA  # Real Data
			popMis <- NA  # Misspecfication
			misfitOut <- NA  # Misfit indices for misspecification
		}
		


		timing$ParseOutput <- (proc.time()[3] - start.time)
		start.time <- proc.time()[3]
		if(is.null(extra) & !is.null(extra2)) {
			extra <- extra2
		} else if (!is.null(extra) & !is.null(extra2)) {
			extra <- list(extra, extra2)
		}
		
		Result <- list(coef = coef, se = se, fit = fit, converged = converged, param = popParam, 
        FMI1 = FMI1, FMI2 = FMI2, std = std, timing = timing, extra = extra, popMis = popMis, cilower = cilower, ciupper = ciupper,
        misfitOut = misfitOut)
		return(Result)
	} else {
		return(data)
	}
}

##MispecSet is still needed but paramSet above can be replaced
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

##Check to see if random parameters are in the model. Can use to simplfy runRep
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

collapseExtraParam <- function(pls, dgen, fill=TRUE, con=NULL) {
	# Collapse all labels
	if(length(pls) == 1) dgen <- list(dgen)
	temp <- extractLab(pls, dgen, fill=fill, con=con)
	target <- temp[[1]]
	realval <- temp[[2]]
	oldop <- con$op
	for(i in 1:length(con[[1]])) {
		if(con[[2]][i] %in% c(">", "==")) {
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

renameExtraParam <- function(lhs, op, rhs) {
	for(i in 1:length(lhs)) {
		if(op[i] %in% c(">", "==")) {
			lhs[i] <- paste("[", lhs[i], "]", "-", "[", rhs[i], "]")
		} else if (op[i] == "<") {
			lhs[i] <- paste("[", rhs[i], "]", "-", "[", lhs[i], "]")
		}
	}
	lhs	
}



# The steps follows the buildPT function.
parsePopulation <- function(paramSet, draws, group = 1) {
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
	is.list(object) && all(names(object) %in% c("id", "lhs", "op", "rhs", "user", "group", "free", "ustart", "exo", "label", "eq.id", "unco"))
}

is.lavaancall <- function(object) {
	is.list(object) && ("model" %in% names(object))
}