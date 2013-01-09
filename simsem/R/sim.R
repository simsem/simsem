
sim <- function(nRep = NULL, model = NULL, n = NULL, generate = NULL, rawData = NULL, miss = NULL, datafun = NULL, 
    outfun = NULL, pmMCAR = NULL, pmMAR = NULL, facDist = NULL, indDist = NULL, errorDist = NULL, 
    sequential = FALSE, modelBoot = FALSE, realData = NULL, covData = NULL, maxDraw = 50, misfitType = "f0", 
    misfitBounds = NULL, averageNumMisspec = FALSE, optMisfit = NULL, optDraws = 50, createOrder = c(1, 2, 3), 
    aux = NULL, seed = 123321, silent = FALSE, multicore = FALSE, cluster = FALSE, 
    numProc = NULL, paramOnly = FALSE, dataOnly = FALSE, smartStart = FALSE, ...) {
	#Future plans. Add summaryTime option. Or include as an option in summary. Guess time forfull sim
	#Update function. Takes results object. Or takes model object.
	#Speed things: Reference class, functions in c or c++ (maybe drawparam), look at sugar functions
	#Reference class for model object?
	#Difference percent missing in different groups
    start.time0 <- start.time <- proc.time()[3]
    timing <- list()
    require(parallel)
    RNGkind("L'Ecuyer-CMRG")
    
    ## 1. Set up correct data generation template (move inside the runRep).

    set.seed(seed)
	
	# Find the number of groups
	#Change in draw param so we always have a nested list (even with 1 group).
	#Then get rid of the if/else.
	ngroups <- 1
	if(!is.null(generate)) {
		ngroups <- max(generate@pt$group)
	} else {
		ngroups <- max(model@pt$group)
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
            simConds[[i]][[1]] <- NULL
            simConds[[i]][[2]] <- sapply(n, "[", i)
            simConds[[i]][[3]] <- pmMCAR[i]
            simConds[[i]][[4]] <- pmMAR[i]
            simConds[[i]][[5]] <- numseed[[i]]
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
        sys <- .Platform$OS.type
        if (is.null(numProc)) 
            numProc <- detectCores()
        if (sys == "windows") {
            cl <- makeCluster(rep("localhost", numProc), type = "SOCK")
            Result.l <- clusterApplyLB(cl, simConds, runRep, model = model, generate = generate, 
                miss = miss, datafun = datafun, outfun = outfun, silent = silent, 
                facDist = facDist, indDist = indDist, errorDist = errorDist, sequential = sequential, 
                realData = realData, covData = covData, maxDraw = maxDraw, misfitBounds = misfitBounds, 
                averageNumMisspec = averageNumMisspec, optMisfit = optMisfit, optDraws = optDraws, createOrder = createOrder,
                misfitType = misfitType, aux = aux, paramOnly = paramOnly, dataOnly = dataOnly, smartStart = smartStart, ...)
            stopCluster(cl)
        } else {
            Result.l <- mclapply(simConds, runRep, model = model, generate = generate, 
                miss = miss, datafun = datafun, outfun = outfun, silent = silent, 
                facDist = facDist, indDist = indDist, errorDist = errorDist, sequential = sequential, 
                realData = realData, covData = covData, maxDraw = maxDraw, misfitBounds = misfitBounds, 
                averageNumMisspec = averageNumMisspec, optMisfit = optMisfit, optDraws = optDraws, createOrder = createOrder, 
                misfitType = misfitType, aux = aux, mc.cores = numProc, paramOnly = paramOnly, dataOnly = dataOnly, smartStart = smartStart, ...)
        }
    } else {
        Result.l <- lapply(simConds, runRep, model = model, generate = generate, 
            miss = miss, datafun = datafun, outfun = outfun, silent = silent, facDist = facDist, 
            indDist = indDist, errorDist = errorDist, sequential = sequential, realData = realData, covData = covData, 
            maxDraw = maxDraw, misfitBounds = misfitBounds, averageNumMisspec = averageNumMisspec, 
            optMisfit = optMisfit, optDraws = optDraws,  createOrder = createOrder, misfitType = misfitType, 
            aux = aux, paramOnly = paramOnly, dataOnly = dataOnly, smartStart = smartStart, ...)
    }

	if(dataOnly) {
		## Return data when dataOnly is requested
		return(Result.l)
	} else {
	#Now we create a SimResult object
		timing.l <- lapply(Result.l, function(x) {
			x$timing
		})
		timing$InReps <- colSums(matrix(unlist(timing.l), nrow = nRep, byrow = TRUE))
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
		std.l <- lapply(Result.l, function(rep) {
			rep$std
		})
		extra <- list()
		if (!is.null(outfun)) {
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
		std <- as.data.frame(do.call(rbind, std.l))
		converged <- as.vector(unlist(converged.l))
		
		if(paramOnly) converged <- rep(TRUE, length(converged))
		param <- NULL
		FMI1 <- NULL
		FMI2 <- NULL
		popMis <- NULL
		misfitOut <- NULL
		if (!is.null(param.l[[1]])) {
			param <- as.data.frame(do.call(rbind, param.l))
			if (nrow(unique(param)) == 1) 
				param <- unique(param)
		}
		if (!is.null(FMI1.l[[1]])) {
			FMI1 <- as.data.frame(do.call(rbind, FMI1.l))
			if (sum(dim(FMI1)) == 0) 
				FMI1 <- data.frame()
			
		} else {
			FMI1 <- data.frame()
		}
		
		if (!is.null(FMI2.l[[1]])) {
			FMI2 <- as.data.frame(do.call(rbind, FMI2.l))
			if (sum(dim(FMI2)) == 0) 
				FMI2 <- data.frame()
			
		} else {
			FMI2 <- data.frame()
		}
		
		if (!is.null(popMis.l[[1]])) {
			popMis <- as.data.frame(do.call(rbind, popMis.l))
			if (nrow(unique(popMis)) == 1) 
				popMis <- unique(popMis)
		} else {
		 	popMis <- data.frame()
		}
		
		if (!is.null(misfitOut.l[[1]])) {
			misfitOut <- as.data.frame(do.call(rbind, misfitOut.l))
			if (nrow(unique(misfitOut)) == 1) 
				misfitOut <- unique(misfitOut)
		} else {
			misfitOut <- data.frame()
		}
		
		if (is.null(pmMCAR)) 
			ifelse(is.null(miss), pmMCAR <- 0, pmMCAR <- miss@pmMCAR)
		if (is.null(pmMAR)) 
			ifelse(is.null(miss), pmMAR <- 0, pmMAR <- miss@pmMAR)
		
		timing$CombineResults <- (proc.time()[3] - start.time)
		start.time <- proc.time()[3]
		
		nobs <- as.data.frame(n)
		n <- Reduce("+", n)
		colnames(nobs) <- 1:ngroups
		
		Result <- new("SimResult", modelType = model@modelType, nRep = nRep, coef = coef, 
			se = se, fit = fit, converged = converged, seed = seed, paramValue = param, 
			misspecValue = popMis, popFit = misfitOut, FMI1 = FMI1, FMI2 = FMI2, 
			stdCoef = std, n = n, nobs=nobs, pmMCAR = pmMCAR, pmMAR = pmMAR, extraOut = extra,
			paramOnly=paramOnly, timing = timing)
		if (silent) 
			options(warn = warnT)
		return <- Result
	}
}

# runRep: Run one replication

runRep <- function(simConds, model, generate = NULL, miss = NULL, datafun = NULL, 
    outfun = NULL, facDist = NULL, indDist = NULL, indLab = NULL, errorDist = NULL, 
    sequential = FALSE, realData = NULL, covData = NULL, silent = FALSE, modelBoot = FALSE, maxDraw = 50, 
    misfitType = "f0", misfitBounds = NULL, averageNumMisspec = NULL, optMisfit = NULL, 
    optDraws = 50, createOrder = c(1, 2, 3), aux = NULL, paramOnly = FALSE, dataOnly = FALSE, smartStart = TRUE, ...) {
    start.time0 <- start.time <- proc.time()[3]
    timing <- list()
    #Check why some are NULL and some are NA
	param <- NULL
    coef <- NA
    se <- NA
    fit <- NA
    std <- NA
    extra <- NA
    FMI1 <- NULL
    FMI2 <- NULL
	labelParam <- NULL
    converged <- 1
    n <- simConds[[2]]
    pmMCAR <- simConds[[3]]
    pmMAR <- simConds[[4]]
    dgen <- model@dgen
    RNGkind("L'Ecuyer-CMRG")
    assign(".Random.seed", simConds[[5]], envir = .GlobalEnv)
    specifiedGenerate <- TRUE
	
    if (is.null(generate)) {
        generate <- model
		specifiedGenerate <- FALSE
    }
	
	#Two things to think about: 1. Only do this when the generating and analysis models are different
	#2. Why do we do this for each rep? Move up to sim/model and pass values to runRep. Add 2 slots to SimSem class, put it there.
	#3. Also don't do this with rawData
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
    if (is.null(data)) {
		# Need to draw parameters
		genout <- generate(model = generate, n = n, maxDraw = maxDraw, misfitBounds = misfitBounds, 
			misfitType = misfitType, averageNumMisspec = averageNumMisspec, optMisfit = optMisfit, 
			optDraws = optDraws, createOrder = createOrder, indDist = indDist, sequential = sequential, 
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
    if (!is.null(miss)) {
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
	if(!paramOnly & !dataOnly) {
		# Impute missing and run results
		# Will use analyze either when there is a missing object or auxiliary variables specified. 
		# If users provide their own data there maybe a case with auxiliary variables and no missing object
		if (!is.null(miss) | !is.null(aux)) {
			if (silent) {
				invisible(capture.output(suppressMessages(try(out <- analyze(model, data, 
					aux = aux, miss = miss, ...), silent = TRUE))))
			} else {
				try(out <- analyze(model, data, aux = aux, miss = miss, ...))
			}
		} else {
			if (silent) {
				invisible(capture.output(suppressMessages(try(out <- anal(model, data, ...), 
					silent = TRUE))))
			} else {
				try(out <- anal(model, data, ...))
			}
		}
    }
    timing$Analyze <- (proc.time()[3] - start.time)
    start.time <- proc.time()[3]
    
    ## 6. Parse Lavaan Output
	if (!dataOnly) {
		if (!is.null(out)) {
			try(converged <- as.numeric(!inspect(out, "converged")))
			if(converged == 0) {
				se <- inspect(out, "se")
				improperSE <- any(unlist(se) < 0) | any(is.na(unlist(se)))
				if (improperSE) {
					converged <- 3
				}
				if (checkVar(out)) {
					converged <- 4
				} else if(checkCov(out)) {
					converged <- 5
				}
				#Below is only for multiple imputation. If the number of converged imputations is below the threshold then converged = 2
				if(is(out, "lavaanStar") && length(out@imputed) > 0) {
					if(out@imputed[[1]][1] < miss@convergentCutoff) converged <- 2
				}
			}
		}
		
		#reminder: out=lavaan object
		if (converged %in% c(0, 3:5)) {
		
			fit <- inspect(out, "fit") # Avoid fitMeasures function becuase the runMI function does not support the fitMeasures function.
			
			#redo with parameterEstimate function in lavaan (for coef se, std) all the way to 526
			result <- parameterEstimates(out, standardize=TRUE)
			extraParamIndex <- model@pt$op %in% c(">", "<", "==", ":=")
			index <- ((model@pt$free != 0) & !(duplicated(model@pt$free))) | extraParamIndex
			coef <- result$est[index]
			se <- result$se[index]
			std <- result$std.all[index]
			lab <- lavaan:::getParameterLabels(model@pt, type="free")
			if(any(extraParamIndex)) lab <- c(lab, renameExtraParam(model@con$lhs, model@con$op, model@con$rhs))
			names(coef) <- lab
			names(se) <- lab
			names(std) <- lab
			
			## 6.1. Call output function (if exists)
			if (!is.null(outfun)) {
				extra <- outfun(out)
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
			names(popParam) <- c(lavaan:::getParameterLabels(generate@pt, type="free"), extraParamName)
		} else {
			popParam <- NA  # Real Data
			popMis <- NA  # Misspecfication
			misfitOut <- NA  # Misfit indices for misspecification
		}
		

		if(!is.null(miss) && miss@m > 0) {
			if (converged %in% c(0, 3:5)) {
				fmiOut <- out@imputed[[2]]
				FMI1 <- fmiOut[,5]
				FMI2 <- fmiOut[,6]
				names(FMI1) <- paste0(fmiOut[,4], ".", fmiOut[,1], fmiOut[,2], fmiOut[,3]) 
				names(FMI2) <- names(FMI1) 
			} 	
		}
		#Finish FIML FMI below! from parameterEstimates
		if(!is.null(miss) && miss@m == 0){
		}
		timing$ParseOutput <- (proc.time()[3] - start.time)
		start.time <- proc.time()[3]
    	
		Result <- list(coef = coef, se = se, fit = fit, converged = converged, param = popParam, 
        FMI1 = FMI1, FMI2 = FMI2, std = std, timing = timing, extra = extra, popMis = popMis, 
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
	
    for (g in seq_along(misspecSet)) {
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
            lab <- outer(indLab, facLab[1:ncol(free)], function(x, y, op, g) paste0(g, ".", y, 
                op, x), op = "=~", g = g)
            param <- tpset$LY[free]
            names(param) <- lab[free]
            final <- c(final, param)
        }
        if (!is.null(tpset$PS)) {
            free <- tpset$PS != 0 & lower.tri(tpset$PS, diag = TRUE)
            lab <- outer(psLab[1:ncol(free)], psLab[1:ncol(free)], function(x, y, op, g) paste0(g, ".", x, op, 
                y), op = "~~", g = g)
            param <- tpset$PS[free]
            names(param) <- lab[free]
            final <- c(final, param)
        }
        if (!is.null(tpset$RPS)) {
            free <- tpset$RPS != 0 & lower.tri(tpset$RPS, diag = FALSE)
            lab <- outer(psLab[1:ncol(free)], psLab[1:ncol(free)], function(x, y, op, g) paste0(g, ".", x, op, 
                y), op = "~~*", g = g)
            param <- tpset$RPS[free]
            names(param) <- lab[free]
            final <- c(final, param)
        }
        if (!is.null(tpset$VPS)) {
            free <- tpset$VPS != 0
            lab <- paste0(g, ".", psLab[1:length(free)], "~~", psLab)
            param <- tpset$VPS[free]
            names(param) <- lab[free]
            final <- c(final, param)
        }
        if (!is.null(tpset$VE)) {
            free <- tpset$VE != 0
            lab <- paste0(g, ".", psLab[1:length(free)], "~~*", psLab)
            param <- tpset$VE[free]
            names(param) <- lab[free]
            final <- c(final, param)
        }
        if (!is.null(tpset$TE)) {
            free <- tpset$TE != 0 & lower.tri(tpset$TE, diag = TRUE)
            lab <- outer(indLab, indLab, function(x, y, op, g) paste0(g, ".", x, 
                op, y), op = "~~", g = g)
            param <- tpset$TE[free]
            names(param) <- lab[free]
            final <- c(final, param)
        }
        if (!is.null(tpset$RTE)) {
            free <- tpset$RTE != 0 & lower.tri(tpset$RTE, diag = FALSE)
            lab <- outer(indLab, indLab, function(x, y, op, g) paste0(g, ".", x, 
                op, y), op = "~~*", g = g)
            param <- tpset$RTE[free]
            names(param) <- lab[free]
            final <- c(final, param)
        }
        if (!is.null(tpset$VTE)) {
            free <- tpset$VTE != 0
            lab <- paste0(g, ".", indLab, "~~", indLab)
            param <- tpset$VTE[free]
            names(param) <- lab[free]
            final <- c(final, param)
        }
        if (!is.null(tpset$VY)) {
            free <- tpset$VY != 0
            lab <- paste0(g, ".", indLab, "~~*", indLab)
            param <- tpset$VY[free]
            names(param) <- lab[free]
            final <- c(final, param)
        }
        if (!is.null(tpset$BE)) {
            free <- tpset$BE != 0
            lab <- outer(psLab[1:ncol(free)], psLab[1:ncol(free)], function(x, y, op, g) paste0(g, ".", x, op, 
                y), op = "~", g = g)
            param <- tpset$BE[free]
            names(param) <- lab[free]
            final <- c(final, param)
        }
        if (!is.null(tpset$AL)) {
            free <- tpset$AL != 0
            lab <- paste0(g, ".", psLab[1:length(free)], "~1")
            param <- tpset$AL[free]
            names(param) <- lab[free]
            final <- c(final, param)
        }
        if (!is.null(tpset$ME)) {
            free <- tpset$ME != 0
            lab <- paste0(g, ".", psLab[1:length(free)], "~1*")
            param <- tpset$ME[free]
            names(param) <- lab[free]
            final <- c(final, param)
        }
        if (!is.null(tpset$TY)) {
            free <- tpset$TY != 0
            lab <- paste0(g, ".", indLab, "~1")
            param <- tpset$TY[free]
            names(param) <- lab[free]
            final <- c(final, param)
        }
        if (!is.null(tpset$MY)) {
            free <- tpset$MY != 0
            lab <- paste0(g, ".", indLab, "~1*")
            param <- tpset$MY[free]
            names(param) <- lab[free]
            final <- c(final, param)
        }
        if (!is.null(tpset$GA)) {
            free <- tpset$GA != 0
            lab <- outer(psLab[1:nrow(free)], covLab[1:ncol(free)], function(x, y, op, g) paste0(g, ".", x, op, 
                y), op = "~", g = g)
            param <- tpset$GA[free]
            names(param) <- lab[free]
            final <- c(final, param)
        }
        if (!is.null(tpset$KA)) {
            free <- tpset$KA != 0
            lab <- outer(indLab[1:nrow(free)], covLab[1:ncol(free)], function(x, y, op, g) paste0(g, ".", x, op, 
                y), op = "~", g = g)
            param <- tpset$KA[free]
            names(param) <- lab[free]
            final <- c(final, param)
        }
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
