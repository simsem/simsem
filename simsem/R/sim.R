
sim <- function(nRep = NULL, model = NULL, n = NULL, generate = NULL, rawData = NULL, miss = NULL, datafun = NULL, 
    outfun = NULL, pmMCAR = NULL, pmMAR = NULL, facDist = NULL, indDist = NULL, errorDist = NULL, 
    sequential = FALSE, modelBoot = FALSE, realData = NULL, maxDraw = 50, misfitType = "f0", 
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
		ngroups <- max(generate$pt$group)
	} else {
		ngroups <- max(model$pt$group)
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
                realData = realData, maxDraw = maxDraw, misfitBounds = misfitBounds, 
                averageNumMisspec = averageNumMisspec, optMisfit = optMisfit, optDraws = optDraws, createOrder = createOrder,
                misfitType = misfitType, aux = aux, paramOnly = paramOnly, dataOnly = dataOnly, smartStart = smartStart, ...)
            stopCluster(cl)
        } else {
            Result.l <- mclapply(simConds, runRep, model = model, generate = generate, 
                miss = miss, datafun = datafun, outfun = outfun, silent = silent, 
                facDist = facDist, indDist = indDist, errorDist = errorDist, sequential = sequential, 
                realData = realData, maxDraw = maxDraw, misfitBounds = misfitBounds, 
                averageNumMisspec = averageNumMisspec, optMisfit = optMisfit, optDraws = optDraws, createOrder = createOrder, 
                misfitType = misfitType, aux = aux, mc.cores = numProc, paramOnly = paramOnly, dataOnly = dataOnly, smartStart = smartStart, ...)
        }
    } else {
        Result.l <- lapply(simConds, runRep, model = model, generate = generate, 
            miss = miss, datafun = datafun, outfun = outfun, silent = silent, facDist = facDist, 
            indDist = indDist, errorDist = errorDist, sequential = sequential, realData = realData, 
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
		
		name <- colnames(coef)
		#This determines which reps have names for parameters. Which indicates if the generating model has  
		#parameters with names (e.g. model constraints, or lavaan names). TRUE=has names
		haveName <- which(!sapply(Result.l, function(x) { if(is.null(x)) {return(TRUE)} else {return(is.null(x$labelParam))} }))
		lab <- NULL
		if(length(haveName) > 0) {
			lab <- Result.l[[haveName[1]]]$labelParam
			lab[is.na(lab)] <- ""
			names(lab) <- name
		} else {
			lab <- rep("", length(name))
			names(lab) <- name
		}
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
			ifelse(is.null(miss), pmMCAR <- 0, pmMCAR <- miss$pmMCAR)
		if (is.null(pmMAR)) 
			ifelse(is.null(miss), pmMAR <- 0, pmMAR <- miss$pmMAR)
		
		timing$CombineResults <- (proc.time()[3] - start.time)
		start.time <- proc.time()[3]
		
		nobs <- as.data.frame(n)
		n <- Reduce("+", n)
		colnames(nobs) <- 1:ngroups
		
		Result <- new("SimResult", modelType = model$modelType, nRep = nRep, coef = coef, 
			se = se, fit = fit, converged = converged, seed = seed, paramValue = param, 
			misspecValue = popMis, popFit = misfitOut, FMI1 = FMI1, FMI2 = FMI2, 
			stdCoef = std, n = n, nobs=nobs, pmMCAR = pmMCAR, pmMAR = pmMAR, extraOut = extra,
			paramOnly=paramOnly, labelParam = lab, timing = timing)
		if (silent) 
			options(warn = warnT)
		return <- Result
	}
}

# runRep: Run one replication

runRep <- function(simConds, model, generate = NULL, miss = NULL, datafun = NULL, 
    outfun = NULL, facDist = NULL, indDist = NULL, indLab = NULL, errorDist = NULL, 
    sequential = FALSE, realData = NULL, silent = FALSE, modelBoot = FALSE, maxDraw = 50, 
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
    dgen <- model$dgen
    RNGkind("L'Ecuyer-CMRG")
    assign(".Random.seed", simConds[[5]], envir = .GlobalEnv)
    
    if (is.null(generate)) {
        generate <- model
    }
	
	#Two things to think about: 1. Only do this when the generating and analysis models are different
	#2. Why do we do this for each rep? Move up to sim/model and pass values to runRep. Add 2 slots to SimSem class, put it there.
	#3. Also don't do this with rawData
    indLabGen <- NULL
	if (generate$modelType == "Path") {
		indLabGen <- unique(generate$pt$lhs)
	} else {
		indLabGen <- unique(generate$pt$rhs[generate$pt$op == "=~"])
	}
	facLabGen <- NULL
	if (generate$modelType != "Path") {
		facLabGen <- unique(generate$pt$lhs[generate$pt$op == "=~"])
	}
			
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
            pmMCAR <- miss$pmMCAR
        if (is.null(pmMAR)) 
            pmMAR <- miss$pmMAR
    }
    
    ## 2. Generate data (data) & store parameter values (paramSet)
    data <- simConds[[1]]  # either a paramSet or raw data
    if (is.null(data)) {
		# Need to draw parameters
		genout <- generate(model = generate, n = n, maxDraw = maxDraw, misfitBounds = misfitBounds, 
			misfitType = misfitType, averageNumMisspec = averageNumMisspec, optMisfit = optMisfit, 
			optDraws = optDraws, createOrder = createOrder, indDist = indDist, sequential = sequential, 
			facDist = facDist, errorDist = errorDist, indLab = indLab, modelBoot = modelBoot, 
			realData = realData, params = TRUE)
		data <- genout[[1]]
		psl <- genout[[2]]  
		
		# We need the real parameter values regardless of having model misspecification 
		# because the real parameter values are what we really need to infer
		paramSet <- lapply(psl, "[[", 1) 
		if(smartStart) {
		#Once indLab and facLab are in the model object, sub them in for indLabGen and facLabGen
			model <- imposeSmartStart(model, paramSet, indLabGen, facLabGen, latent=(generate$modelType != "Path"))
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
					if(out@imputed[[1]][1] < miss$convergentCutoff) converged <- 2
				}
			}
		}
		#Lines 490-5-1 can be replaced when labels are in SimSem object. Just call that slot.
		if (is.null(indLab)) {
			if (model$modelType == "Path") {
				indLab <- unique(model$pt$lhs)
			} else {
				indLab <- unique(model$pt$rhs[model$pt$op == "=~"])
			}
		}
		indLab <- setdiff(indLab, aux)
		facLab <- NULL
		if (model$modelType != "Path") {
			facLab <- unique(model$pt$lhs[model$pt$op == "=~"])
		}
		#reminder: out=lavaan object
		if (converged %in% c(0, 3:5)) {
			#use lavaan fitMeasures function
			fit <- extractLavaanFit(out)
			#redo with parameterEstimate function in lavaan (for coef se, std) all the way to 526
			coef <- reduceLavaanParam(inspect(out, "coef"), dgen, indLab, facLab)
			se <- reduceLavaanParam(se, dgen, indLab, facLab)
			std <- reduceLavaanParam(standardize(out), dgen, indLab, facLab)
			labelParam <- getLavaanLabels(inspect(out, "coef"), dgen, indLab, facLab)
			
			indexExtraParam <- out@ParTable$op %in% c(":=", ">", "<", "==")
			if(any(indexExtraParam)) {
				nameExtraParam <- renameExtraParam(out@ParTable$lhs[indexExtraParam], out@ParTable$op[indexExtraParam], out@ParTable$rhs[indexExtraParam])
				len <- length(nameExtraParam)
				lenout <- length(out@Fit@est)
				indexout <- (lenout-len+1):lenout
				extracoef <- out@Fit@est[indexout]
				extrase <- out@Fit@se[indexout]
				extrastd <- rep(NA, len)
				names(extracoef) <- names(extrase) <- names(extrastd) <- nameExtraParam
				coef <- c(coef, extracoef)
				se <- c(se, extrase)
				std <- c(std, extrastd)
				labelParam <- c(labelParam, nameExtraParam)
			}
			## 6.1. Call output function (if exists)
			if (!is.null(outfun)) {
				extra <- outfun(out)
			}
		} 
		
		## Keep parameters regardless of convergence - may want to examine
		## non-convergent sets
		if (!is.null(paramSet)) {
			
			popParam <- reduceParamSet(paramSet, generate$dgen, indLabGen, facLabGen, 
				aux)
			
			if (!is.null(psl[[1]]$misspec)) {
				misParamSet <- lapply(psl, "[[", 3)
				popMis <- reduceMisspecSet(misParamSet, generate$modelType != "Path", 
					indLabGen, facLabGen)
				p <- length(indLabGen)
				nElements <- (p + (p * (p + 1)/2)) * length(psl)
				dfParam <- nElements - max(generate$pt$free)
				misfitOut <- popMisfitParams(psl, df = dfParam)
			} else {
				popMis <- NA
				misfitOut <- NA
			}
			
			if(!is.null(generate$con[[1]])) {
				extraparam <- collapseExtraParam(paramSet, generate$dgen, fill=TRUE, con=generate$con)
				names(extraparam) <- renameExtraParam(generate$con$lhs, generate$con$op, generate$con$rhs)
				popParam <- c(popParam, extraparam)
			}
		} else {
			popParam <- NA  # Real Data
			popMis <- NA  # Misspecfication
			misfitOut <- NA  # Misfit indices for misspecification
		}
		

		if(!is.null(miss) && miss$m > 0) {
			if (converged %in% c(0, 3:5)) {
				fmiOut <- out@imputed[[2]]
				FMI1 <- fmiOut[,5]
				FMI2 <- fmiOut[,6]
				names(FMI1) <- paste0(fmiOut[,4], ".", fmiOut[,1], fmiOut[,2], fmiOut[,3]) 
				names(FMI2) <- names(FMI1) 
			} 	
		}
		#Finish FIML FMI below! from parameterEstimates
		if(!is.null(miss) && miss$m == 0){
		}
		timing$ParseOutput <- (proc.time()[3] - start.time)
		start.time <- proc.time()[3]
    	
		Result <- list(coef = coef, se = se, fit = fit, converged = converged, param = popParam, 
        FMI1 = FMI1, FMI2 = FMI2, std = std, timing = timing, extra = extra, popMis = popMis, 
        misfitOut = misfitOut, labelParam = labelParam)
		return(Result)
	} else {
		return(data)
	}
}

####Get rid of functions below that are no longer used...


# \title{
	# Extract fit indices from the lavaan object
# }
# \description{
	# Extract fit indices from the \code{lavaan} object
# }
# \usage{
# extractLavaanFit(Output)
# }
# \arguments{
  # \item{Output}{
	# The \code{lavaan} object
# }
# }
# \value{
	# The renamed vector of fit measures
# }

extractLavaanFit <- function(Output) {
    Indices <- inspect(Output, "fit") # inspect function is needed to get the null model stats for lavaanStar*
    result <- c(Indices["chisq"], Indices["df"], Indices["pvalue"], Indices["baseline.chisq"], 
        Indices["baseline.df"], Indices["baseline.pvalue"], Indices["cfi"], Indices["tli"], 
        Indices["aic"], Indices["bic"], Indices["rmsea"], Indices["rmsea.ci.lower"], 
        Indices["rmsea.ci.upper"], Indices["srmr"])
    old.name <- c("chisq", "cfi", "tli", "aic", "bic", "rmsea", "srmr")
    new.name <- c("Chi", "CFI", "TLI", "AIC", "BIC", "RMSEA", "SRMR")
    name <- names(result)
    for (i in 1:length(old.name)) {
        name <- gsub(old.name[i], new.name[i], name)
    }
    names(result) <- name
    result
}

## paramSet -> Re-labeled population parameter values (for free parameters)
## Rework reduceParamSet so that run once to get location of free parameters, then use that in runRep
## Maybe use smartStart instead of resduceParamSet? 
## What about a check for random parameters if it is false, then only do it once in sim
reduceParamSet <- function(paramSet, dgen, indLab = NULL, facLab = NULL, aux = NULL) {
    
    if (!is.list(dgen[[1]])) {
        dgen <- list(dgen)
    }
    final <- NULL
    selectInd <- seq_along(indLab)
    if (!is.null(aux)) 
        selectInd <- match(setdiff(indLab, aux), indLab)
    indLab <- setdiff(indLab, aux)
	savedFacLab <- facLab
    
    for (g in seq_along(paramSet)) {
        tpset <- paramSet[[g]]
        tpgen <- dgen[[g]]
		facLab <- savedFacLab

        if (!is.null(tpset$LY)) {
			facLab <- facLab[1:ncol(tpgen$LY$free)]
			selectFac <- seq_along(facLab)
		
            free <- is.free(tpgen$LY$free)[selectInd, selectFac]
            lab <- outer(indLab, facLab, function(x, y, op, g) paste0(g, ".", y, 
                op, x), op = "=~", g = g)
            param <- tpset$LY[selectInd, selectFac][free]
            names(param) <- lab[free]
            final <- c(final, param)
        }
        if (!is.null(tpset$TE)) {
            
            if (!is.null(tpgen$TE)) {
                free <- is.free(tpgen$TE$free) & lower.tri(tpgen$TE$free, diag = TRUE)
            } else {
                free <- is.free(tpgen$RTE$free) & lower.tri(tpgen$RTE$free, diag = FALSE)
                if (!is.null(tpgen$VTE)) {
                  diag(free) <- is.free(tpgen$VTE$free)
                } else if (!is.null(tpgen$VY)) {
                  diag(free) <- is.free(tpgen$VY$free)
                }
            }
            free <- free[selectInd, selectInd]
            lab <- outer(indLab, indLab, function(x, y, op, g) paste0(g, ".", x, 
                op, y), op = "~~", g = g)
            param <- tpset$TE[selectInd, selectInd][free]
            names(param) <- lab[free]
            final <- c(final, param)
        }
        if (!is.null(tpset$PS)) {
            if (!is.null(tpgen$PS)) {
                free <- is.free(tpgen$PS$free) & lower.tri(tpgen$PS$free, diag = TRUE)
            } else {
                free <- is.free(tpgen$RPS$free) & lower.tri(tpgen$RPS$free, diag = FALSE)
                if (!is.null(tpgen$VPS)) {
                  diag(free) <- is.free(tpgen$VPS$free)
                } else if (!is.null(tpgen$VE)) {
                  diag(free) <- is.free(tpgen$VE$free)
                }
            }
            if (!is.null(tpset$LY)) {
                free <- free[selectFac, selectFac]
                lab <- outer(facLab, facLab, function(x, y, op, g) paste0(g, ".", 
                  x, op, y), op = "~~", g = g)
                param <- tpset$PS[selectFac, selectFac][free]
            } else {
                free <- free[selectInd, selectInd]
                lab <- outer(indLab, indLab, function(x, y, op, g) paste0(g, ".", 
                  x, op, y), op = "~~", g = g)
                param <- tpset$PS[selectInd, selectInd][free]
            }
            names(param) <- lab[free]
            final <- c(final, param)
        }
        if (!is.null(tpset$BE)) {
            if (!is.null(tpset$LY)) {
                free <- is.free(tpgen$BE$free)[selectFac, selectFac]
                lab <- outer(facLab, facLab, function(x, y, op, g) paste0(g, ".", 
                  x, op, y), op = "~", g = g)
                param <- tpset$BE[selectFac, selectFac][free]
            } else {
                free <- is.free(tpgen$BE$free)[selectInd, selectInd]
                lab <- outer(indLab, indLab, function(x, y, op, g) paste0(g, ".", 
                  x, op, y), op = "~", g = g)
                param <- tpset$BE[selectInd, selectInd][free]
            }
            names(param) <- lab[free]
            final <- c(final, param)
        }
        if (!is.null(tpset$AL)) {
            if (!is.null(tpgen$AL)) {
                free <- is.free(tpgen$AL$free)
            } else if (!is.null(tpgen$ME)) {
                free <- is.free(tpgen$ME$free)
            } else {
                free <- rep(FALSE, length(tpset$AL))
            }
            if (!is.null(tpset$LY)) {
                free <- free[selectFac]
                lab <- paste0(g, ".", facLab, "~1")
                param <- tpset$AL[selectFac][free]
            } else {
                free <- free[selectInd]
                lab <- paste0(g, ".", indLab, "~1")
                param <- tpset$AL[selectInd][free]
            }
            names(param) <- lab[free]
            final <- c(final, param)
        }
        if (!is.null(tpset$TY)) {
            if (!is.null(tpgen$TY)) {
                free <- is.free(tpgen$TY$free)
            } else if (!is.null(tpgen$MY)) {
                free <- is.free(tpgen$MY$free)
            } else {
                free <- rep(FALSE, length(tpset$TY))
            }
            free <- free[selectInd]
            lab <- paste0(g, ".", indLab, "~1")
            param <- tpset$TY[selectInd][free]
            names(param) <- lab[free]
            final <- c(final, param)
        }
        
        
    }
    final
}

## paramSet -> Re-labeled population parameter values (for free parameters)
##MispecSet is still needed but paramSet above can be replaced
reduceMisspecSet <- function(misspecSet, latent, indLab = NULL, facLab = NULL) {
    
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
    }
    final
}

## GLIST -> Re-labeled Parameter Estimates
reduceLavaanParam <- function(glist, dgen, indLab, facLab) {
    # Chunk at a time approach
    names <- names(glist)
    final <- NULL
    if (!is.list(dgen[[1]])) {
        dgen <- list(dgen)
    }
	ngroups <- length(dgen)
	if("lambda" %in% names && !is.null(dgen[[1]]$LY)) {
		nfac <- lapply(lapply(dgen, "[[", "LY"), function(x) ncol(x$free))
		facLab <- lapply(nfac, function(num, lab) lab[1:num], lab=facLab)
	} else {
		facLab <- list(facLab)
	}
	
    if ("lambda" %in% names && !is.null(dgen[[1]]$LY)) {
        idx <- which(names == "lambda")
        for (i in seq_along(idx)) {
            free <- is.free(dgen[[i]]$LY$free)
            param <- glist[idx[i]]$lambda[indLab, facLab[[i]]][free]
            lab <- outer(indLab, facLab[[i]], function(x, y, op, g) paste0(g, ".", y, 
                op, x), op = "=~", g = i)
            names(param) <- lab[free]
            final <- c(final, param)
        }
    }
    
    if ("theta" %in% names && (!is.null(dgen[[1]]$TE) || !is.null(dgen[[1]]$RTE))) {
        idx <- which(names == "theta")
        for (i in seq_along(idx)) {
            if (!is.null(dgen[[i]]$TE)) {
                free <- is.free(dgen[[i]]$TE$free) & lower.tri(dgen[[i]]$TE$free, 
                  diag = TRUE)
            } else {
                free <- is.free(dgen[[i]]$RTE$free) & lower.tri(dgen[[i]]$RTE$free, 
                  diag = FALSE)
                if (!is.null(dgen[[i]]$VTE)) {
                  diag(free) <- is.free(dgen[[i]]$VTE$free)
                } else if (!is.null(dgen[[i]]$VY)) {
                  diag(free) <- is.free(dgen[[i]]$VY$free)
                }
            }
            param <- glist[idx[i]]$theta[indLab, indLab][free]
            lab <- outer(indLab, indLab, function(x, y, op, g) paste0(g, ".", x, 
                op, y), op = "~~", g = i)
            names(param) <- lab[free]
            final <- c(final, param)
        }
    }
    if ("psi" %in% names) {
        idx <- which(names == "psi")
        for (i in seq_along(idx)) {
            if (!is.null(dgen[[i]]$PS)) {
                free <- is.free(dgen[[i]]$PS$free) & lower.tri(dgen[[i]]$PS$free, 
                  diag = TRUE)
            } else {
                free <- is.free(dgen[[i]]$RPS$free) & lower.tri(dgen[[i]]$RPS$free, 
                  diag = FALSE)
                if (!is.null(dgen[[i]]$VPS)) {
                  diag(free) <- is.free(dgen[[i]]$VPS$free)
                } else if (!is.null(dgen[[i]]$VE)) {
                  diag(free) <- is.free(dgen[[i]]$VE$free)
                }
            }
            if (!is.null(dgen[[i]]$LY)) {
                param <- glist[idx[i]]$psi[facLab[[i]], facLab[[i]]][free]
                lab <- outer(facLab[[i]], facLab[[i]], function(x, y, op, g) paste0(g, ".", 
                  x, op, y), op = "~~", g = i)
            } else {
                param <- glist[idx[i]]$psi[indLab, indLab][free]
                lab <- outer(indLab, indLab, function(x, y, op, g) paste0(g, ".", 
                  x, op, y), op = "~~", g = i)
            }
            names(param) <- lab[free]
            final <- c(final, param)
        }
    }
    
    if ("beta" %in% names) {
        idx <- which(names == "beta")
        for (i in seq_along(idx)) {
            free <- is.free(dgen[[i]]$BE$free)
            if (!is.null(dgen[[i]]$LY)) {
                param <- glist[idx[i]]$beta[facLab[[i]], facLab[[i]]][free]
                lab <- outer(facLab[[i]], facLab[[i]], function(x, y, op, g) paste0(g, ".", 
                  x, op, y), op = "~", g = i)
            } else {
                param <- glist[idx[i]]$beta[indLab, indLab][free]
                lab <- outer(indLab, indLab, function(x, y, op, g) paste0(g, ".", 
                  x, op, y), op = "~", g = i)
            }
            names(param) <- lab[free]
            final <- c(final, param)
        }
    }
    
    if ("alpha" %in% names && (!is.null(dgen[[i]]$AL) || !is.null(dgen[[i]]$ME))) {
        idx <- which(names == "alpha")
        for (i in seq_along(idx)) {
            if (!is.null(dgen[[i]]$AL)) {
                free <- is.free(dgen[[i]]$AL$free)
            } else {
                free <- is.free(dgen[[i]]$ME$free)
            }
            if (!is.null(dgen[[i]]$LY)) {
                param <- glist[idx[i]]$alpha[facLab[[i]], ][free]
                lab <- paste0(i, ".", facLab[[i]], "~1")
            } else {
                param <- glist[idx[i]]$alpha[indLab, ][free]
                lab <- paste0(i, ".", indLab, "~1")
            }
            names(param) <- lab[free]
            final <- c(final, param)
        }
    }
    
    if ("nu" %in% names && (!is.null(dgen[[i]]$TY) || !is.null(dgen[[i]]$MY))) {
        idx <- which(names == "nu")
        for (i in seq_along(idx)) {
            if (!is.null(dgen[[i]]$TY)) {
                free <- is.free(dgen[[i]]$TY$free)
            } else {
                free <- is.free(dgen[[i]]$MY$free)
            }
            param <- glist[idx[i]]$nu[indLab, ][free]
            lab <- paste0(i, ".", indLab, "~1")
            names(param) <- lab[free]
            final <- c(final, param)
        }
    }
    
    return(final)
}

## GLIST -> Re-labeled Parameter Estimates
getLavaanLabels <- function(glist, dgen, indLab, facLab) {
    # Chunk at a time approach
    names <- names(glist)
	label <- NULL
    if (!is.list(dgen[[1]])) {
        dgen <- list(dgen)
    }
    if ("lambda" %in% names && !is.null(dgen[[1]]$LY)) {
        idx <- which(names == "lambda")
        for (i in seq_along(idx)) {
            free <- is.free(dgen[[i]]$LY$free)
			label <- c(label, dgen[[i]]$LY$free[free])
        }
    }
    
    if ("theta" %in% names && (!is.null(dgen[[1]]$TE) || !is.null(dgen[[1]]$RTE))) {
        idx <- which(names == "theta")
        for (i in seq_along(idx)) {
			targetLab <- NULL
            if (!is.null(dgen[[i]]$TE)) {
                free <- is.free(dgen[[i]]$TE$free) & lower.tri(dgen[[i]]$TE$free, 
                  diag = TRUE)
				targetLab <- dgen[[i]]$TE$free
            } else {
                free <- is.free(dgen[[i]]$RTE$free) & lower.tri(dgen[[i]]$RTE$free, 
                  diag = FALSE)
				targetLab <- dgen[[i]]$RTE$free
                if (!is.null(dgen[[i]]$VTE)) {
                  diag(free) <- is.free(dgen[[i]]$VTE$free)
				  diag(targetLab) <- dgen[[i]]$VTE$free
                } else if (!is.null(dgen[[i]]$VY)) {
                  diag(free) <- is.free(dgen[[i]]$VY$free)
				  diag(targetLab) <- dgen[[i]]$VY$free
                }
            }
			label <- c(label, targetLab[free])
        }
    }
    if ("psi" %in% names) {
        idx <- which(names == "psi")
        for (i in seq_along(idx)) {
			targetLab <- NULL
            if (!is.null(dgen[[i]]$PS)) {
                free <- is.free(dgen[[i]]$PS$free) & lower.tri(dgen[[i]]$PS$free, 
                  diag = TRUE)
				targetLab <- dgen[[i]]$PS$free
            } else {
                free <- is.free(dgen[[i]]$RPS$free) & lower.tri(dgen[[i]]$RPS$free, 
                  diag = FALSE)
				targetLab <- dgen[[i]]$RPS$free
                if (!is.null(dgen[[i]]$VPS)) {
                  diag(free) <- is.free(dgen[[i]]$VPS$free)
				  diag(targetLab) <- dgen[[i]]$VPS$free
                } else if (!is.null(dgen[[i]]$VE)) {
                  diag(free) <- is.free(dgen[[i]]$VE$free)
				  diag(targetLab) <- dgen[[i]]$VE$free
                }
            }
            label <- c(label, targetLab[free])
        }
    }
    
    if ("beta" %in% names) {
        idx <- which(names == "beta")
        for (i in seq_along(idx)) {
            free <- is.free(dgen[[i]]$BE$free)
            label <- c(label, dgen[[i]]$BE$free[free])
        }
    }
    
    if ("alpha" %in% names && (!is.null(dgen[[i]]$AL) || !is.null(dgen[[i]]$ME))) {
        idx <- which(names == "alpha")
        for (i in seq_along(idx)) {
            if (!is.null(dgen[[i]]$AL)) {
                free <- is.free(dgen[[i]]$AL$free)
				label <- c(label, dgen[[i]]$AL$free[free])
            } else {
                free <- is.free(dgen[[i]]$ME$free)
				label <- c(label, dgen[[i]]$ME$free[free])
            }
        }
    }
    
    if ("nu" %in% names && (!is.null(dgen[[i]]$TY) || !is.null(dgen[[i]]$MY))) {
        idx <- which(names == "nu")
        for (i in seq_along(idx)) {
            if (!is.null(dgen[[i]]$TY)) {
                free <- is.free(dgen[[i]]$TY$free)
				label <- c(label, dgen[[i]]$TY$free[free])
            } else {
                free <- is.free(dgen[[i]]$MY$free)
				label <- c(label, dgen[[i]]$MY$free[free])
            }
        }
    }
    
    return(label)
}

##Check to see if random parameters are in the model. Can use to simplfy runRep
is.random <- function(dat) {
    dat[is.empty(dat)] <- "0"
    isRandom <- sapply(dat, FUN = function(x) {
        x <- suppressWarnings(is.na(as.numeric(x)))
    })
    return(isRandom)
}

## Lavaan -> GLIST (std)

## taken shamelessly from param.value in lavaan.
standardize <- function(object) {
    
    GLIST <- object@Model@GLIST
    est.std <- standardizedSolution(object)$est.std
    
    for (mm in 1:length(GLIST)) {
        ## labels
        dimnames(GLIST[[mm]]) <- object@Model@dimNames[[mm]]
        
        ## fill in starting values
        m.user.idx <- object@Model@m.user.idx[[mm]]
        x.user.idx <- object@Model@x.user.idx[[mm]]
        GLIST[[mm]][m.user.idx] <- est.std[x.user.idx]
        
        ## class
        class(GLIST[[mm]]) <- c("matrix")
        
        
    }
    GLIST
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

imposeSmartStart <- function(model, paramSet, indLab, facLab, latent) {
	pt <- as.data.frame(model$pt)
	#Change to return on single vector for param, then add onto pt
	param <- mapply(collapseParamSet, paramSet, unique(pt$group), MoreArgs=list(indLab=indLab, facLab=facLab, latent=latent), SIMPLIFY=FALSE)
	param <- do.call(rbind, param)
	temp <- merge(pt, param, by=c("group", "op", "lhs", "rhs"))
	temp2 <- c(temp$ustart2, pt$ustart[setdiff(pt$id, temp$id)])
	ordertemp <- order(c(temp$id, setdiff(pt$id, temp$id)))
	model$pt$ustart <- temp2[ordertemp]
	model
}

#Instead of creating a new parameter table in collapseParamSet... 
#Just make smartStart order the same at pt from model command and add a column of starting values...
collapseParamSet <- function(param, group, indLab, facLab, latent) {
	op <- NULL
	lhs <- NULL
	rhs <- NULL
	ustart <- NULL
	psLab <- NULL
    if (latent) {
        psLab <- facLab
    } else {
        psLab <- indLab
    }
	if(!is.null(param$LY)) {
		lhs <- c(lhs, facLab[as.vector(col(param$LY))])
		rhs <- c(rhs, indLab[as.vector(row(param$LY))])
		op <- c(op, rep("=~", prod(dim(param$LY))))
		ustart <- c(ustart, as.vector(param$LY))
	}
	if(!is.null(param$PS)) {
		lhs <- c(lhs, psLab[col(param$PS)[lower.tri(param$PS, diag = TRUE)]])
		rhs <- c(rhs, psLab[row(param$PS)[lower.tri(param$PS, diag = TRUE)]])
		op <- c(op, rep("~~", sum(lower.tri(param$PS, diag = TRUE))))
		ustart <- c(ustart, param$PS[lower.tri(param$PS, diag = TRUE)])
	}
	if(!is.null(param$TE)) {
		lhs <- c(lhs, indLab[col(param$TE)[lower.tri(param$TE, diag = TRUE)]])
		rhs <- c(rhs, indLab[row(param$TE)[lower.tri(param$TE, diag = TRUE)]])
		op <- c(op, rep("~~", sum(lower.tri(param$TE, diag = TRUE))))
		ustart <- c(ustart, param$TE[lower.tri(param$TE, diag = TRUE)])
	}
	if(!is.null(param$BE)) {
		lhs <- c(lhs, psLab[as.vector(row(param$BE))])
		rhs <- c(rhs, psLab[as.vector(col(param$BE))])
		op <- c(op, rep("~", prod(dim(param$BE))))
		ustart <- c(ustart, as.vector(param$BE))
	}
	if(!is.null(param$AL)) {
		lhs <- c(lhs, psLab)
		rhs <- c(rhs, rep("", length(psLab)))
		op <- c(op, rep("~1", length(psLab)))
		ustart <- c(ustart, param$AL)
	}
	if(!is.null(param$TY)) {
		lhs <- c(lhs, indLab)
		rhs <- c(rhs, rep("", length(indLab)))
		op <- c(op, rep("~1", length(indLab)))
		ustart <- c(ustart, param$TY)
	}
	group <- rep(group, length(lhs))
	data.frame(lhs=lhs, op=op, rhs=rhs, group=group, ustart2=ustart)
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
