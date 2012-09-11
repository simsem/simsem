
sim <- function(nRep, model, n, generate = NULL, rawData = NULL, miss = NULL, datafun = NULL, 
    outfun = NULL, pmMCAR = NULL, pmMAR = NULL, facDist = NULL, indDist = NULL, errorDist = NULL, 
    sequential = FALSE, modelBoot = FALSE, realData = NULL, maxDraw = 50, misfitType = "f0", 
    misfitBounds = NULL, averageNumMisspec = NULL, optMisfit = NULL, optDraws = 50, 
    aux = NULL, seed = 123321, silent = FALSE, multicore = FALSE, cluster = FALSE, 
    numProc = NULL, paramOnly = FALSE, dataOnly = FALSE, ...) {
    start.time0 <- start.time <- proc.time()[3]
    timing <- list()
    require(parallel)
    RNGkind("L'Ecuyer-CMRG")
    
    ## 1. Set up correct data generation template (move inside the runRep).
    
    ## Will be used for error checking
    
    ## 2. Compute combinations of simulation parameters (MAR, MCAR, n): complete
    ## factorial
    set.seed(seed)
    warnT <- as.numeric(options("warn"))
    if (silent) 
        options(warn = -1)
    if (is.null(nRep)) {
        if (!is.vector(n)) 
            stop("Please specify the number of replications")
        if (!is.null(pmMCAR) && !is.vector(pmMCAR)) 
            stop("Please specify the number of replications")
        if (!is.null(pmMAR) && !is.vector(pmMAR)) 
            stop("Please specify the number of replications")
        usedMCAR <- NULL
        usedMAR <- NULL
        ifelse(is.null(pmMCAR), usedMCAR <- 1, usedMCAR <- pmMCAR)
        ifelse(is.null(pmMAR), usedMAR <- 1, usedMAR <- pmMAR)
        out <- expand.grid(n, usedMCAR, usedMAR)
        n <- out[, 1]
        if (!is.null(pmMCAR)) 
            pmMCAR <- out[, 2]
        if (!is.null(pmMAR)) 
            pmMAR <- out[, 3]
        nRep <- nrow(out)
    } else if (is.null(rawData)) {
        # If there is no raw data, compute correct n for each condition
        if (length(n) != nRep) {
            if (length(n) == 1) {
                n <- rep(n, nRep)
            } else if (length(n) < nRep && (nRep%%length(n)) == 0) {
                n <- rep(n, each = (nRep/length(n)))
            } else {
                stop("n can only be of length 1, of length nRep, or a multiple of nRep")
            }
        }
    }
    
    timing$SimulationParams <- (proc.time()[3] - start.time0)
    start.time <- proc.time()[3]
    
    ## 3. Draws for randomly varying simulation parameters
    if (!is.null(n)) {
        if (is.vector(n)) {
            if (length(n) != nRep) 
                ifelse(length(n) > nRep, n <- sample(n, nRep, replace = TRUE), n <- sample(n, 
                  nRep))
        } else {
            stop("The n argument should be in a vector of numbers or distribution object only.")
        }
    }
    if (!is.null(pmMCAR)) {
        if (is.vector(pmMCAR)) {
            if (length(pmMCAR) != nRep) 
                ifelse(length(pmMCAR) > nRep, pmMCAR <- sample(pmMCAR, nRep, replace = TRUE), 
                  pmMCAR <- sample(pmMCAR, nRep))
        } else {
            stop("The pmMCAR argument should be in a vector of numbers or distribution object only.")
        }
    }
    if (!is.null(pmMAR)) {
        if (is.vector(pmMAR)) {
            if (length(pmMAR) != nRep) 
                ifelse(length(pmMAR) > nRep, pmMAR <- sample(pmMAR, nRep, replace = TRUE), 
                  pmMAR <- sample(pmMAR, nRep))
        } else {
            stop("The pmMAR argument should be in a vector of numbers or distribution object only.")
        }
    }
    
    timing$RandomSimParams <- (proc.time()[3] - start.time)
    start.time <- proc.time()[3]
    
    ## 4. Build list of simulation conditions. Each element of simConds is a
    ## replication.
    
    param <- NULL
    drawnParams <- list()
    simConds <- list()
    
    
    set.seed(seed)
    numseed <- list()
    s <- .Random.seed
    origSeed <- s
    for (i in 1:nRep) {
        numseed[[i]] <- s
        s <- nextRNGStream(s)
    }
    
    if (is.null(rawData)) {
        
        for (i in seq_len(nRep)) {
            simConds[[i]] <- list()
            simConds[[i]][[1]] <- NULL
            simConds[[i]][[2]] <- n[i]
            simConds[[i]][[3]] <- pmMCAR[i]
            simConds[[i]][[4]] <- pmMAR[i]
            simConds[[i]][[5]] <- numseed[[i]]
        }
        
    } else if (is.list(rawData)) {
        
        if (is.data.frame(rawData[[1]])) {
            if (!is.null(n) && ((n > nrow(rawData[[1]])) %in% TRUE)) 
                stop("The specified n is greater than the number of cases provided.")
        } else if (is.matrix(rawData[[1]])) {
            rawData <- lapply(rawData, data.frame)
            n <- lapply(rawData, nrow)  # Find n for rawData
            if (!is.null(n) && ((n > nrow(rawData[[1]])) %in% TRUE)) 
                stop("The specified n is greater than the number of cases provided.")
        } else {
            stop("The list in the rawData argument does not contain matrices or data frames.")
        }
        
        for (i in seq_along(rawData)) {
            simConds[[i]] <- list()
            simConds[[i]][[1]] <- rawData[[i]]
            simConds[[i]][[2]] <- n[i]
            simConds[[i]][[3]] <- pmMCAR[i]
            simConds[[i]][[4]] <- pmMAR[i]
            simConds[[i]][[5]] <- numseed[[i]]
        }
    } else {
        stop("The rawData argument is not a SimData class or a list of data frames.")
    }
    
    timing$SimConditions <- (proc.time()[3] - start.time)
    start.time <- proc.time()[3]
    
    
    ## 5. Run replications
    if (multicore) {
        library(parallel)
        sys <- .Platform$OS.type
        if (is.null(numProc)) 
            numProc <- detectCores()
        if (sys == "windows") {
            cl <- makeCluster(rep("localhost", numProc), type = "SOCK")
            Result.l <- clusterApplyLB(cl, simConds, runRep, model = model, generate = generate, 
                miss = miss, datafun = datafun, outfun = outfun, silent = silent, 
                facDist = facDist, indDist = indDist, errorDist = errorDist, sequential = sequential, 
                realData = realData, maxDraw = maxDraw, misfitBounds = misfitBounds, 
                averageNumMisspec = averageNumMisspec, optMisfit = optMisfit, optDraws = optDraws, 
                misfitType = misfitType, aux = aux)
            stopCluster(cl)
        } else {
            Result.l <- mclapply(simConds, runRep, model = model, generate = generate, 
                miss = miss, datafun = datafun, outfun = outfun, silent = silent, 
                facDist = facDist, indDist = indDist, errorDist = errorDist, sequential = sequential, 
                realData = realData, maxDraw = maxDraw, misfitBounds = misfitBounds, 
                averageNumMisspec = averageNumMisspec, optMisfit = optMisfit, optDraws = optDraws, 
                misfitType = misfitType, aux = aux, mc.cores = numProc)
        }
    } else {
        Result.l <- lapply(simConds, runRep, model = model, generate = generate, 
            miss = miss, datafun = datafun, outfun = outfun, silent = silent, facDist = facDist, 
            indDist = indDist, errorDist = errorDist, sequential = sequential, realData = realData, 
            maxDraw = maxDraw, misfitBounds = misfitBounds, averageNumMisspec = averageNumMisspec, 
            optMisfit = optMisfit, optDraws = optDraws, misfitType = misfitType, 
            aux = aux)
    }
    
    
    timing.l <- lapply(Result.l, function(x) {
        x$timing
    })
    repTimes <- colSums(matrix(unlist(timing.l), nrow = nRep, byrow = TRUE))
    names(repTimes) <- names(timing.l[[1]])
    timing$InReps <- repTimes
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
    param <- NULL
    FMI1 <- NULL
    FMI2 <- NULL
    popMis <- NULL
    misfitOut <- NULL
    if (!is.null(param.l[[1]])) {
        param <- as.data.frame(do.call(rbind, param.l))
        if (sum(dim(param)) == 0) 
            param <- NULL
        if (nrow(unique(param)) == 1) 
            param <- unique(param)
    }
    if (!is.null(FMI1.l[[1]])) {
        FMI1 <- as.data.frame(do.call(rbind, FMI1.l))
        if (sum(dim(FMI1)) == 0) 
            FMI1 <- NULL
        if (nrow(unique(FMI1)) == 1) 
            FMI1 <- unique(FMI1)
    } else {
		FMI1 <- data.frame()
	}
    if (!is.null(FMI2.l[[1]])) {
        FMI2 <- as.data.frame(do.call(rbind, FMI2.l))
        if (sum(dim(FMI2)) == 0) 
            FMI2 <- NULL
        if (nrow(unique(FMI2)) == 1) 
            FMI2 <- unique(FMI2)
    } else {
		FMI2 <- data.frame()
	}
    if (!is.null(popMis.l[[1]])) {
        popMis <- as.data.frame(do.call(rbind, popMis.l))
        if (sum(dim(popMis)) == 0) 
            popMis <- NULL
        if (nrow(unique(popMis)) == 1) 
            popMis <- unique(popMis)
    }
    if (all(dim(popMis) == 1)) 
        popMis <- data.frame()
    if (!is.null(misfitOut.l[[1]])) {
        misfitOut <- as.data.frame(do.call(rbind, misfitOut.l))
        if (sum(dim(misfitOut)) == 0) 
            misfitOut <- NULL
        if (nrow(unique(misfitOut)) == 1) 
            misfitOut <- unique(misfitOut)
    }
    if (all(dim(misfitOut) == 1)) 
        misfitOut <- data.frame()
    if (is.null(pmMCAR)) 
        ifelse(is.null(miss), pmMCAR <- 0, pmMCAR <- miss@pmMCAR)
    if (is.null(pmMAR)) 
        ifelse(is.null(miss), pmMAR <- 0, pmMAR <- miss@pmMAR)
    
    timing$CombineResults <- (proc.time()[3] - start.time)
    start.time <- proc.time()[3]
    
    Result <- new("SimResult", modelType = model@modelType, nRep = nRep, coef = coef, 
        se = se, fit = fit, converged = converged, seed = seed, paramValue = param, 
        misspecValue = popMis, popFit = misfitOut, FMI1 = FMI1, FMI2 = FMI2, 
        stdCoef = std, n = n, pmMCAR = pmMCAR, pmMAR = pmMAR, extraOut = extra, timing = timing)
    if (silent) 
        options(warn = warnT)
    return <- Result
}

# runRep: Run one replication

runRep <- function(simConds, model, generate = NULL, miss = NULL, datafun = NULL, 
    outfun = NULL, facDist = NULL, indDist = NULL, indLab = NULL, errorDist = NULL, 
    sequential = FALSE, realData = NULL, silent = FALSE, modelBoot = FALSE, maxDraw = 50, 
    misfitType = "f0", misfitBounds = NULL, averageNumMisspec = NULL, optMisfit = NULL, 
    optDraws = 50, timing = NULL, aux = NULL) {
    start.time0 <- start.time <- proc.time()[3]
    timing <- list()
    param <- NULL
    coef <- NA
    se <- NA
    fit <- NA
    std <- NA
    extra <- NA
    FMI1 <- NULL
    FMI2 <- NULL
    converged <- FALSE
    n <- simConds[[2]]
    pmMCAR <- simConds[[3]]
    pmMAR <- simConds[[4]]
    dgen <- model@dgen
    RNGkind("L'Ecuyer-CMRG")
    assign(".Random.seed", simConds[[5]], envir = .GlobalEnv)
    
    
    if (is.null(generate)) {
        generate <- model
    }
    
    ## 1. Create a missing data template from simulation parameters.
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
    if (is.null(data)) 
        {
            # Need to draw parameters
            genout <- generate(model = generate, n = n, maxDraw = maxDraw, misfitBounds = misfitBounds, 
                misfitType = misfitType, averageNumMisspec = averageNumMisspec, optMisfit = optMisfit, 
                optDraws = optDraws, indDist = indDist, sequential = sequential, 
                facDist = facDist, errorDist = errorDist, indLab = indLab, modelBoot = modelBoot, 
                realData = realData, params = TRUE)
            data <- genout[[1]]
            psl <- genout[[2]]  # Indexing: Group -> param/misParam/misOnly -> paramSet (reduced)
            if (!is.null(psl[[1]]$misspec)) {
                paramSet <- lapply(psl, "[[", 2)  # Group -> misParam -> paramSet (by group)
            } else {
                paramSet <- lapply(psl, "[[", 1)  # Group -> param -> paramSet (by group)
            }
        }  # else: do nothing. Raw data.
    timing$GenerateData <- (proc.time()[3] - start.time)
    start.time <- proc.time()[3]
    ## 3. Impose Missing (if any)
    if (!is.null(miss)) {
        
        data <- imposeMissing(data, cov = miss@cov, pmMCAR = pmMCAR, pmMAR = pmMAR, 
            nforms = miss@nforms, itemGroups = miss@itemGroups, twoMethod = miss@twoMethod, 
            prAttr = miss@prAttr, timePoints = miss@timePoints, logical = miss@logical, 
            ignoreCols = miss@ignoreCols, threshold = miss@threshold)
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
    # Impute missing and run results
    if (!is.null(miss)) {
        # Remove numImps out
        if (silent) {
            invisible(capture.output(suppressMessages(try(out <- analyze(model, data, 
                aux = aux, miss = miss), silent = TRUE))))
        } else {
            try(out <- analyze(model, data, aux = aux, miss = miss))
        }
    } else {
        if (silent) {
            invisible(capture.output(suppressMessages(try(out <- anal(model, data), 
                silent = TRUE))))
        } else {
            try(out <- anal(model, data))
        }
    }
    
    timing$Analyze <- (proc.time()[3] - start.time)
    start.time <- proc.time()[3]
    
    ## 6. Parse Lavaan Output
    if (!is.null(out)) {
        try(se <- inspect(out, "se"))
        try(converged <- inspect(out, "converged"))
        try(check <- sum(unlist(lapply(se, sum))))
        try(negVar <- checkVar(out))
        try(if (is.na(check) || check == 0 || negVar) {
            converged <- FALSE
        }, silent = TRUE)
		if(is(out, "lavaanStar") && length(out@imputed) > 0) {
			if(out@imputed[[1]][1] < miss@convergentCutoff) converged <- FALSE
		}
    }
    
    if (is.null(indLab)) {
        if (model@modelType == "Path") {
            indLab <- unique(model@pt$lhs)
        } else {
            indLab <- unique(model@pt$rhs[model@pt$op == "=~"])
        }
    }
    indLab <- setdiff(indLab, aux)
    facLab <- NULL
    if (model@modelType != "Path") {
        facLab <- unique(model@pt$lhs[model@pt$op == "=~"])
    }
    
    if (converged) {
        outLab <- out@Model@dimNames
        fit <- extractLavaanFit(out)
        coef <- reduceLavaanParam(inspect(out, "coef"), dgen, indLab, facLab)
        se <- reduceLavaanParam(se, dgen, indLab, facLab)
        std <- reduceLavaanParam(standardize(out), dgen, indLab, facLab)
        
        ## 6.1. Call output function (if exists)
        if (!is.null(outfun)) {
            extra <- outfun(out)
        }
    } else {
        fit <- NA
        coef <- NA
        se <- NA
        std <- NA
    }
    
    ## Keep parameters regardless of convergence - may want to examine
    ## non-convergent sets
    if (!is.null(paramSet)) {
        indLabGen <- NULL
        if (generate@modelType == "Path") {
            indLabGen <- unique(generate@pt$lhs)
        } else {
            indLabGen <- unique(generate@pt$rhs[generate@pt$op == "=~"])
        }
        facLabGen <- NULL
        if (generate@modelType != "Path") {
            facLabGen <- unique(generate@pt$lhs[generate@pt$op == "=~"])
        }
        popParam <- reduceParamSet(paramSet, generate@dgen, indLabGen, facLabGen, 
            aux)
        
        if (!is.null(psl[[1]]$misspec)) {
            # Group -> misParam -> paramSet (by group)
            misParamSet <- lapply(psl, "[[", 3)
            popMis <- reduceMisspecSet(misParamSet, generate@modelType != "Path", 
                indLabGen, facLabGen)
            p <- length(indLabGen)
            nElements <- (p + (p * (p + 1)/2)) * length(psl)
            dfParam <- nElements - max(generate@pt$free)
            misfitOut <- popMisfitParams(psl, df = dfParam)
        } else {
            popMis <- NA
            misfitOut <- NA
        }
    } else {
        popParam <- NA  # Real Data
        popMis <- NA  # Misspecfication
        misfitOut <- NA  # Misfit indices for misspecification
    }
    
	if(!is.null(miss) && miss@m > 0) {
		if (converged) {
			fmiOut <- out@imputed[[2]]
			FMI1 <- fmiOut[,5]
			FMI2 <- fmiOut[,6]
			names(FMI1) <- paste0(fmiOut[,4], ".", fmiOut[,1], fmiOut[,2], fmiOut[,3]) 
			names(FMI2) <- names(FMI1) 
		} else {
			FMI1 <- NA
			FMI2 <- NA
		}
	}
    # Need FMI1 and FMI2
    
    timing$ParseOutput <- (proc.time()[3] - start.time)
    start.time <- proc.time()[3]
    
    Result <- list(coef = coef, se = se, fit = fit, converged = converged, param = popParam, 
        FMI1 = FMI1, FMI2 = FMI2, std = std, timing = timing, extra = extra, popMis = popMis, 
        misfitOut = misfitOut)
    Result
}

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
reduceParamSet <- function(paramSet, dgen, indLab = NULL, facLab = NULL, aux = NULL) {
    
    if (!is.list(dgen[[1]])) {
        dgen <- list(dgen)
    }
    final <- NULL
    selectInd <- seq_along(indLab)
    if (!is.null(aux)) 
        selectInd <- match(setdiff(indLab, aux), indLab)
    selectFac <- seq_along(facLab)
    indLab <- setdiff(indLab, aux)
    
    for (g in seq_along(paramSet)) {
        tpset <- paramSet[[g]]
        tpgen <- dgen[[g]]
        
        if (!is.null(tpset$LY)) {
            free <- is.free(tpgen$LY@free)[selectInd, selectFac]
            lab <- outer(indLab, facLab, function(x, y, op, g) paste0(g, ".", y, 
                op, x), op = "=~", g = g)
            param <- tpset$LY[selectInd, selectFac][free]
            names(param) <- lab[free]
            final <- c(final, param)
        }
        if (!is.null(tpset$TE)) {
            
            if (!is.null(tpgen$TE)) {
                free <- is.free(tpgen$TE@free) & lower.tri(tpgen$TE@free, diag = TRUE)
            } else {
                free <- is.free(tpgen$RTE@free) & lower.tri(tpgen$RTE@free, diag = FALSE)
                if (!is.null(tpgen$VTE)) {
                  diag(free) <- is.free(tpgen$VTE@free)
                } else if (!is.null(tpgen$VY)) {
                  diag(free) <- is.free(tpgen$VY@free)
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
                free <- is.free(tpgen$PS@free) & lower.tri(tpgen$PS@free, diag = TRUE)
            } else {
                free <- is.free(tpgen$RPS@free) & lower.tri(tpgen$RPS@free, diag = FALSE)
                if (!is.null(tpgen$VPS)) {
                  diag(free) <- is.free(tpgen$VPS@free)
                } else if (!is.null(tpgen$VE)) {
                  diag(free) <- is.free(tpgen$VE@free)
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
                free <- is.free(tpgen$BE@free)[selectFac, selectFac]
                lab <- outer(facLab, facLab, function(x, y, op, g) paste0(g, ".", 
                  x, op, y), op = "~", g = g)
                param <- tpset$BE[selectFac, selectFac][free]
            } else {
                free <- is.free(tpgen$BE@free)[selectInd, selectInd]
                lab <- outer(indLab, indLab, function(x, y, op, g) paste0(g, ".", 
                  x, op, y), op = "~", g = g)
                param <- tpset$BE[selectInd, selectInd][free]
            }
            names(param) <- lab[free]
            final <- c(final, param)
        }
        if (!is.null(tpset$AL)) {
            if (!is.null(tpgen$AL)) {
                free <- is.free(tpgen$AL@free)
            } else if (!is.null(tpgen$ME)) {
                free <- is.free(tpgen$ME@free)
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
                free <- is.free(tpgen$TY@free)
            } else if (!is.null(tpgen$MY)) {
                free <- is.free(tpgen$MY@free)
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
            lab <- outer(indLab, facLab, function(x, y, op, g) paste0(g, ".", y, 
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
            lab <- paste0(g, ".", facLab, "~~", facLab)
            param <- tpset$VTE[free]
            names(param) <- lab[free]
            final <- c(final, param)
        }
        if (!is.null(tpset$VY)) {
            free <- tpset$VY != 0
            lab <- paste0(g, ".", facLab, "~~*", facLab)
            param <- tpset$VY[free]
            names(param) <- lab[free]
            final <- c(final, param)
        }
        if (!is.null(tpset$PS)) {
            free <- tpset$PS != 0 & lower.tri(tpset$PS, diag = TRUE)
            lab <- outer(psLab, psLab, function(x, y, op, g) paste0(g, ".", x, op, 
                y), op = "~~", g = g)
            param <- tpset$PS[free]
            names(param) <- lab[free]
            final <- c(final, param)
        }
        if (!is.null(tpset$RPS)) {
            free <- tpset$RPS != 0 & lower.tri(tpset$RPS, diag = FALSE)
            lab <- outer(psLab, psLab, function(x, y, op, g) paste0(g, ".", x, op, 
                y), op = "~~*", g = g)
            param <- tpset$RPS[free]
            names(param) <- lab[free]
            final <- c(final, param)
        }
        if (!is.null(tpset$VPS)) {
            free <- tpset$VPS != 0
            lab <- paste0(g, ".", psLab, "~~", psLab)
            param <- tpset$VPS[free]
            names(param) <- lab[free]
            final <- c(final, param)
        }
        if (!is.null(tpset$VE)) {
            free <- tpset$VE != 0
            lab <- paste0(g, ".", psLab, "~~*", psLab)
            param <- tpset$VE[free]
            names(param) <- lab[free]
            final <- c(final, param)
        }
        if (!is.null(tpset$BE)) {
            free <- tpset$BE != 0
            lab <- outer(psLab, psLab, function(x, y, op, g) paste0(g, ".", x, op, 
                y), op = "~", g = g)
            param <- tpset$BE[free]
            names(param) <- lab[free]
            final <- c(final, param)
        }
        if (!is.null(tpset$AL)) {
            free <- tpset$AL != 0
            lab <- paste0(g, ".", psLab, "~1")
            param <- tpset$AL[free]
            names(param) <- lab[free]
            final <- c(final, param)
        }
        if (!is.null(tpset$ME)) {
            free <- tpset$ME != 0
            lab <- paste0(g, ".", psLab, "~1*")
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
    
    if ("lambda" %in% names && !is.null(dgen[[1]]$LY)) {
        idx <- which(names == "lambda")
        for (i in seq_along(idx)) {
            free <- is.free(dgen[[i]]$LY@free)
            param <- glist[idx[i]]$lambda[indLab, facLab][free]
            lab <- outer(indLab, facLab, function(x, y, op, g) paste0(g, ".", y, 
                op, x), op = "=~", g = i)
            names(param) <- lab[free]
            final <- c(final, param)
        }
    }
    
    if ("theta" %in% names && (!is.null(dgen[[1]]$TE) || !is.null(dgen[[1]]$RTE))) {
        idx <- which(names == "theta")
        for (i in seq_along(idx)) {
            if (!is.null(dgen[[i]]$TE)) {
                free <- is.free(dgen[[i]]$TE@free) & lower.tri(dgen[[i]]$TE@free, 
                  diag = TRUE)
            } else {
                free <- is.free(dgen[[i]]$RTE@free) & lower.tri(dgen[[i]]$RTE@free, 
                  diag = FALSE)
                if (!is.null(dgen[[i]]$VTE)) {
                  diag(free) <- is.free(dgen[[i]]$VTE@free)
                } else if (!is.null(dgen[[i]]$VY)) {
                  diag(free) <- is.free(dgen[[i]]$VY@free)
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
                free <- is.free(dgen[[i]]$PS@free) & lower.tri(dgen[[i]]$PS@free, 
                  diag = TRUE)
            } else {
                free <- is.free(dgen[[i]]$RPS@free) & lower.tri(dgen[[i]]$RPS@free, 
                  diag = FALSE)
                if (!is.null(dgen[[i]]$VPS)) {
                  diag(free) <- is.free(dgen[[i]]$VPS@free)
                } else if (!is.null(dgen[[i]]$VE)) {
                  diag(free) <- is.free(dgen[[i]]$VE@free)
                }
            }
            if (!is.null(dgen[[i]]$LY)) {
                param <- glist[idx[i]]$psi[facLab, facLab][free]
                lab <- outer(facLab, facLab, function(x, y, op, g) paste0(g, ".", 
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
            free <- is.free(dgen[[i]]$BE@free)
            if (!is.null(dgen[[i]]$LY)) {
                param <- glist[idx[i]]$beta[facLab, facLab][free]
                lab <- outer(facLab, facLab, function(x, y, op, g) paste0(g, ".", 
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
                free <- is.free(dgen[[i]]$AL@free)
            } else {
                free <- is.free(dgen[[i]]$ME@free)
            }
            if (!is.null(dgen[[i]]$LY)) {
                param <- glist[idx[i]]$alpha[facLab, ][free]
                lab <- paste0(i, ".", facLab, "~1")
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
                free <- is.free(dgen[[i]]$TY@free)
            } else {
                free <- is.free(dgen[[i]]$MY@free)
            }
            param <- glist[idx[i]]$nu[indLab, ][free]
            lab <- paste0(i, ".", indLab, "~1")
            names(param) <- lab[free]
            final <- c(final, param)
        }
    }
    
    return(final)
}

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
