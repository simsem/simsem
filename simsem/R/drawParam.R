## Arguments: misfitBound should be a vector with upper and lower bounds

## Takes a simsem template, and returns population parameter values for data
## generation.  The format is a list with the following elements: [[1]] $param
## - Population parameter values for data generation [[2]] $paramMis -
## Population parameter values with misspecification [[3]] $mis -
## Misspecification only


draw <- function(model, maxDraw = 50, misfitBounds = NULL, averageNumMisspec = FALSE, 
    optMisfit = NULL, optDraws = 50, misfitType = "f0", createOrder = c(1, 2, 3), covData = NULL) {
    stopifnot(class(model) == "SimSem")
	covLab <- unique(model@pt$lhs[model@pt$op == "~1" & model@pt$exo == 1])
	
	if(length(covLab) > 0) {
		if(is.null(covData)) stop("The covariate data must be specified.")
		groupseq <- unique(model@pt$group[model@pt$group > 0])
		if(length(groupseq) > 1) covLab <- c(covLab, model@groupLab)
		covData <- covData[,covLab, drop=FALSE]		
		if(ncol(covData) != length(covLab)) stop(paste0("The covariate data must contain the following variable names: ", paste(covLab, collapse = ", ")))
		if(any(is.na(covData))) stop("The covariate data must not have missing variables.")
	} else {
		if(!is.null(covData)) {
			warnings("CONFLICT: The model template does not have any covariates but the covaraite data are specified. The covaraite data are ignored.")
			covData <- NULL
		}
	}
	
    drawParam(model@dgen, maxDraw = maxDraw, numFree = max(model@pt$free), misfitBounds = misfitBounds, 
        averageNumMisspec = averageNumMisspec, optMisfit = optMisfit, optDraws = optDraws, 
        misfitType = misfitType, con = model@con, ord = createOrder, covData = covData)
}

# Order 1 = constraint, 2 = misspec, 3 = filling parameters
drawParam <- function(paramSet, maxDraw = 50, numFree = 1, misfitBounds = NULL, averageNumMisspec = FALSE, 
    optMisfit = NULL, optDraws = 50, misfitType = "f0", con = NULL, ord = c(1, 2, 3), covData = NULL) {
    if (!is.list(paramSet[[1]])) {
        paramSet <- list(paramSet)
    }
	ngroups <- length(paramSet)
    groupLoop <- seq_len(ngroups)
	covStat <- rep(list(NULL), ngroups)
	if (!is.null(covData)) {
		if(ngroups == 1) {
			covStat[[1]] <- list(MZ = as.matrix(colMeans(covData)), CZ = cov(covData))
		} else {
			groupCov <- covData[,ncol(covData),drop=FALSE]
			targetDat <- covData[,-ncol(covData),drop=FALSE]
			for(i in groupLoop) {
				covStat[[i]] <- list(MZ = as.matrix(colMeans(targetDat[groupCov == i,,drop=FALSE])), CZ = cov(targetDat[groupCov == i,,drop=FALSE]))
			}
		}
	}
    misspec <- any(sapply(paramSet, FUN = function(group) {
        sapply(group, FUN = function(mat) {
            if (!is.null(mat) && length(mat@misspec) != 0 && !any(is.nan(mat@misspec))) {
                TRUE
            } else {
                FALSE
            }
        })
    }))
    misfitType <- tolower(misfitType)
    stopifnot(misfitType == "f0" || misfitType == "rmsea" || misfitType == "srmr" || 
        misfitType == "all")
    
    draw <- 1
    valid <- FALSE
    while (draw <= maxDraw) {
        if (misspec) {
            
            rpls <- list()  # list of drawn parameter sets (raw)
            fullpls <- list()  # list of drawn parameter sets (filled)
            fullmpls <- list()  # list of drawn parameter sets with misspec (filled)
            fullmls <- list()  # list of parameter sets with misspec only (filled)
            for (i in groupLoop) {
                rpls[[i]] <- lapply(paramSet[[i]], rawDraw, constraint = FALSE)
				fullpls[[i]] <- lapply(rpls[[i]], "[[", 1)
                fullmls[[i]] <- lapply(rpls[[i]], "[[", 3)
            }
			temp <- misspecOrder(real=fullpls, misDraw=fullmls, paramSet=paramSet, con=con, ord=ord, covStat=covStat)
			fullpls <- temp[[1]]
			fullmpls <- temp[[2]]
			
            if (!is.null(optMisfit)) 
                {
                  # if misfit needs to be optimized
                  stopifnot(optMisfit == "min" || optMisfit == "max")
                  
				  fillFirst <- ord[ord != 2][1] == 3
			
                  for (i in groupLoop) {
                    fullpls[[i]] <- lapply(paramSet[[i]], rawDraw, misSpec = FALSE, constraint = FALSE)
                  }
				  fullplsdraw <- fullpls
				  
				  if(fillFirst) {
					fullpls <- mapply(fillParam, fullpls, covStat, SIMPLIFY = FALSE)
					fullpls <- equalCon(fullpls, paramSet, fill=TRUE, con=con)
				  } else {
				    fullpls <- equalCon(fullpls, paramSet, fill=FALSE, con=con)
					fullpls <- mapply(fillParam, fullpls, covStat, SIMPLIFY = FALSE)
				  }
                  #fullpls <- mapply(fillParam, fullpls, covStat, SIMPLIFY = FALSE)  # fill after drawing misspec
                  macsPopls <- mapply(createImpliedMACS, fullpls, covStat, SIMPLIFY = FALSE)
                  
                  if (!(all(is.finite(unlist(lapply(macsPopls, "[[", 2)))) && (sum(unlist(lapply(lapply(lapply(macsPopls, 
                    "[[", 2), eigen), "[[", 1)) <= 0) == 0) && all(sapply(fullpls, 
                    validateObject)))) {
                    ## Checks: 1. all covariances are finite 2. Eigenvalues are less than zero 3.
                    ## Paths and variances are valid
                    next
                    draw <- draw + 1
                    # because pop matrix is invalid, move on to next draw
                  }
                  
                  mls <- list()  # list of misspec draws numIterations by groups. length(mls) = optDraws, length(mls[[1]]) = numgroups
                  macsMisls <- list()  # list of misspec macs, numIterations by groups
                  
                  temp <- list()
                  for (i in seq_len(optDraws)) {
                    #misspecDraws <- list()
                    macsMis <- list()
                    temp[[i]] <- list()
                    for (j in groupLoop) {
                      temp[[i]][[j]] <- lapply(paramSet[[j]], rawDraw, misOnly = TRUE)
					}  
					misspecDraws <- misspecOrder(real=fullplsdraw, misDraw=temp[[i]], paramSet=paramSet, con=con, ord=ord, covStat=covStat)[[2]]
					  
					for (j in groupLoop) {  
                      tempMacs <- createImpliedMACS(misspecDraws[[j]], covStat[[j]])
                      if (all(is.finite(tempMacs$CM)) && (sum(eigen(tempMacs$CM)$values <= 
                        0) == 0)) {
                        macsMis[[j]] <- tempMacs
                      } else {
                        macsMis[[j]] <- NULL
                      }
                    }
                    mls[[i]] <- misspecDraws
                    macsMisls[[i]] <- macsMis
                  }
                  
                  p <- length(macsPopls[[1]]$M)
                  nElements <- (p + (p * (p + 1)/2)) * ngroups
                  dfParam <- nElements - numFree
                  misfit <- matrix(0, optDraws, 4)  # need to change for srmr
                  colnames(misfit) <- c("f0", "rmsea", "srmr", "iter")
                  
                  ## For each iteration, send ith draw for each group to calculate the population
                  ## discrepancy, misspec vs. nonmisspec params.
                  
                  for (i in seq_len(optDraws)) {
                    paramM <- lapply(macsPopls, FUN = "[[", 1)
                    paramCM <- lapply(macsPopls, FUN = "[[", 2)
                    misspecM <- lapply(macsMisls[[i]], FUN = "[[", 1)
                    misspecCM <- lapply(macsMisls[[i]], FUN = "[[", 2)
                    if (is.null(misspecM) || is.null(misspecCM)) {
                      misfit[i, ] <- rep(NULL, 3)
                    } else {
                      misfit[i, ] <- c(popMisfitMACS(paramM, paramCM, misspecM, misspecCM, 
                        dfParam, fit.measures = "all"), i)
                    }
                  }
                  
                  if (optMisfit == "min") {
                    misfit <- misfit[sort.list(misfit[, 1]), ]
                    iter <- misfit[1, 4]
                  } else if (optMisfit == "max") {
                    misfit <- misfit[sort.list(misfit[, 1], decreasing = TRUE), ]
                    iter <- misfit[1, 4]
                  }
                  
                  fullmpls <- mls[[iter]]
				  fullmls <- temp[[iter]]
                  
                }  # End optimization, return control to normal misspec checks / draws
            
            if (all(sapply(fullpls, validateObject))) {
                redpls <- lapply(fullpls, reduceMatrices)
                redmpls <- lapply(fullmpls, reduceMatrices)
                redmls <- fullmls #lapply(fullmls, reduceMatrices)
                
                # if (!is.null(param) && !is.null(misspec)) {
                macsPopls <- mapply(createImpliedMACS, redpls, covStat, SIMPLIFY = FALSE)
                macsMisls <- mapply(createImpliedMACS, redmpls, covStat, SIMPLIFY = FALSE)
                ## Checks: 1. all covariances are finite 2. Eigenvalues are less than zero 3.
                ## Paths and variances are valid
                if (!(all(is.finite(unlist(lapply(macsPopls, "[[", 2)))) && (sum(unlist(lapply(lapply(lapply(macsPopls, 
                  "[[", 2), eigen), "[[", 1)) <= 0) == 0) && all(is.finite(unlist(lapply(macsMisls, 
                  "[[", 2)))) && (sum(unlist(lapply(lapply(lapply(macsMisls, "[[", 
                  2), eigen), "[[", 1)) <= 0) == 0))) {
                  # cat('FAIL')
                  draw <- draw + 1
                  next
                  ## because pop matrix is invalid or pop+misspec, move on to next draw
                }
                
                if (is.null(misfitBounds)) {
                  break
                } else {
                  p <- length(macsPopls[[1]]$M)
                  nElements <- (p + (p * (p + 1)/2)) * ngroups
                  nFree <- numFree
                  dfParam <- nElements - nFree
                  misfit <- popMisfitMACS(paramM = lapply(macsPopls, "[[", 1), paramCM = lapply(macsPopls, 
                    "[[", 2), misspecM = lapply(macsMisls, "[[", 1), misspecCM = lapply(macsMisls, 
                    "[[", 2), fit.measures = misfitType, dfParam = dfParam)
                  if (averageNumMisspec) 
                    misfit <- misfit/nFree
                  
                  if (!is.null(misfit) && (misfit > misfitBounds[1] & misfit < misfitBounds[2])) {
                    break
                  } else {
                    draw <- draw + 1
                    if (draw > maxDraw) {
                      stop(paste0("Cannot obtain misfit in bounds within maximum number of draws. Last ", 
                        misfitType, ": ", misfit))
                    }
                    next
                  }
                }
            } else {
                # one or more parameter sets in fullpls is not valid
                draw <- draw + 1
                if (draw > maxDraw) {
                  stop("Cannot obtain valid parameter set within maximum number of draws.")
                }
                next
            }
            
        } else {
			
            # no misspecification present
			
			# Exclude the misspecification from the order
			ord <- ord[ord != 2]
			unfillpls <- list()
            fullpls <- list()  # list of drawn parameter sets (filled)
            for (i in groupLoop) {
				unfillpls[[i]] <- lapply(paramSet[[i]], rawDraw, misSpec = FALSE, constraint = FALSE)
            }
			fillFirst <- ord[1] == 3
			if(fillFirst) {
				fullpls <- mapply(fillParam, unfillpls, covStat, SIMPLIFY = FALSE)
				fullpls <- equalCon(fullpls, paramSet, fill=TRUE, con=con)
			} else {
				fullpls <- equalCon(unfillpls, paramSet, fill=FALSE, con=con)
				fullpls <- mapply(fillParam, fullpls, covStat, SIMPLIFY = FALSE)
			}
			
            if (all(sapply(fullpls, validateObject))) {
                redpls <- lapply(fullpls, reduceMatrices)
                macsPopls <- mapply(createImpliedMACS, redpls, covStat, SIMPLIFY = FALSE)
                if (all(is.finite(unlist(lapply(macsPopls, "[[", 2)))) && all(unlist(lapply(lapply(lapply(macsPopls, 
                  "[[", 2), eigen), "[[", 1)) > 0)) {
                  break
                }
            } 
			draw <- draw + 1
			if (draw > maxDraw) {
			  stop("Cannot obtain valid parameter set within maximum number of draws.")
			}
			next
        }
        
    }
    if (draw < maxDraw) {
        final <- list()
        for (i in groupLoop) {
			if (misspec) {
				final[[i]] <- list(param = redpls[[i]], misspec = redmpls[[i]], misOnly = redmls[[i]])
			} else {
				final[[i]] <- list(param = redpls[[i]], misspec = NULL, misOnly = NULL)
			}
        }
        return(final)
    } else {
        stop(paste0("Cannot make a good set of parameters with given constraints within the maximum number of draws.\n", 
            misfitType, ": ", round(misfit, 6)))
    }
}


## Takes one SimMatrix and returns a matrix with numerical values for
## population parameters.  If constraint = TRUE, then constraints are applied
## simultaneously.  if misSpec = TRUE, then a list is returned with [[1]]
## parameters with no misspec and [[2]] same parameters + misspec (if any) if
## missOnly = TRUE, then only the parameters + misspecification is returned If
## a matrix is symmetric, it is arbitrarily chosen that parameters on the upper
## tri are set equal to the parameters on the lower tri.
rawDraw <- function(simDat, constraint = TRUE, misSpec = TRUE, parMisOnly = FALSE, 
    misOnly = FALSE) {
    if (class(simDat) == "SimMatrix" || class(simDat) == "SimVector") {
        free <- as.vector(simDat@free)
        popParam <- as.vector(simDat@popParam)
        misspec <- as.vector(simDat@misspec)
        
        ## param will contain population parameters, fixed and freed only.  paramMis
        ## will contain param + any misspecification missRaw will contain ONLY the
        ## misspecification
        param <- paramMis <- suppressWarnings(as.numeric(as.vector(free)))
		
        ## param & paramMis now contain any fixed numeric values in free
        missRaw <- rep(0, length(free))
        
        
        if (constraint) {
            conList <- NULL
            isLabel <- is.label(free)
            for (i in seq_along(free)) {
                ## Options: Label (constraint), or Free
                if (isLabel[i]) {
                  label <- free[i]
                  if (is.null(conList[label]) || is.na(conList[label])) {
                    # if label isn't in constraint list
                    param[i] <- paramMis[i] <- eval(parse(text = popParam[i]))  # draw
                    conList <- c(conList, param[i])  #Add value to constraint list
                    names(conList)[length(conList)] <- label  # Add label to constraint list
                  } else {
                    param[i] <- paramMis[i] <- conList[label]
                  }
                } else if (is.na(param[i])) {
                  # Is not a label, but is NA
                  param[i] <- paramMis[i] <- eval(parse(text = popParam[i]))  # normal draw
                }
                # Any entry (fixed or freed) can optionally have misspecification, even free
                # random parameters.
				if (misSpec) {
					if (!is.nan(misspec) && length(misspec) > 0 && !is.empty(misspec[i])) {
					  missRaw[i] <- eval(parse(text = misspec[i]))
					  paramMis[i] <- param[i] + missRaw[i]
					}
				}
            }
        } else if (misSpec) {
            # Don't apply constraints but apply misspecification
            for (i in seq_along(free)) {
                if (is.na(param[i])) {
                  param[i] <- paramMis[i] <- eval(parse(text = popParam[i]))
                }
                # Again, any entry can optionally have misspecification.
                if (!is.nan(misspec) && length(misspec) > 0 && !is.empty(misspec[i])) {
                  # Is not free - check for misspecification
                  missRaw[i] <- eval(parse(text = misspec[i]))
                  paramMis[i] <- param[i] + missRaw[i]
                }
            }
        } else {
            # Don't apply constraints or misspecification
            for (i in seq_along(free)) {
                if (is.na(param[i])) {
                  param[i] <- eval(parse(text = popParam[i]))
                }
            }
        }
        if (class(simDat) == "SimMatrix") {
            param <- matrix(param, nrow = nrow(simDat@free), ncol = ncol(simDat@free))
            paramMis <- matrix(paramMis, nrow = nrow(simDat@free), ncol = ncol(simDat@free))
            missRaw <- matrix(missRaw, nrow = nrow(simDat@free), ncol = ncol(simDat@free))
            if (simDat@symmetric) {
                param[upper.tri(param)] <- t(param)[upper.tri(param)]
                paramMis[upper.tri(paramMis)] <- t(paramMis)[upper.tri(paramMis)]
                missRaw[upper.tri(missRaw)] <- t(missRaw)[upper.tri(missRaw)]
            }
        }
        # This is not the full set of options.
        if (misSpec && !parMisOnly && !misOnly) {
            return(list(param = param, paramMis = paramMis, missRaw = missRaw))
        } else if (parMisOnly) {
            return(paramMis)
        } else if (misOnly) {
            return(missRaw)
        } else {
            return(param = param)
        }
    } else {
        return(NULL)
    }  # Was not a SimMatrix or SimVector
}

# Auto-completition of parameters
fillParam <- function(rawParamSet, covStat = NULL) {
    require(lavaan)
    LY <- rawParamSet$LY
    VTE <- rawParamSet$VTE
    TE <- rawParamSet$TE
    RTE <- rawParamSet$RTE
	if(!is.null(RTE)) diag(RTE) <- 1
    VY <- rawParamSet$VY
    TY <- rawParamSet$TY
    MY <- rawParamSet$MY
    BE <- rawParamSet$BE
    VPS <- rawParamSet$VPS
    PS <- rawParamSet$PS
    RPS <- rawParamSet$RPS
	if(!is.null(RPS)) diag(RPS) <- 1
    VE <- rawParamSet$VE
    AL <- rawParamSet$AL
    ME <- rawParamSet$ME
    KA <- rawParamSet$KA
	GA <- rawParamSet$GA
	
	MZ <- NULL
	CZ <- NULL
	isCov <- !is.null(covStat)
	if(isCov) {
		MZ <- covStat$MZ
		CZ <- covStat$CZ
	}
	
    if (is.null(BE) && !is.null(LY)) {
        # CFA
        if (is.null(PS)) {
			#if(isCov) VE <- 
			VPS <- findFactorResidualVar(matrix(0, nrow(RPS), ncol(RPS)), RPS, totalVarPsi = VE, gamma = GA, covcov = CZ)
            PS <- suppressWarnings(cor2cov(RPS, sqrt(VPS)))
        } else {
            VPS <- diag(PS)
            RPS <- cov2corMod(PS)
            VE <- findFactorTotalVar(matrix(0, nrow(RPS), ncol(RPS)), RPS, VPS, gamma = GA, covcov = CZ)
        }
		if (is.null(ME)) 
            ME <- findFactorMean(matrix(0, length(AL), length(AL)), AL, gamma = GA, covmean = MZ)
        if (is.null(AL)) 
            AL <- findFactorIntercept(matrix(0, length(ME), length(ME)), ME, gamma = GA, covmean = MZ)

		# CE is model-implied covariance
		CE <- findFactorTotalCov(matrix(0, nrow(RPS), ncol(RPS)), psi = PS, gamma = GA, covcov = CZ)
        if (is.null(TE)) {
            if (is.null(VTE)) 
                VTE <- findIndResidualVar(LY, CE, VY, kappa = KA, covcov = CZ)  
            if (is.null(VY)) 
                VY <- findIndTotalVar(LY, CE, VTE, kappa = KA, covcov = CZ)
            TE <- suppressWarnings(cor2cov(RTE, suppressWarnings(sqrt(VTE))))
        } else {
            VTE <- diag(TE)
            RTE <- cov2corMod(TE)
            VY <- findIndTotalVar(LY, CE, VTE, kappa = KA, covcov = CZ)
        }
        if (is.null(MY)) 
            MY <- findIndMean(LY, ME, TY, kappa = KA, covmean = MZ)
        if (is.null(TY)) 
            TY <- findIndIntercept(LY, ME, MY, kappa = KA, covmean = MZ)
        
    } else if (!is.null(BE) && is.null(LY)) {
        # Path
        if (is.null(PS)) {
            if (is.null(VPS)) 
                VPS <- findFactorResidualVar(BE, RPS, VE, gamma = GA, covcov = CZ)
            if (is.null(VE)) 
                VE <- findFactorTotalVar(BE, RPS, VPS, gamma = GA, covcov = CZ)
            PS <- suppressWarnings(cor2cov(RPS, suppressWarnings(sqrt(VPS))))
        } else {
            VPS <- diag(PS)
            RPS <- cov2corMod(PS)
            VE <- findFactorTotalVar(BE, RPS, VPS, gamma = GA, covcov = CZ)
        }
        if (is.null(ME)) 
            ME <- findFactorMean(BE, AL, gamma = GA, covmean = MZ)
        if (is.null(AL)) 
            AL <- findFactorIntercept(BE, ME, gamma = GA, covmean = MZ)
        
    } else if (!is.null(BE) && !is.null(LY)) {
        # SEM
        if (is.null(PS)) {
            if (is.null(VPS)) 
                VPS <- findFactorResidualVar(BE, RPS, VE, gamma = GA, covcov = CZ)
            if (is.null(VE)) 
                VE <- findFactorTotalVar(BE, RPS, VPS, gamma = GA, covcov = CZ)
            PS <- suppressWarnings(cor2cov(RPS, suppressWarnings(sqrt(VPS))))
        } else {
            VPS <- diag(PS)
            RPS <- cov2corMod(PS)
            VE <- findFactorTotalVar(BE, RPS, VPS, gamma = GA, covcov = CZ)
        }
        if (is.null(ME)) 
            ME <- findFactorMean(BE, AL, gamma = GA, covmean = MZ)
        if (is.null(AL)) 
            AL <- findFactorIntercept(BE, ME, gamma = GA, covmean = MZ)
        facCov <- findFactorTotalCov(BE, PS, gamma = GA, covcov = CZ)
        if (is.null(TE)) {
            if (is.null(VTE)) 
                VTE <- findIndResidualVar(LY, facCov, VY, kappa = KA, covcov = CZ)
            if (is.null(VY)) 
                VY <- findIndTotalVar(LY, facCov, VTE, kappa = KA, covcov = CZ)
            TE <- suppressWarnings(cor2cov(RTE, suppressWarnings(sqrt(VTE))))
        } else {
            RTE <- cov2corMod(TE)
            VTE <- diag(TE)
            VY <- findIndTotalVar(LY, facCov, VTE, kappa = KA, covcov = CZ)
        }
        if (is.null(MY)) 
            MY <- findIndMean(LY, ME, TY, kappa = KA, covmean = MZ)
        if (is.null(TY)) 
            TY <- findIndIntercept(LY, ME, MY, kappa = KA, covmean = MZ)
    }
    fullParamSet <- list(LY = LY, VTE = VTE, TE = TE, RTE = RTE, VY = VY, TY = TY, 
        MY = MY, BE = BE, VPS = VPS, PS = PS, RPS = RPS, VE = VE, AL = AL, ME = ME, GA = GA, KA = KA)
    return(fullParamSet)
}

# Reduce RPS/RTE to PS/TE if present.
reduceMatrices <- function(paramSet) {
    require(lavaan)
    
    if (is.null(paramSet$PS)) 
        paramSet$PS <- suppressWarnings(cor2cov(paramSet$RPS, sqrt(paramSet$VPS)))
    
    # If SEM or CFA, Convert RTE/VTE to TE
    if (!is.null(paramSet$LY) && is.null(paramSet$TE)) {
        paramSet$TE <- suppressWarnings(cor2cov(paramSet$RTE, sqrt(paramSet$VTE)))
    }
    
    reducedParamSet <- list(PS = paramSet$PS, BE = paramSet$BE, AL = paramSet$AL, 
        TE = paramSet$TE, LY = paramSet$LY, TY = paramSet$TY, GA = paramSet$GA, KA = paramSet$KA)
    return(reducedParamSet)
}

createImpliedMACS <- function(reducedParamSet, covStat = NULL) {
    implied.mean <- NULL
    implied.covariance <- NULL
	if(!is.null(covStat)) {
		reducedParamSet <- combineParamSetCov(reducedParamSet, covStat = covStat)
	}
    ID <- diag(nrow(reducedParamSet$PS))
    if (!is.null(reducedParamSet$BE)) {
        # Path or SEM
		temp <- solve(ID - reducedParamSet$BE)
        implied.mean <- temp %*% reducedParamSet$AL
        implied.covariance <- temp %*% reducedParamSet$PS %*% t(temp)
        if (!is.null(reducedParamSet$LY)) {
            # SEM
            implied.mean <- reducedParamSet$TY + (reducedParamSet$LY %*% implied.mean)
            implied.covariance <- (reducedParamSet$LY %*% implied.covariance %*% 
                t(reducedParamSet$LY)) + reducedParamSet$TE
        }
    } else {
        # CFA
		facMean <- reducedParamSet$AL
		facCov <- reducedParamSet$PS
        implied.mean <- reducedParamSet$LY %*% facMean + reducedParamSet$TY
        implied.covariance <- (reducedParamSet$LY %*% facCov %*% t(reducedParamSet$LY)) + 
            reducedParamSet$TE
    }
    return(list(M = as.vector(implied.mean), CM = implied.covariance))
}

createImpliedConditionalMACS <- function(reducedParamSet, covData) {
    implied.mean <- list()
    implied.covariance <- NULL
	covData <- as.list(data.frame(t(covData)))
	covData <- lapply(covData, as.matrix)
	nf <- nrow(reducedParamSet$PS)
    ID <- diag(nf)
	if (is.null(reducedParamSet$BE)) reducedParamSet$BE <- matrix(0, nf, nf) #CFA
	temp <- solve(ID - reducedParamSet$BE)
	implied.mean <- temp %*% reducedParamSet$AL
	covBeta <- temp %*% reducedParamSet$GA
	factorMeanShift <- lapply(covData, function(x) covBeta %*% x)
	implied.mean <- lapply(factorMeanShift, function(x) as.matrix(implied.mean + x))
	implied.covariance <- temp %*% reducedParamSet$PS %*% t(temp)
	if (!is.null(reducedParamSet$LY)) {
		# SEM or CFA
		indicatorMeanShift <- lapply(covData, function(x) reducedParamSet$KA %*% x)
		implied.mean <- lapply(implied.mean, function(x) reducedParamSet$TY + (reducedParamSet$LY %*% x))
		implied.mean <- mapply("+", implied.mean, indicatorMeanShift, SIMPLIFY=FALSE) 
		implied.covariance <- (reducedParamSet$LY %*% implied.covariance %*% 
			t(reducedParamSet$LY)) + reducedParamSet$TE
	}
    return(list(M = implied.mean, CM = implied.covariance))
}

combineParamSetCov <- function(reducedParamSet, covStat = NULL) {
	nz <- length(covStat$MZ)
	if(!is.null(reducedParamSet$BE)) reducedParamSet$BE <- parseGammaToBeta(reducedParamSet$BE, reducedParamSet$GA)
	if(!is.null(reducedParamSet$AL)) reducedParamSet$AL <- as.matrix(c(covStat$MZ, reducedParamSet$AL))
	if(!is.null(reducedParamSet$PS)) reducedParamSet$PS <- parseCovCovToPsi(reducedParamSet$PS, covStat$CZ)
	if(!is.null(reducedParamSet$LY)) reducedParamSet$LY <- parseKappaToLambda(reducedParamSet$LY, reducedParamSet$KA)
	if(!is.null(reducedParamSet$TE)) reducedParamSet$TE <- parseCovToTheta(reducedParamSet$TE, covStat$CZ)
	if(!is.null(reducedParamSet$TY)) reducedParamSet$TY <- as.matrix(c(rep(0, nz), reducedParamSet$TY))
	reducedParamSet$KA <- NULL
	reducedParamSet$GA <- NULL
	reducedParamSet
}

parseGammaToBeta <- function(beta, gamma) {
	nf <- nrow(beta)
	nz <- ncol(gamma)
	result <- matrix(0, nf + nz, nf + nz)
	result[nz + 1:nf, nz + 1:nf] <- beta
	result[nz + 1:nf, 1:nz] <- gamma
	result
}

parseCovCovToPsi <- function(psi, sigmaxx) {
	nf <- nrow(psi)
	nz <- ncol(sigmaxx)
	result <- matrix(0, nf + nz, nf + nz)
	result[nz + 1:nf, nz + 1:nf] <- psi
	result[1:nz, 1:nz] <- sigmaxx	
	result
}

parseKappaToLambda <- function(lambda, kappa) {
	ni <- nrow(lambda)
	nf <- ncol(lambda)
	nz <- ncol(kappa)
	result <- matrix(0, nz + ni, nz + nf)
	result[1:nz, 1:nz] <- diag(nz)
	result[(nz+1):(nz+ni), 1:nz] <- kappa
	result[(nz+1):(nz+ni), (nz+1):(nz+nf)] <- lambda
	result
}

parseCovToTheta <- function(theta, sigmaxx) {
	nz <- ncol(sigmaxx)
	ni <- nrow(theta)
	result <- matrix(0, ni + nz, ni + nz)
	result[(nz + 1):(nz + ni), (nz + 1):(nz + ni)] <- theta
	result
}

## Now takes lists for the matrices for mg

popMisfitMACS <- function(paramM, paramCM, misspecM, misspecCM, dfParam = NULL, fit.measures = "all") {
    if (!is.list(paramM)) 
        paramM <- list(paramM)
    if (!is.list(misspecM)) 
        misspecM <- list(misspecM)
    if (!is.list(paramCM)) 
        paramCM <- list(paramCM)
    if (!is.list(misspecCM)) 
        misspecCM <- list(misspecCM)
    
    if (fit.measures == "all") {
        fit.measures <- c("f0", "rmsea", "srmr")
        # fit.measures <- c('f0','rmsea')
        if (is.null(dfParam)) 
            fit.measures <- fit.measures[c(1, 3)]
    }
    p <- length(paramM[[1]])
    fit.measures <- tolower(fit.measures)
    
    # If multiple group, f0 is added for each group
    if (is.list(paramM) && is.list(paramCM) && is.list(misspecM) && is.list(misspecCM)) {
        tempf0 <- mapply(FUN = popDiscrepancy, paramM, paramCM, misspecM, misspecCM)
        f0 <- sum(tempf0)
    } else {
        f0 <- popDiscrepancy(paramM, paramCM, misspecM, misspecCM)
    }
    
    if (!is.null(f0)) {
        
        rmsea <- NULL
        srmr <- NULL
        result <- NULL
        if (any(fit.measures %in% "f0")) {
            result <- c(result, f0)
        }
        if (any(fit.measures %in% "rmsea")) {
            rmsea <- sqrt(f0/dfParam)
            result <- c(result, rmsea)
        }
        if (any(fit.measures %in% "srmr")) {
            disSquared <- mapply(function(x, y) {
                (cov2cor(x) - cov2cor(y))^2
            }, x = misspecCM, y = paramCM)
            sumDis <- sapply(disSquared, function(x) {
                sum(x[lower.tri(x, diag = TRUE)])
            })
            nElements <- length(sumDis) * (p * (p + 1))/2
            numerator <- sum(sumDis)
            srmr <- sqrt(numerator/nElements)
            result <- c(result, srmr)
        }
        result <- as.vector(result)
        names(result) <- fit.measures
    } else {
        result <- NULL
    }
    
    return(result)
}

# F0 in population: The discrepancy due to approximation (Browne & Cudeck,
# 1992)

popDiscrepancy <- function(paramM, paramCM, misspecM, misspecCM) {
    p <- length(misspecM)
    inv <- solve(paramCM)
    dis.CM <- misspecCM %*% inv
    t.1 <- sum(diag(dis.CM))
    t.1.1 <- det(dis.CM)
    if (t.1.1 < 0) 
        return(NULL)
    t.2 <- log(t.1.1)
    dis.M <- as.matrix(misspecM - paramM)
    t.3 <- t(dis.M) %*% inv %*% dis.M
    discrepancy <- t.1 - t.2 - p + t.3
    return(discrepancy)
}

# cov2corMod: The cov2cor function that takes care of the zero-variance
# variables

cov2corMod <- function(V) {
    targetCol <- which(diag(V) != 0)
    if (!length(targetCol) == 0) {
        V[targetCol, targetCol] <- cov2cor(V[targetCol, targetCol])
    }
    return(V)
}

equalCon <- function(pls, dgen, fill=FALSE, con=NULL) {
	# Collapse all labels
	temp <- extractLab(pls, dgen, fill=fill, con=con)
	target <- temp[[1]]
	realval <- temp[[2]]
	dgen <- temp[[3]]
	if(!is.null(con) && !is.null(con[[1]])) {
		temp <- applyConScript(target, realval, con)
		target <- temp[[1]]
		realval <- temp[[2]]
	}
	if(length(target) == 0) {
		return(pls)
	} else {
		for(j in 1:length(target)) {
			equalVal <- 0
			numFound <- 1
			for(g in 1:length(pls)) {
				for(i in seq_along(names(pls[[g]]))) {
					if(!is.null(pls[[g]][[i]])) {
						temp <- getElement(dgen[[g]], names(pls[[g]])[i])
						if(!is.null(temp)) {
							index <- which(temp@free == target[j])
							for(k in seq_len(length(index))) {
								# if(numFound == 1) {
									# equalVal <- pls[[g]][[i]][index[k]]
								# } else {
									# pls[[g]][[i]][index[k]] <- equalVal
								# }
								pls[[g]][[i]][index[k]] <- realval[j]
								numFound <- numFound + 1
							}
						}
					}
				}
			}
		}
		return(pls)
	}
	
}


extractLab <- function(pls, dgen, fill=FALSE, con=NULL) {

	free <- lapply(dgen, function(x) lapply(x, function(y) if(is.null(y)) { return(NULL) } else { return(slot(y, "free")) }))
	if(fill) {
		
		for(i in 1:length(free)) {
			temp <- free[[i]]
			if(is.null(temp$PS)) {
				if(!is.null(temp$RPS)) {
					temp$PS <- temp$RPS
					dgen[[i]]$PS <- dgen[[i]]$RPS
					if(!is.null(temp$VPS)) {
						diag(temp$PS) <- temp$VPS
					} else if(!is.null(temp$VE)) {
						diag(temp$PS) <- temp$VE
					}
					dgen[[i]]$PS@free <- temp$PS
				}
			}
			if(is.null(temp$TE)) {
				if(!is.null(temp$RTE)) {
					temp$TE <- temp$RTE
					dgen[[i]]$TE <- dgen[[i]]$RTE
					if(!is.null(temp$VTE)) {
						diag(temp$TE) <- temp$VTE
					} else if(!is.null(temp$VY)) {
						diag(temp$TE) <- temp$VY
					}
					dgen[[i]]$TE@free <- temp$TE
				}
			}
			if(is.null(temp$AL)) {
				if(!is.null(temp$ME)) {
					temp$AL <- temp$ME
					dgen[[i]]$AL <- dgen[[i]]$ME
				}
			}
			if(is.null(temp$TY)) {
				if(!is.null(temp$MY)) {
					temp$TY <- temp$MY
					dgen[[i]]$TY <- dgen[[i]]$MY
				}
			}
			temp$RPS <- NULL
			temp$VPS <- NULL
			temp$VE <- NULL
			temp$RTE <- NULL
			temp$VTE <- NULL
			temp$VY <- NULL
			temp$ME <- NULL
			temp$MY <- NULL
			free[[i]][names(temp)] <- temp
		}
	}
	free2 <- do.call(c, lapply(free, function(x) do.call(c, x)))
	lab <- free2[is.na(suppressWarnings(as.numeric(as.vector(free2)))) & !is.na(free2)]
	target <- unique(lab) #[duplicated(lab)])
	val <- lapply(pls, function(x) lapply(x, function(y) if(is.null(y)) { return(NULL) } else { return(y) }))
	val2 <- do.call(c, lapply(val, function(x) do.call(c, x)))
	val2 <- val2[match(names(free2), names(val2))]
	realval <- val2[is.na(suppressWarnings(as.numeric(as.vector(free2)))) & !is.na(free2)]
	realval <- realval[match(target, lab)]
	list(target, realval, dgen)
}

# Make the variable name very very weird so that the assign and get functions will work by setting the labels in the internal environment
applyConScript <- function(xxxtargetxxx, xxxvalxxx, xxxconxxx, xxxthresholdxxx = 0.00001) {
	for(i in 1:length(xxxtargetxxx)) {
		assign(xxxtargetxxx[i], xxxvalxxx[i])
	}
	for(i in 1:length(xxxconxxx[[1]])) {
		if(xxxconxxx[[2]][i] %in% c(":=", "==")) {
			assign(xxxconxxx[[1]][i], eval(parse(text = xxxconxxx[[3]][i])))
		} else if (xxxconxxx[[2]][i] == ">") {
			xxxlhsxxx <- get(xxxconxxx[[1]][i])
			xxxrhsxxx <- eval(parse(text = xxxconxxx[[3]][i]))
			if(xxxlhsxxx < xxxrhsxxx) {
				assign(xxxconxxx[[1]][i], xxxrhsxxx + xxxthresholdxxx)
			}
		} else if (xxxconxxx[[3]][i] == ">") {
			xxxlhsxxx <- get(xxxconxxx[[1]][i])
			xxxrhsxxx <- eval(parse(text = xxxconxxx[[3]][i]))
			if(xxxlhsxxx > xxxrhsxxx) {
				assign(xxxconxxx[[1]][i], xxxrhsxxx - xxxthresholdxxx)
			}		
		}
	}
	xxxalltargetxxx <- unique(c(xxxtargetxxx, xxxconxxx[[1]]))
	xxxresultxxx <- NULL
	for(i in 1:length(xxxalltargetxxx)) {
		xxxresultxxx <- c(xxxresultxxx, get(xxxalltargetxxx[i]))
	}
	list(xxxalltargetxxx, xxxresultxxx)
}

misspecOrder <- function(real, misDraw, paramSet, con, ord=c(1, 2, 3), covStat=NULL) {
	pls <- list()
	mpls <- list()
	if(is.null(covStat)) covStat <- rep(list(NULL), length(paramSet))
	# Order 1 = constraint, 2 = misspec, 3 = filling parameters
	if(isTRUE(all.equal(ord, c(1, 2, 3)))) {
		# con, misspec, fill
		real <- equalCon(real, paramSet, con=con)
		pls <- mapply(fillParam, real, covStat, SIMPLIFY = FALSE)
		mis <- imposeMis(real, misDraw)
		mpls <- mapply(fillParam, mis, covStat, SIMPLIFY = FALSE)
	} else if (isTRUE(all.equal(ord, c(1, 3, 2)))) {
		# con, fill, misspec
		real <- equalCon(real, paramSet, con=con)
		pls <- mapply(fillParam, real, covStat, SIMPLIFY = FALSE)
		mpls <- imposeMis(pls, misDraw)		
	} else if (isTRUE(all.equal(ord, c(2, 1, 3)))) {
		# mis, con, fill
		mis <- imposeMis(real, misDraw)
		mis <- equalCon(mis, paramSet, con=con)
		mpls <- mapply(fillParam, mis, covStat, SIMPLIFY = FALSE)
		real <- equalCon(real, paramSet, con=con)
		pls <- mapply(fillParam, real, covStat, SIMPLIFY = FALSE)	
	} else if (isTRUE(all.equal(ord, c(2, 3, 1)))) {
		# mis, fill, con
		mis <- imposeMis(real, misDraw)
		mis <- mapply(fillParam, mis, covStat, SIMPLIFY = FALSE)
		mpls <- equalCon(mis, paramSet, con=con, fill=TRUE)
		real <- mapply(fillParam, real, covStat, SIMPLIFY = FALSE)
		pls <- equalCon(real, paramSet, con=con, fill=TRUE)
	} else if (isTRUE(all.equal(ord, c(3, 1, 2)))) {
		# fill, con, mis
		real <- mapply(fillParam, real, covStat, SIMPLIFY = FALSE)
		pls <- equalCon(real, paramSet, con=con, fill=TRUE)
		mpls <- imposeMis(pls, misDraw)
	} else if (isTRUE(all.equal(ord, c(3, 2, 1)))) {
		# fill, mis, con
		real <- mapply(fillParam, real, covStat, SIMPLIFY = FALSE)
		pls <- equalCon(real, paramSet, con=con, fill=TRUE)
		mis <- imposeMis(real, misDraw)
		mpls <- equalCon(mis, paramSet, con=con, fill=TRUE)
	} else {
		stop("The 'ord' argument specification is incorrect")
	}
	list(pls, mpls)
}

imposeMis <- function(real, misDraw) {
	result <- list()
	for (j in 1:length(real)) {
		addMis <- real[[j]]
		name <- names(real[[j]])
		for(i in 1:length(real[[j]])) {
			tempmis <- getElement(misDraw[[j]], name[i])
			if(!is.null(tempmis)) {
				addMis[[i]] <- addMis[[i]] + tempmis
			} 
		}
		#addMis <- mapply("+", real[[j]], misDraw[[j]])
		result[[j]] <- lapply(addMis, FUN = function(x) {
			if (length(x) == 0) {
			  x <- NULL
			} else x
		  })
	}
	result
}
			
test.draw <- function() {
path.BE <- matrix(0, 4, 4)
path.BE[3, 1:2] <- NA
path.BE[4, 3] <- NA
starting.BE <- matrix("", 4, 4)
starting.BE[3, 1:2] <- "runif(1, 0.3, 0.5)"
starting.BE[4, 3] <- "runif(1,0.5,0.7)"
mis.path.BE <- matrix(0, 4, 4)
mis.path.BE[4, 1:2] <- "runif(1,-0.1,0.1)"
BE <- bind(path.BE, starting.BE, misspec=mis.path.BE)

residual.error <- diag(4)
residual.error[1,2] <- residual.error[2,1] <- NA
RPS <- binds(residual.error, "rnorm(1,0.3,0.1)")

ME <- bind(rep(NA, 4), 0)

Path.Model <- model(RPS = RPS, BE = BE, ME = ME, modelType="path")

param1 <- draw(Path.Model, misfitBounds = c(0.10, 0.12), misfitType="rmsea")
param2 <- draw(Path.Model, misfitBounds = c(0.03, 0.05), misfitType="f0")
param3 <- draw(Path.Model, optMisfit = "max", misfitType="f0")
param4 <- draw(Path.Model, optMisfit = "min", misfitType="f0")
param5 <- draw(Path.Model, optMisfit = "max", misfitType="f0", optDraws = 10)

}

test.unequalCon <- function() {
loading.in <- matrix(0, 6, 2)
loading.in[1:3, 1] <- c("load1", "load2", "load3")
loading.in[4:6, 2] <- c("load4", "load5", "load6")
mis <- matrix(0,6,2)
mis[loading.in == "0"] <- "runif(1, -0.1, 0.1)"
LY.in <- bind(loading.in, "runif(1, 0.7, 0.8)", mis)
latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPS <- binds(latent.cor, 0.5)
RTE <- binds(diag(6))
VTE <- bind(rep(NA, 6), 0.51)
VPS1 <- bind(rep(1, 2))
VPS2 <- bind(rep(NA, 2), c(1.1, 1.2))
script <- "
sth := load1 + load2 + load3
load4 == (load5 + load6) / 2
load4 > 0
load5 > 0
sth2 := load1 - load2
"
weak <- model(LY = LY.in, RPS = RPS, VPS=list(VPS1, VPS2), RTE = RTE, VTE=VTE, ngroups=2, modelType = "CFA", con=script)
dat <- generate(weak, 200)
out <- analyze(weak, dat)
Output <- sim(2, weak, n=200) 
}

test.order <- function() {
loading.in <- matrix(0, 6, 2)
loading.in[1:3, 1] <- c("load1", "load2", "load3")
loading.in[4:6, 2] <- c("load4", "load5", "load6")
mis <- matrix(0,6,2)
mis[loading.in == "0"] <- "runif(1, -0.1, 0.1)"
LY.in <- bind(loading.in, "runif(1, 0.7, 0.8)", mis)
latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPS <- binds(latent.cor, 0.5)
RTE <- binds(diag(6))
VTE <- bind(rep(NA, 6), 0.51)
VPS1 <- bind(rep(1, 2))
VPS2 <- bind(rep(NA, 2), c(1.1, 1.2))
script <- "
sth := load1 + load2 + load3
load4 == (load5 + load6) / 2
load4 > 0
load5 > 0
sth2 := load1 - load2
"
weak <- model(LY = LY.in, RPS = RPS, VPS=list(VPS1, VPS2), RTE = RTE, VTE=VTE, ngroups=2, modelType = "CFA", con=script)
draw(weak, createOrder=c(1, 2, 3))
draw(weak, createOrder=c(1, 3, 2))
draw(weak, createOrder=c(2, 1, 3))
draw(weak, createOrder=c(2, 3, 1))
draw(weak, createOrder=c(3, 1, 2))
draw(weak, createOrder=c(3, 2, 1))
dat1 <- generate(weak, 200, createOrder=c(1, 2, 3))
dat2 <- generate(weak, 200, createOrder=c(1, 3, 2))
dat3 <- generate(weak, 200, createOrder=c(2, 1, 3))
dat4 <- generate(weak, 200, createOrder=c(2, 3, 1))
dat5 <- generate(weak, 200, createOrder=c(3, 1, 2))
dat6 <- generate(weak, 200, createOrder=c(3, 2, 1))
Output1 <- sim(3, weak, n=200, createOrder=c(1, 2, 3))
Output2 <- sim(3, weak, n=200, createOrder=c(1, 3, 2))
Output3 <- sim(3, weak, n=200, createOrder=c(2, 1, 3))
Output4 <- sim(3, weak, n=200, createOrder=c(2, 3, 1))
Output5 <- sim(3, weak, n=200, createOrder=c(3, 1, 2))
Output6 <- sim(3, weak, n=200, createOrder=c(3, 2, 1))
}
