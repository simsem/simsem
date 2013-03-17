
combineSim <- function(...) {
  s4list <- list(...)
  if (!all(sapply(s4list, is, "SimResult"))) {
    stop("This function only combines objects of S4 class 'SimResult' ")
  }
  ## check whether seeds are all unique, remind user it might invalidate results
  if(multipleAnyEqualList(lapply(s4list, slot, name="seed"))) {
	warning("Some result objects have common seed number.")
  }
    
  ## check that all models are the same type
  mT <- sapply(s4list, function(dat) slot(dat, "modelType"))
  if (length(unique(mT)) > 1) {
    stop("Model Types are not identical. Do not combine SimResults from structurally different models. \n")
  } else mT = mT[1]
  
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
  nobs <- do.call("rbind", lapply(s4list, stackEm, "nobs"))
  
  if(all(is.na(misspecValue))) misspecValue <- data.frame(V1 = NA)
  if(all(is.na(popFit))) popFit <- data.frame(V1 = NA)
   
  ## function to stack paramValues so nrows == nReps, (unless it already is, e.g. random parameters)
  stackParams <- function(dat) {
    if (nrow(dat@paramValue) == 1) {
      paramVec <- dat@paramValue
      for (i in 2:dat@nRep) paramVec <- rbind(paramVec, dat@paramValue)
      return(paramVec)
    } else return(dat@paramValue)
  }
  ## save stacked paramValues
  pV <- do.call("rbind", lapply(s4list, stackParams))
  if (nrow(unique(pV)) == 1) pV <- unique(pV)
  
  ## save vectors. If single values, save them as vectors to match nReps rows in data.frames
  converged <- do.call("c" , lapply(s4list, function(dat) dat@converged))
  seed <- s4list[[length(s4list)]]@seed 
  n <- do.call("c" , lapply(s4list, function(dat) rep(dat@n, length.out = dat@nRep)))
  pmMCAR <- do.call("c" , lapply(s4list, function(dat) rep(dat@pmMCAR, length.out = dat@nRep)))
  pmMAR <- do.call("c" , lapply(s4list, function(dat) rep(dat@pmMAR, length.out = dat@nRep)))
  
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
	timing$InReps <- totaltime/nRep
	timing$StartTime <- Reduce(min, lapply(s4list, function(dat) dat@timing$StartTime))
	timing$EndTime <- Reduce(max, lapply(s4list, function(dat) dat@timing$EndTime))
    
  ## store in single S4 SimResult object, which is the return value of this function
  output <- new("SimResult", modelType = mT, nRep = nRep, coef = coef, se = se,
                fit = fit, converged = converged, paramValue = pV,
                misspecValue = misspecValue, popFit = popFit, FMI1 = FMI1, 
                FMI2 = FMI2, cilower = cilower, ciupper = ciupper, stdCoef = stdCoef, seed = seed, n = n, nobs = nobs,
                pmMCAR = pmMCAR, pmMAR = pmMAR, extraOut = extraOut, timing = timing, paramOnly = paramOnly)
				# nobs, paramOnly
  output
}
