source("model.R")
source("find.R")
source("validate.R")

# There has to be a better way of specifying what order things are generated. Constraint, Auto-completion, misspecification

drawParam <- function(model, misfitType, misfitBounds, averageNumMisspec, optMisfit, numIter) {
  paramSet <- model@dgen
  free <- max(model@pt$free)
  param <- NULL
  misspec <- NULL
  misspecAdd <- NULL
  implied.CM.param <- NULL
  implied.CM.misspec <- NULL
  misfit <- NULL
  count <- 0
  repeat {
    if (!isNullObject(objMisspec)) {
      
      if (validateObject(param)) {
        param <- reduceMatrices(param)
        misspec <- reduceMatrices(misspec)
        if (!isNullObject(param) && !isNullObject(misspec)) {
          implied.CM.param <- createImpliedMACS(param)
          implied.CM.misspec <- createImpliedMACS(misspec)
          if (all(is.finite(implied.CM.misspec$CM)) && (sum(eigen(implied.CM.misspec$CM)$values <= 0) == 0)) {
            if (isNullObject(objMisspec@misfitBound)) {
              break
            } else {
              p <- length(implied.CM.param$M)
              nElements <- p + (p * (p + 1)/2)
              nFree <- countFreeParameters(objSet)
              if (!isNullObject(objEqualCon)) 
                nFree <- nFree + countFreeParameters(objEqualCon)
              dfParam <- nElements - nFree
              misfit <- popMisfitMACS(implied.CM.param$M, implied.CM.param$CM, implied.CM.misspec$M, implied.CM.misspec$CM,
                                      fit.measures = objMisspec@misfitType, dfParam = dfParam)
              if (objMisspec@averageNumMisspec) 
                misfit <- misfit/countFreeParameters(objMisspec)
              
              if (!is.null(misfit) && (misfit > misfitBounds[1] & misfit < misfitBounds[2])) 
                break
            }
          }
        }
      }
    } else {
      param <- rawDrawSet(paramSet)
      if (validateObject(param)) {
        param <- reduceMatrices(param)
        if (!isNullObject(param)) {
          implied.CM.param <- createImpliedMACS(param)
          implied.CM.misspec <- implied.CM.param
          if (sum(eigen(implied.CM.param$CM)$values <= 0) == 0) 
            break
        }
      }
    }
    count <- count + 1
    if (count > maxDraw) 
      stop("The model cannot make a good set of parameters within limit of maximum random sampling of parameters")
  }
  return(list(real = param, misspec = misspec, misspecAdd = misspecAdd))
}

runMisspec <- function(paramSet, modelType, numFree, optMisfit = "none", numIter=1) {
  
  rawParamLS <- list()
  
  if (optMisfit == "none") {
    rawParam <- lapply(paramSet,rawDraw)
    fullParamls <- list()
    fullParams <- fillParam(lapply(rawParam,"[[",1),modelType=modelType)
    fullMiss <- fillParam(lapply(rawParam,"[[",2),modelType=modelType)
  } else {
    rawParam <- lapply(paramSet,rawDraw,miss=FALSE,mRaw=FALSE)
    missParam <- list()
    for(i in 1:numIter) {
      temp <- lapply(paramSet,rawDraw,missOnly=TRUE)
      addMis <- mapply("+",rawParam,temp)  # We only want to draw the raw params once, so we add the rawParameter values to each misspecified draw
      missParam[[i]] <- lapply(addMis, FUN=function(x) {if(length(x) == 0) {x <- NULL} else x})
    }
    fullParams <- fillParam(rawParam,modelType=modelType)
    fullMiss <- list()
    for(i in 1:length(rawParam)) {
      fullMiss[[i]] <- fillParam(missParam[[i]],modelType=modelType)
    }
    
  }
  
  ## fullParams = Constraints / Misspecification -> Auto completition of parameters -> 1 Full Parameter Set
  ## fullMiss = Constraints / Misspecification -> Auto completion of parameters -> Misspecification only, list if optMisfit = TRUE

  if (class(fullMiss[[1]]) == "list") {
    macsMis <- lapply(fullMiss, createImpliedMACS,modelType)
    macsPop <- createImpliedMACS(fullParams,modelType)
    p <- length(macsPop$M)
    nElements <- p + (p * (p + 1)/2)

    dfParam <- nElements - numFree
    misfit <- sapply(macsMis, popMisfit, param = macsPop, dfParam = dfParam, fit.measures = "all")
    misfit <- misfit[!sapply(misfit,is.null)]
    element <- NULL
    if (optMisfit == "min") {
      element <- which.min(sapply(misfit,"[[",1))
    } else if (optMisfit == "max") {
      element <- which.max(sapply(misfit,"[[",1))
    } else {
      stop("Something is wrong in the runMisspec function!")
    }
    finalMis <- fullMiss[[element]]
    ## Mis <- Mis[[element]]
  } else {
    finalMis <- fullMiss
    ##  Mis <- Mis[[1]]
  }
  return(list(param = fullParams, misspec = finalMis))
}


# Auto-completition of parameters
fillParam <- function(rawParamSet, modelType) {
    require(lavaan)
    LY <- rawParamSet$LY
    VTE <- rawParamSet$VTE
    TE <- rawParamSet$TE
    RTE <- rawParamSet$RTE
    VY <- rawParamSet$VY
    TY <- rawParamSet$TY
    MY <- rawParamSet$MY
    BE <- rawParamSet$BE
    VPS <- rawParamSet$VPS
    PS <- rawParamSet$PS
    RPS <- rawParamSet$RPS
    VE <- rawParamSet$VE
    AL <- rawParamSet$AL
    ME <- rawParamSet$ME

    if (modelType == "CFA") {
        if (is.null(PS)) {
            PS <- suppressWarnings(cor2cov(RPS, sqrt(VE)))
        } else {
            VE <- diag(PS)
            VPS <- diag(PS)
            RPS <- cov2corMod(PS)
        }
        if (is.null(TE)) {
            if (is.null(VTE)) 
                VTE <- findIndResidualVar(LY, PS, VY)  # PS is model-implied covariance
            if (is.null(VY)) 
                VY <- findIndTotalVar(LY, PS, VTE)
            TE <- suppressWarnings(cor2cov(RTE, suppressWarnings(sqrt(VTE))))
        } else {
            VTE <- diag(TE)
            RTE <- cov2corMod(TE)
            VY <- findIndTotalVar(LY, PS, VTE)
        }
        if (is.null(MY)) 
            MY <- findIndMean(LY, ME, TY)
        if (is.null(TY)) 
            TY <- findIndIntercept(LY, ME, MY)
        
    } else if (modelType == "Path") {
        if (is.null(PS)) {
            if (is.null(VPS)) 
                VPS <- findFactorResidualVar(BE, RPS, VE)
            if (is.null(VE)) 
                VE <- findFactorTotalVar(BE, RPS, VPS)
            PS <- suppressWarnings(cor2cov(RPS, suppressWarnings(sqrt(VPS))))
        } else {
            VPS <- diag(PS)
            RPS <- cov2corMod(PS)
            VE <- findFactorTotalVar(BE, RPS, VPS)
        }
        if (is.null(ME)) 
            ME <- findFactorMean(BE, AL)
        if (is.null(AL)) 
            AL <- findFactorIntercept(BE, ME)
        
    } else if (modelType == "SEM") {
        if (is.null(PS)) {
            if (is.null(VPS)) 
                VPS <- findFactorResidualVar(BE, RPS, VE)
            if (is.null(VE)) 
                VE <- findFactorTotalVar(BE, RPS, VPS)
            PS <- suppressWarnings(cor2cov(RPS, suppressWarnings(sqrt(VPS))))
        } else {
            VPS <- diag(PS)
            RPS <- cov2corMod(PS)
            VE <- findFactorTotalVar(BE, RPS, VPS)
        }
        if (is.null(ME)) 
            ME <- findFactorMean(BE, AL)
        if (is.null(AL)) 
            AL <- findFactorIntercept(BE, ME)
        facCov <- findFactorTotalCov(BE, PS)
        if (is.null(TE)) {
            if (is.null(VTE)) 
                VTE <- findIndResidualVar(LY, facCov, VY)
            if (is.null(VY)) 
                VY <- findIndTotalVar(LY, facCov, VTE)
            TE <- suppressWarnings(cor2cov(RTE, suppressWarnings(sqrt(VTE))))
        } else {
            RTE <- cov2corMod(TE)
            VTE <- diag(TE)
            VY <- findIndTotalVar(LY, facCov, VTE)
        }
        if (is.null(MY)) 
            MY <- findIndMean(LY, ME, TY)
        if (is.null(TY)) 
            TY <- findIndIntercept(LY, ME, MY)
    }
    fullParamSet <- list(LY = LY, VTE = VTE, TE = TE, RTE = RTE, VY = VY, TY = TY, MY = MY,
               BE = BE, VPS = VPS, PS = PS, RPS = RPS, VE = VE, AL = AL, ME = ME)
    return(fullParamSet)
}

reduceMatrices <- function(paramSet) {
  require(lavaan)
 
  if (is.null(paramSet$PS)) 
    paramSet$PS <- suppressWarnings(cor2cov(paramSet$RPS, sqrt(paramSet$VPS)))
  if (paramSet$modelType == "CFA" | paramSet$modelType == "SEM" | paramSet$modelType == "SEM.exo") {
    if (isNullObject(paramSet$TE)) 
      paramSet$TE <- suppressWarnings(cor2cov(paramSet$RTE, sqrt(paramSet$VTE)))
  }  
  reducedParamSet <- list(PS = object$PS, BE = object$BE, AL = object$AL, TE = object$TE,
                LY = object$LY, TY = object$TY, PH = object$PH, GA = object$GA, KA = object$KA, 
                TD = object$TD, LX = object$LX, TX = object$TX, TH = object$TH, modelType = object$modelType)
  return(reducedParamSet)
}

createImpliedMACS <- function(reducedParamSet,modelType) {
    implied.mean <- NULL
    implied.covariance <- NULL
    ID <- matrix(0, nrow(reducedParamSet$PS), nrow(reducedParamSet$PS))
    diag(ID) <- 1
    if (modelType == "CFA") {
        implied.mean <- reducedParamSet$LY %*% reducedParamSet$AL + reducedParamSet$TY
        implied.covariance <- (reducedParamSet$LY %*% reducedParamSet$PS %*% t(reducedParamSet$LY)) + reducedParamSet$TE
    } else if (modelType == "Path" | modelType == "SEM") {
        implied.mean <- solve(ID - reducedParamSet$BE) %*% reducedParamSet$AL
        implied.covariance <- solve(ID - reducedParamSet$BE) %*% reducedParamSet$PS %*% t(solve(ID - reducedParamSet$BE))
        if (modelType == "SEM") {
            implied.mean <- reducedParamSet$TY + (reducedParamSet$LY %*% implied.mean)
            implied.covariance <- (reducedParamSet$LY %*% implied.covariance %*% t(reducedParamSet$LY)) + reducedParamSet$TE
        }
    } 
    return(list(M = as.vector(implied.mean), CM = implied.covariance))
}

popMisfit <- function(param, misspec, dfParam = NULL, fit.measures = "all") {
    paramCM <- NULL
    paramM <- NULL
    misspecCM <- NULL
    misspecM <- NULL
    if (is(param[[1]], "matrix")) {
        paramCM <- param[[1]]
        p <- nrow(paramCM)
        paramM <- rep(0, p)
        if (is(param[[2]], "vector")) {
            paramM <- param[[2]]
        }
    } else if (is(param[[2]], "matrix")) {
        paramCM <- param[[2]]
        if (is(param[[1]], "vector")) {
            paramM <- param[[1]]
        } else {
            stop("Cannot find the mean vector of the parameter values.")
        }
    } else {
        stop("Cannot find covariance matrix in the parameter values")
    }
    if (is(misspec[[1]], "matrix")) {
        misspecCM <- misspec[[1]]
        p <- nrow(misspecCM)
        misspecM <- rep(0, p)
        if (is(misspec[[2]], "vector")) {
            misspecM <- misspec[[2]]
        }
    } else if (is(misspec[[2]], "matrix")) {
        misspecCM <- misspec[[2]]
        if (is(param[[1]], "vector")) {
            misspecM <- misspec[[1]]
        } else {
            stop("Cannot find the mean vector of the misspecification values.")
        }
    } else {
        stop("Cannot find covariance matrix in the misspecification values")
    }
    result <- popMisfitMACS(paramM, paramCM, misspecM, misspecCM, dfParam = dfParam, fit.measures = fit.measures)
    return(result)
}

popMisfitMACS <- function(paramM, paramCM, misspecM, misspecCM, dfParam = NULL, fit.measures = "all") {
    if (fit.measures == "all") {
        fit.measures <- c("f0","rmsea","srmr")
        if (is.null(dfParam)) 
            fit.measures <- fit.measures[c(1, 3)]
    }
    p <- length(paramM)
    fit.measures <- tolower(fit.measures)
    f0 <- popDiscrepancy(paramM, paramCM, misspecM, misspecCM)
    if(!is.null(f0)) {
      
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
        disSquared <- (cov2cor(misspecCM) - cov2cor(paramCM))^2
        numerator <- 2 * sum(disSquared[lower.tri(disSquared, diag = TRUE)])
        srmr <- sqrt(numerator/(p * (p + 1)))
        result <- c(result, srmr)
      }
      result <- as.vector(result)
      names(result) <- fit.measures
    } else {result <- NULL}
    
    return(result)
}

# F0 in population: The discrepancy due to approximation (Browne & Cudeck, 1992)
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

## Takes one SimMatrix and returns a matrix with numerical values for population parameters.
## If constraint = TRUE, then constraints are applied simultaneously.
## if missAdd = TRUE, then misspecification is added.
## if mRaw = TRUE, then a list is returned with the raw misspecification separate
## if rawOnly = TRUE, then only the misspecification is returned
rawDraw <- function(simDat,constraint=TRUE,miss=TRUE,mRaw=TRUE, missOnly=FALSE) {
  if(class(simDat) == "SimMatrix" || class(simDat) == "SimVector") {
    free <- as.vector(simDat@free)
    popParam <- as.vector(simDat@popParam)
    misspec <- as.vector(simDat@misspec)
    rawDat <- missRaw <- suppressWarnings(as.numeric(as.vector(free)))
    if(missOnly) { missRaw <- rep(0,length(free)) } else {    missRaw[is.na(missRaw)] <- 0}
    
    
    if(constraint && miss) {
      conList <- NULL
      isLabel <- is.label(free)
      for(i in seq_along(free)) {
        if(isLabel[i]) {
          label <- free[i]
          if(is.null(conList[label]) || is.na(conList[label])) { #if label isn't in constraint list
            rawDat[i] <- eval(parse(text = popParam[i])) # draw
            conList <- c(conList,rawDat[i]) #Add value to constraint list          
            names(conList)[length(conList)] <- label # Add label to constraint list 
          } else { 
            rawDat[i] <- conList[label]
          }
        } else if(is.na(rawDat[i])) { # Is not a label, but is NA
            rawDat[i] <- eval(parse(text = popParam[i])) # normal draw
        } else if(miss==TRUE && !is.nan(misspec) && length(misspec) > 0){ # Is not free - check for misspecification
            missRaw[i] <- eval(parse(text=misspec[i])) # Combine the original parameter with the misspecification
            rawDat[i] <- rawDat[i] + missRaw[i]# else {}        
          }
      }
    } else if(miss){ # Don't apply constraints but apply misspecification
      for(i in seq_along(free)) {
        if(is.na(rawDat[i])) {
            rawDat[i] <- eval(parse(text = popParam[i]))
          } else if(miss==TRUE && !is.nan(misspec) && length(misspec) > 0){ # Is not free - check for misspecification
          missRaw[i] <- eval(parse(text=misspec[i])) # Combine the original parameter with the misspecification
          rawDat[i] <- rawDat[i] + missRaw[i] 
        }
      }
    } else { # Don't apply constraints or misspecification
      for(i in seq_along(free)) {
        if(is.na(rawDat[i])) {
            rawDat[i] <- eval(parse(text = popParam[i]))
          }
      }
    }
    
    if(class(simDat) == "SimMatrix") {
      rawDat <- matrix(rawDat,nrow=nrow(simDat@free),ncol=ncol(simDat@free))
      missRaw <- matrix(missRaw,nrow=nrow(simDat@free),ncol=ncol(simDat@free))
    }
    if(mRaw && miss && !missOnly) {
      return(list(rawDat,missRaw))
    } else if (missOnly) {
      return(missRaw)
    } else {
      return(rawDat)
    }
  } else { return(NULL) } # Was not a SimMatrix or SimVector
}
  

# cov2corMod: The cov2cor function that takes care of the zero-variance variables

cov2corMod <- function(V) {
    targetCol <- which(diag(V) != 0)
    V[targetCol, targetCol] <- cov2cor(V[targetCol, targetCol])
    return(V)
} 

