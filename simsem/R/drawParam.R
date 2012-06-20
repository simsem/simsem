## Arguments: misfitBound should be a vector with upper and lower bounds

## Takes a simsem template, and returns population parameter values for data generation.
## The format is a list with the following elements:
## [[1]] $param - Population parameter values for data generation
## [[2]] $paramMis - Population parameter values with misspecification
## [[3]] $mis - Misspecification only

## Possible misfitTypes are "f0", "rmsea", "srmr"
## drawParam <- function(model, maxDraw=20, misfitBounds=NULL, misfitType=NULL,averageNumMisspec=FALSE, optMisfit=NULL, numIter=1) {
##   modelType <- model@modelType
##   free <- max(model@pt$free)
##   param <- NULL
##   if("SimMatrix" %in% sapply(model@dgen,class)) { # Single group
##     param <- drawParameters(model@dgen, free, modelType, maxDraw=maxDraw, misfitBounds=misfitBounds,
##                    misfitType=misfitType, averageNumMisspec=averageNumMisspec, optMisfit=optMisfit, numIter=numIter)
##   } else { # multiple group
##     param <- lapply(model@dgen, drawParameters, free, modelType, maxDraw=maxDraw, misfitBounds=misfitBounds,
##                    misfitType=misfitType, averageNumMisspec=averageNumMisspec, optMisfit=optMisfit, numIter=numIter)
##   }

##   return(param)
## }

## Fuck it, let's push everything down
## drawParameters <- function(dgen,free, maxDraw=20, misfitBounds=NULL, misfitType=NULL,averageNumMisspec=FALSE, optMisfit=NULL, numIter=1) {

##   param <- NULL
##   misCheck <- any(sapply(dgen,FUN=function(x) {
##     if(!is.null(x) && length(x@misspec)!=0 && !any(is.nan(x@misspec)))
##       {TRUE} else {FALSE}  }))

##   implied.CM.param <- NULL
##   implied.CM.misspec <- NULL
##   misfit <- NULL
##   count <- 0
##   repeat {
##     if (misCheck) {
##       if(!is.null(optMisfit)) {
##         paramsMis <- drawSet(paramSet = dgen, misspec=misCheck, numFree=free, optMisfit=optMisfit, numIter=numIter, misfitType=misfitType)
##       } else {
##         paramsMis <- drawSet(paramSet = dgen, misspec=misCheck, numFree=free)
##       }
##       param <- paramsMis$param
##       misspec <- paramsMis$misParam
##       misOnly <- mapply("-",misspec,param)
##       if (validateObject(param)) {
##         param <- reduceMatrices(param)
##         misspec <- reduceMatrices(misspec)
##         misOnly <- reduceMatrices(misOnly)
##         if (!is.null(param) && !is.null(misspec)) {
##           implied.CM.param <- createImpliedMACS(param)
##           implied.CM.misspec <- createImpliedMACS(misspec)
##           if (all(is.finite(implied.CM.misspec$CM)) && (sum(eigen(implied.CM.misspec$CM)$values <= 0) == 0)) {
##             if (is.null(misfitBounds)) {
##               break
##             } else {
##               p <- length(implied.CM.param$M)
##               nElements <- p + (p * (p + 1)/2)
##               nFree <- free
##               dfParam <- nElements - nFree
##               misfit <- popMisfitMACS(implied.CM.param$M, implied.CM.param$CM, implied.CM.misspec$M, implied.CM.misspec$CM,
##                                       fit.measures = misfitType, dfParam = dfParam)
##               if (averageNumMisspec)
##                 misfit <- misfit/free
              
##               if (!is.null(misfit) && (misfit > misfitBounds[1] & misfit < misfitBounds[2])) 
##                 break
##             }
##           }
##         }
##       }
##     } else {
##       params <- drawSet(paramSet = dgen, misspec=FALSE, numFree=free)
##       misspec <- NULL
##       misOnly <- NULL
##       if (validateObject(params$param)) {
##         param <- reduceMatrices(params$param)
##         if (!is.null(param)) {
##           implied.CM.param <- createImpliedMACS(param)
          
##           if (sum(eigen(implied.CM.param$CM)$values <= 0) == 0) 
##             break
##         }
##       }
##     }
##     count <- count + 1
##     if (count > maxDraw) 
##       stop("The model cannot make a good set of parameters within limit of maximum random sampling of parameters")
##   }
##   list(param=param,misParam=misspec,mis=misOnly)
## }

## Makes one draw of population parameters, but may make more draws of
## misspecification params if optMisfit is specified, can be min or max.
## Output:
## [[1]] - param (list of matrices with parameter values)
## [[2]] - paramMis (param + misspec)
## [[3]] - mis (misspec only)

drawParam <- function(paramSet, maxDraw, numFree, misspec, misfitBounds=NULL, averageNumMisspec=FALSE, optMisfit = NULL, numIter=1, misfitType="f0") {
  if(!is.list(paramSet[[1]])) {
    paramSet <- list(paramSet)
  }
  
  draw <- 1
  while(draw < maxDraw) {  
   
    ngroups <- length(paramSet)
    groupLoop <- seq_len(ngroups)
    
    if(misspec) {
      
      rpls <- list() # list of drawn parameter sets (raw)
      fullpls <- list() # list of drawn parameter sets (filled)
      fullmpls <- list() # list of drawn parameter sets with misspec (filled)
      fullmls <- list() # list of parameter sets with misspec only (filled)
      
      for(i in groupLoop) {
        rpls[[i]] <- lapply(paramSet[[i]],rawDraw)
        fullpls[[i]] <- fillParam(lapply(rpls[[i]],"[[",1))
        fullmpls[[i]] <- fillParam(lapply(rpls[[i]],"[[",2))
        fullmls[[i]] <- mapply("-",fullpls[[i]],fullmpls[[i]])
      }
             
      if(!is.null(optMisfit)) { #  if misfit needs to be optimized
        
        for(i in groupLoop) {
          rpls[[i]] <- lapply(paramSet[[i]],rawDraw,misSpec=FALSE)
        }
        rplsfill <- lapply(rpls,fillParam) # fill after drawing misspec
        macsPopls <- lapply(rplsfill,createImpliedMACS)
        
        if(!(all(is.finite(unlist(lapply(macsPopls,"[[",2)))) &&
             (sum(unlist(lapply(lapply(lapply(macsPopls,"[[",2),eigen),"[[",1)) <= 0) == 0) &&
             all(sapply(rplsfill,validateObject)))) {
          ## Checks:
          ## 1. all covariances are finite
          ## 2. Eigenvalues are less than zero
          ## 3. Paths and variances are valid
          
          next
          draw <- draw+1
          # because pop matrix is invalid, move on to next draw
        }
        
        mls <- list() # list of misspec draws numIterations by groups. length(mls) = numIter, length(mls[[1]]) = numgroups
        macsMisls <- list() # list of misspec macs, numIterations by groups
               
        for (i in seq_len(numIter)) {
          misspecDraws <- list()
          macsMis <- list()
          temp <- list()
          for (j in groupLoop) {
            temp <- lapply(paramSet[[j]],rawDraw,misOnly=TRUE)
            addMis <- mapply("+",rpls[[j]],temp)
            misspecDraws[[j]] <- fillParam(lapply(addMis, FUN=function(x) {if(length(x) == 0) { x <- NULL } else x }))
            tempMacs <- createImpliedMACS(misspecDraws[[j]])
            if (all(is.finite(tempMacs$CM)) && (sum(eigen(tempMacs$CM)$values <= 0) == 0)) {
              macsMis[[j]] <- tempMacs
            } else { macsMis[[j]] <- NULL }
          }          
          mls[[i]] <- misspecDraws
          macsMisls[[i]] <- macsMis
        }
        
        rpls <- rplsfill
     
        p <- length(macsPopls[[1]]$M)
        nElements <- (p + (p*(p+1)/2))*ngroups
        dfParam <- nElements - numFree

        misfit <- matrix(0,numIter,4) # need to change for srmr
        colnames(misfit) <- c("f0","rmsea","srmr","iter")
        
        ## For each iteration, send ith draw for each group to calculate the population discrepancy, misspec vs. nonmisspec params.
        
        for(i in seq_len(numIter)) {
          paramM <- lapply(macsPopls,FUN="[[",1)
          paramCM <- lapply(macsPopls,FUN="[[",2)
          misspecM <- lapply(macsMisls[[i]],FUN="[[",1)
          misspecCM <- lapply(macsMisls[[i]],FUN="[[",2)
          if(is.null(misspecM) || is.null(misspecCM) ) {
            misfit[i,] <- rep(NULL,3)
          } else {
            misfit[i,] <- c(popMisfitMACS(paramM,paramCM,misspecM,misspecCM,dfParam,fit.measures="all"),i)
          }
        }

        if(optMisfit == "min") {
          misfit <- misfit[sort.list(misfit[,1]),]
          iter <- misfit[1,4]
        } else if(optMisfit == "max") {
          misfit <- misfit[sort.list(misfit[,1],decreasing=TRUE),]
          iter <- misfit[1,4]
        }
        
        finalPopParam <- rpls
        finalMisParam <- mls[[iter]]
        return(list(param=rpls,misspec=mls[[iter]]))
      } # End optimization, return control to normal misspec checks / draws
      
      if (all(sapply(fullpls,validateObject))) {
        redpls <- lapply(fullpls,reduceMatrices)
        redmpls <- lapply(fullmpls,reduceMatrices)
        redmls <- lapply(fullmls,reduceMatrices)
        
        #if (!is.null(param) && !is.null(misspec)) {
        macsPopls <- lapply(redpls,createImpliedMACS)
        macsMisls <- lapply(redmpls, createImpliedMACS)

        ## Checks:
        ## 1. all covariances are finite
        ## 2. Eigenvalues are less than zero
        ## 3. Paths and variances are valid
        if(!(all(is.finite(unlist(lapply(macsPopls,"[[",2)))) &&
             (sum(unlist(lapply(lapply(lapply(macsPopls,"[[",2),eigen),"[[",1)) <= 0) == 0) &&
             all(is.finite(unlist(lapply(macsMisls,"[[",2)))) &&
             (sum(unlist(lapply(lapply(lapply(macsMisls,"[[",2),eigen),"[[",1)) <=0) == 0) )) {
          cat("FAIL")
          draw <- draw + 1
          next
          ## because pop matrix is invalid or pop+misspec, move on to next draw
        }
          
        if (is.null(misfitBounds)) {
          break
        } else {
          p <- length(macsPopls[[1]]$M)
          nElements <- (p + (p * (p + 1)/2))*ngroups
          nFree <- free
          dfParam <- nElements - nFree
          misfit <- popMisfitMACS(paramM = lapply(macsPopls,"[[",1),
                                  paramCM = lapply(macsPopls,"[[",2),
                                  misspecM = lapply(macsMisls, "[[",1),
                                  misspecCM = lapply(macsMisls,"[[",2),
                                  fit.measures = misfitType, dfParam = dfParam)
          if (averageNumMisspec)
            misfit <- misfit/free
          
          if (!is.null(misfit) && (misfit > misfitBounds[1] & misfit < misfitBounds[2])) 
                break
        }
      } else {  # one or more parameter sets in fullpls is not valid
        draw <- draw + 1
        next
      }
      
    } else { # no misspecification present
      
      fullpls <- list() # list of drawn parameter sets (filled)
       for(i in groupLoop) {
        fullpls[[i]] <- fillParam(lapply(paramSet[[i]],rawDraw,misSpec=FALSE))
      }
      if(all(sapply(fullpls,validateObject))) {
        redpls <- lapply(fullpls,reduceMatrices)
        macsPopls <- lapply(redpls,createImpliedMACS)
        if(!(all(is.finite(unlist(lapply(macsPopls,"[[",2)))) &&
           (sum(unlist(lapply(lapply(lapply(macsPopls,"[[",2),eigen),"[[",1)) <= 0) == 0))) {
          next
        }
      } else {
        draw <- draw + 1
        next
      }
      return(param = redpls,misspec=NULL,misOnly=NULL)
    }
    
  }
  if( draw < maxDraw) {
    return(list(param = redpls, misspec = redmpls, misOnly = redmls))
  } else {
    stop("Cannot make a good set of parameters within the maximum number of draws")
  }
}
  
    ## if(misspec) {    
##       if (optMisfit == "none") {
##         rawParam <- lapply(paramSet,rawDraw)
##         fullParams <- fillParam(lapply(rawParam,"[[",1))
##         fullMiss <- fillParam(lapply(rawParam,"[[",2))
##       } else {
##         rawParam <- lapply(paramSet,rawDraw,misSpec=FALSE)
##         missParam <- list()
##         for(i in 1:numIter) {
##           temp <- lapply(paramSet,rawDraw,misOnly=TRUE)
##           addMis <- mapply("+",rawParam,temp)  # We only want to draw the raw params once, so we add the rawParameter values to each misspecified draw
##           missParam[[i]] <- lapply(addMis, FUN=function(x) {if(length(x) == 0) {x <- NULL} else x}) # Make empty matrices NULL instead of numeric(0)
##         }
##         fullParams <- fillParam(rawParam)
##         fullMiss <- list()
##         for(i in 1:length(missParam)) {
##           fullMiss[[i]] <- fillParam(missParam[[i]])
##         }
        
##       }
      
##       ## fullParams = Constraints / Misspecification -> Auto completition of parameters -> 1 Full Parameter Set
##       ## fullMiss = Constraints / Misspecification -> Auto completion of parameters -> Misspecification only, list if optMisfit = TRUE

##       if (class(fullMiss[[1]]) == "list") {
##         macsMis <- lapply(fullMiss, createImpliedMACS)
##         macsPop <- createImpliedMACS(fullParams)
##         p <- length(macsPop$M)
##         nElements <- p + (p * (p + 1)/2)

##         dfParam <- nElements - numFree
##         misfit <- sapply(macsMis, popMisfit, param = macsPop, dfParam = dfParam, fit.measures = misfitType)

##         element <- NULL
##         fit.measures <- c("f0","rmsea","srmr")
        
##         if (optMisfit == "min") {
##           element <- which.min(sapply(misfit,"[[",1))
##         } else if (optMisfit == "max") {
##           element <- which.max(sapply(misfit,"[[",1))
##         } else {
##           stop("optMisfit must be either \"max\" or \"min\"")
##         }
##         finalMis <- fullMiss[[element]]
##         ## Mis <- Mis[[element]]
##       } else {
##         finalMis <- fullMiss
##         ##  Mis <- Mis[[1]]
##       }
##       return(list(param = fullParams, misParam = finalMis))
##     } else {
##       return(list(param = fillParam(lapply(paramSet,rawDraw,misSpec=FALSE)), misspec=NULL))
##     }
  
## }                                       

## Takes one SimMatrix and returns a matrix with numerical values for population parameters.
## If constraint = TRUE, then constraints are applied simultaneously.
## if misSpec = TRUE, then a list is returned with [[1]] parameters with no misspec and [[2]] same parameters + misspec (if any)
## if missOnly = TRUE, then only the parameters + misspecification is returned
## If a matrix is symmetric, it is arbitrarily chosen that parameters on the upper tri
## are set equal to the parameters on the lower tri.
rawDraw <- function(simDat,constraint=TRUE,misSpec=TRUE,parMisOnly=FALSE,misOnly=FALSE) {
  if(class(simDat) == "SimMatrix" || class(simDat) == "SimVector") {
    free <- as.vector(simDat@free)
    popParam <- as.vector(simDat@popParam)
    misspec <- as.vector(simDat@misspec)

    ## param will contain population parameters, fixed and freed only.
    ## paramMis will contain param + any misspecification
    ## missRaw will contain ONLY the misspecification
    param <- paramMis <- suppressWarnings(as.numeric(as.vector(free)))
    ## param & paramMis now contain any fixed numeric values in free
    missRaw <- rep(0,length(free))
    
    
    if(constraint && misSpec) {
      conList <- NULL
      isLabel <- is.label(free)
      for(i in seq_along(free)) {
        ## Options: Label (constraint), or Free
        if(isLabel[i]) {
          label <- free[i]
          if(is.null(conList[label]) || is.na(conList[label])) { #if label isn't in constraint list
            param[i] <- paramMis[i] <- eval(parse(text = popParam[i])) # draw
            conList <- c(conList,param[i]) #Add value to constraint list          
            names(conList)[length(conList)] <- label # Add label to constraint list 
          } else { 
            param[i] <- paramMis[i] <- conList[label]
          }
        } else if(is.na(param[i])) { # Is not a label, but is NA
            param[i] <- paramMis[i] <- eval(parse(text = popParam[i])) # normal draw
          }
        # Any entry (fixed or freed) can optionally have misspecification, even free random parameters.
        if(!is.nan(misspec) && length(misspec) > 0 && !is.empty(misspec[i])){
            missRaw[i] <- eval(parse(text=misspec[i]))
            paramMis[i] <- param[i] + missRaw[i]
          }         
      }
    } else if(misSpec){ # Don't apply constraints but apply misspecification
      for(i in seq_along(free)) {
        if(is.na(param[i])) {
            param[i] <- paramMis[i] <- eval(parse(text = popParam[i]))
          }
        # Again, any entry can optionally have misspecification.
        if(!is.nan(misspec) && length(misspec) > 0 && !is.empty(misspec[i])){ # Is not free - check for misspecification            
            missRaw[i] <- eval(parse(text=misspec[i]))
            paramMis[i] <- param[i] + missRaw[i]
        }
      }
    } else { # Don't apply constraints or misspecification
      for(i in seq_along(free)) {
        if(is.na(param[i])) {
            param[i] <- eval(parse(text = popParam[i]))
          }
      }
    }   
    if(class(simDat) == "SimMatrix") {
        param <- matrix(param,nrow=nrow(simDat@free),ncol=ncol(simDat@free))
        paramMis <- matrix(paramMis,nrow=nrow(simDat@free),ncol=ncol(simDat@free))
        missRaw <- matrix(missRaw,nrow=nrow(simDat@free),ncol=ncol(simDat@free))
        if(simDat@symmetric) {
          param[upper.tri(param)] <- t(param)[upper.tri(param)]
          paramMis[upper.tri(paramMis)] <- t(paramMis)[upper.tri(paramMis)]
          missRaw[upper.tri(missRaw)] <- t(missRaw)[upper.tri(missRaw)]
        }
    }
    # This is not the full set of options.
    if(misSpec && !parMisOnly && !misOnly) {
      return(list(param=param,paramMis=paramMis))
    } else if (parMisOnly) {
      return(paramMis)
    } else if(misOnly) {
      return(missRaw)
    } else {
      return(param=param)
    }
  } else { return(NULL) } # Was not a SimMatrix or SimVector
}


# Auto-completition of parameters
fillParam <- function(rawParamSet) {
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

    if (is.null(BE) && !is.null(LY)) { # CFA
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
        
    } else if (!is.null(BE) && is.null(LY)) { # Path
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
        
    } else if (!is.null(BE) && !is.null(LY)) { # SEM
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

# Reduce RPS/RTE to PS/TE if present.
reduceMatrices <- function(paramSet) {
  require(lavaan)
 
  if (is.null(paramSet$PS)) 
    paramSet$PS <- suppressWarnings(cor2cov(paramSet$RPS, sqrt(paramSet$VPS)))

  if (is.null(paramSet$TE)) {
      paramSet$TE <- suppressWarnings(cor2cov(paramSet$RTE, sqrt(paramSet$VTE)))
  }
  
  reducedParamSet <- list(PS = paramSet$PS, BE = paramSet$BE, AL = paramSet$AL, TE = paramSet$TE,
                LY = paramSet$LY, TY = paramSet$TY)
  return(reducedParamSet)
}

createImpliedMACS <- function(reducedParamSet) {
    implied.mean <- NULL
    implied.covariance <- NULL
    ID <- matrix(0, nrow(reducedParamSet$PS), nrow(reducedParamSet$PS))
    diag(ID) <- 1
    if (!is.null(paramSet$BE)) { # Path or SEM
      implied.mean <- solve(ID - reducedParamSet$BE) %*% reducedParamSet$AL
      implied.covariance <- solve(ID - reducedParamSet$BE) %*% reducedParamSet$PS %*% t(solve(ID - reducedParamSet$BE))
      if (!is.null(paramSet$LY)) { # SEM
        implied.mean <- reducedParamSet$TY + (reducedParamSet$LY %*% implied.mean)
        implied.covariance <- (reducedParamSet$LY %*% implied.covariance %*% t(reducedParamSet$LY)) + reducedParamSet$TE
      }
      
    } else { # CFA
      implied.mean <- reducedParamSet$LY %*% reducedParamSet$AL + reducedParamSet$TY
      implied.covariance <- (reducedParamSet$LY %*% reducedParamSet$PS %*% t(reducedParamSet$LY)) + reducedParamSet$TE
    } 
    return(list(M = as.vector(implied.mean), CM = implied.covariance))
}

    ## Doesn't really do anything? just finds the mean and covariance matrices from param and misspec. Weird.
## popMisfit <- function(param, misspec, dfParam = NULL, fit.measures = "all", mg=FALSE) {
##     paramCM <- NULL
##     paramM <- NULL
##     misspecCM <- NULL
##     misspecM <- NULL
##     if (is(param[[1]], "matrix")) {
##         paramCM <- param[[1]]
##         p <- nrow(paramCM)
##         paramM <- rep(0, p)
##         if (is(param[[2]], "vector")) {
##             paramM <- param[[2]]
##         }
##     } else if (is(param[[2]], "matrix")) {
##         paramCM <- param[[2]]
##         if (is(param[[1]], "vector")) {
##             paramM <- param[[1]]
##         } else {
##             stop("Cannot find the mean vector of the parameter values.")
##         }
##     } else {
##         stop("Cannot find covariance matrix in the parameter values")
##     }
##     if (is(misspec[[1]], "matrix")) {
##         misspecCM <- misspec[[1]]
##         p <- nrow(misspecCM)
##         misspecM <- rep(0, p)
##         if (is(misspec[[2]], "vector")) {
##             misspecM <- misspec[[2]]
##         }
##     } else if (is(misspec[[2]], "matrix")) {
##         misspecCM <- misspec[[2]]
##         if (is(param[[1]], "vector")) {
##             misspecM <- misspec[[1]]
##         } else {
##             stop("Cannot find the mean vector of the misspecification values.")
##         }
##     } else {
##         stop("Cannot find covariance matrix in the misspecification values")
##     }
##     result <- popMisfitMACS(paramM, paramCM, misspecM, misspecCM, dfParam = dfParam, fit.measures = fit.measures)
##     return(result)
## }


## Now takes lists for the matrices for mg
popMisfitMACS <- function(paramM, paramCM, misspecM, misspecCM, dfParam = NULL, fit.measures = "all") {
    if (fit.measures == "all") {
        fit.measures <- c("f0","rmsea","srmr")
        #fit.measures <- c("f0","rmsea")
        if (is.null(dfParam)) 
            fit.measures <- fit.measures[c(1, 3)]
    }
    p <- length(paramM)
    fit.measures <- tolower(fit.measures)

    # If multiple group, f0 is added for each group
    if(is.list(paramM) && is.list(paramCM) && is.list(misspecM) && is.list(misspecCM)) { 
      tempf0 <- mapply(FUN=popDiscrepancy,paramM, paramCM, misspecM, misspecCM)
      f0 <- sum(tempf0)
    } else {
      f0 <- popDiscrepancy(paramM, paramCM, misspecM, misspecCM)
    }
    
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
        disSquared <- mapply(function(x,y){(cov2cor(x) - cov2cor(y))^2},x=misspecCM,y=paramCM)
        sumDis <-  sapply(disSquared, function(x) { sum(x[lower.tri(x,diag=TRUE)]) })
        nElements <- length(sumDis)*(p*(p+1))/2
        numerator <- sum(sumDis)
        srmr <- sqrt(numerator/nElements)
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

# cov2corMod: The cov2cor function that takes care of the zero-variance variables

cov2corMod <- function(V) {
    targetCol <- which(diag(V) != 0)
    if(!length(targetCol)==0) {
      V[targetCol, targetCol] <- cov2cor(V[targetCol, targetCol])
    }
    return(V)
} 

