source("AllClass.R")
source("bind.R")
source("model.R")
source("find.R")

# defaults?

drawParam <- function(model, conBeforeMis, misBeforeFill, misfitType, misfitBounds, averageNumMisspec, optMisfit, numIter) {
  paramSet <- model@dgen
  param <- NULL
  misspec <- NULL
  misspecAdd <- NULL
  implied.CM.param <- NULL
  implied.CM.misspec <- NULL
  misfit <- NULL
  count <- 0
  repeat {
    if (!isNullObject(objMisspec)) {
      Output <- runMisspec(objSet, objMisspec, objEqualCon)
      param <- Output$param
      misspec <- Output$misspec
      misspecAdd <- Output$misspecAdd
      if (validateObject(param) | validateObject(misspec)) {
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

# Take character population generation matrices, and return a list the same matrices with only numerical values. These are the raw (pre-processed) population parameters.
rawDrawSet <- function(paramSet, conBeforeFill, makeList = FALSE) {
  out <- NULL
  rawParam <- lapply(paramSet,rawDraw,constraint=TRUE)
    if ( # constraints exist) {
        if (object@modelType != equalCon@modelType) 
            stop("Please provide same tags of SimSet and constraint")
        if (equalCon@conBeforeFill) {
            param <- constrainMatrices(param, equalCon)
            out <- fillParam(param, object@modelType)
        } else {
            param <- fillParam(param, object@modelType)
            param <- constrainMatrices(param, equalCon)
            out <- fillParam(param, object@modelType)
        }
    } else {
        out <- fillParam(param, object@modelType)
    }
    if (makeList) {
        return(list(out, param))
    } else {
        return(out)
    }
}

rawDraw <- function(simDat,constraint=TRUE) {
  if(class(simDat) == "SimMatrix" || class(simDat) == "SimVector") {
    free <- as.vector(simDat@free)
    popParam <- as.vector(simDat@popParam)
    rawDat <- suppressWarnings(as.numeric(as.vector(simDat@free)))
    
    if(constraint) {
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
          rawDat[i] <- eval(parse(text = popParam[i])) # draw
        } else { }  # Is not a label or is not NA. Do nothing.
      }
    } else { # Don't apply constraints
      for(i in seq_along(free)) {
        if(is.na(rawDat[i])) {
          rawDat[i] <- eval(parse(text = popParam[i]))
        }
      }
    }
    if(class(simDat) == "SimMatrix") {
      return(matrix(rawDat,nrow=nrow(simDat@free),ncol=ncol(simDat@free)))
    } else {
      return(rawDat)
    }         
  } else { return(NULL) } # Was not a SimMatrix or SimVector
}



fillParam <- function(paramSet, modelType) {
    library(lavaan)
    LY <- param@LY
    VTE <- param@VTE
    TE <- param@TE
    RTE <- param@RTE
    VY <- param@VY
    TY <- param@TY
    MY <- param@MY
    BE <- param@BE
    VPS <- param@VPS
    PS <- param@PS
    RPS <- param@RPS
    VE <- param@VE
    AL <- param@AL
    ME <- param@ME
    LX <- param@LX
    VTD <- param@VTD
    TD <- param@TD
    RTD <- param@RTD
    VX <- param@VX
    TX <- param@TX
    MX <- param@MX
    GA <- param@GA
    VPH <- param@VPH
    PH <- param@PH
    RPH <- param@RPH
    KA <- param@KA
    TH <- param@TH
    RTH <- param@RTH
    if (modelType == "CFA") {
        if (isNullObject(PS)) {
            PS <- suppressWarnings(cor2cov(RPS, sqrt(VE)))
        } else {
            VE <- diag(PS)
            VPS <- diag(PS)
            RPS <- cov2corMod(PS)
        }
        if (isNullObject(TE)) {
            if (isNullObject(VTE)) 
                VTE <- findIndResidualVar(LY, PS, VY)  # PS is model-implied covariance
            if (isNullObject(VY)) 
                VY <- findIndTotalVar(LY, PS, VTE)
            TE <- suppressWarnings(cor2cov(RTE, suppressWarnings(sqrt(VTE))))
        } else {
            VTE <- diag(TE)
            RTE <- cov2corMod(TE)
            VY <- findIndTotalVar(LY, PS, VTE)
        }
        if (isNullObject(MY)) 
            MY <- findIndMean(LY, ME, TY)
        if (isNullObject(TY)) 
            TY <- findIndIntercept(LY, ME, MY)
    } else if (modelType == "Path") {
        if (isNullObject(PS)) {
            if (isNullObject(VPS)) 
                VPS <- findFactorResidualVar(BE, RPS, VE)
            if (isNullObject(VE)) 
                VE <- findFactorTotalVar(BE, RPS, VPS)
            PS <- suppressWarnings(cor2cov(RPS, suppressWarnings(sqrt(VPS))))
        } else {
            VPS <- diag(PS)
            RPS <- cov2corMod(PS)
            VE <- findFactorTotalVar(BE, RPS, VPS)
        }
        if (isNullObject(ME)) 
            ME <- findFactorMean(BE, AL)
        if (isNullObject(AL)) 
            AL <- findFactorIntercept(BE, ME)
    } else if (modelType == "Path.exo") {
        nx <- ncol(GA)
        ny <- nrow(GA)
        if (!isNullObject(PS)) {
            VPS <- diag(PS)
            RPS <- cov2corMod(PS)
        }
        if (!isNullObject(PH)) {
            VPH <- diag(PH)
            RPH <- cov2corMod(PH)
        }
        temp.BE <- combinePathExoEndo(GA, BE)
        temp.RPS <- combineLatentCorExoEndo(RPH, RPS)
        if (isNullObject(VPS)) {
            temp.VPS <- findFactorResidualVar(temp.BE, temp.RPS, c(VPH, VE))
            VPS <- temp.VPS[(nx + 1):(nx + ny)]
        }
        if (isNullObject(VE)) {
            temp.VE <- findFactorTotalVar(temp.BE, temp.RPS, c(VPH, VPS))
            VE <- temp.VE[(nx + 1):(nx + ny)]
        }
        if (isNullObject(ME)) {
            temp.ME <- findFactorMean(temp.BE, c(KA, AL))
            ME <- temp.ME[(nx + 1):(nx + ny)]
        }
        if (isNullObject(AL)) {
            temp.AL <- findFactorIntercept(temp.BE, c(KA, ME))
            AL <- temp.AL[(nx + 1):(nx + ny)]
        }
        if (isNullObject(PS)) 
            PS <- suppressWarnings(cor2cov(RPS, suppressWarnings(sqrt(VPS))))
        if (isNullObject(PH)) 
            PH <- suppressWarnings(cor2cov(RPH, suppressWarnings(sqrt(VPH))))
    } else if (modelType == "SEM") {
        if (isNullObject(PS)) {
            if (isNullObject(VPS)) 
                VPS <- findFactorResidualVar(BE, RPS, VE)
            if (isNullObject(VE)) 
                VE <- findFactorTotalVar(BE, RPS, VPS)
            PS <- suppressWarnings(cor2cov(RPS, suppressWarnings(sqrt(VPS))))
        } else {
            VPS <- diag(PS)
            RPS <- cov2corMod(PS)
            VE <- findFactorTotalVar(BE, RPS, VPS)
        }
        if (isNullObject(ME)) 
            ME <- findFactorMean(BE, AL)
        if (isNullObject(AL)) 
            AL <- findFactorIntercept(BE, ME)
        facCov <- findFactorTotalCov(BE, PS)
        if (isNullObject(TE)) {
            if (isNullObject(VTE)) 
                VTE <- findIndResidualVar(LY, facCov, VY)
            if (isNullObject(VY)) 
                VY <- findIndTotalVar(LY, facCov, VTE)
            TE <- suppressWarnings(cor2cov(RTE, suppressWarnings(sqrt(VTE))))
        } else {
            RTE <- cov2corMod(TE)
            VTE <- diag(TE)
            VY <- findIndTotalVar(LY, facCov, VTE)
        }
        if (isNullObject(MY)) 
            MY <- findIndMean(LY, ME, TY)
        if (isNullObject(TY)) 
            TY <- findIndIntercept(LY, ME, MY)
    } else if (modelType == "SEM.exo") {
        nk <- ncol(GA)
        ne <- nrow(GA)
        if (!isNullObject(PS)) {
            VPS <- diag(PS)
            RPS <- cov2corMod(PS)
        }
        if (!isNullObject(PH)) {
            VPH <- diag(PH)
            RPH <- cov2corMod(PH)
        }
        if (!isNullObject(TE)) {
            VTE <- diag(TE)
            RTE <- cov2corMod(TE)
        }
        if (!isNullObject(TD)) {
            VTD <- diag(TD)
            RTD <- cov2corMod(TD)
        }
        temp.BE <- combinePathExoEndo(GA, BE)
        temp.RPS <- combineLatentCorExoEndo(RPH, RPS)
        if (isNullObject(VPS)) {
            temp.VPS <- findFactorResidualVar(temp.BE, temp.RPS, c(VPH, VE))
            VPS <- temp.VPS[(nk + 1):(nk + ne)]
        }
        if (isNullObject(VE)) {
            temp.VE <- findFactorTotalVar(temp.BE, temp.RPS, c(VPH, VPS))
            VE <- temp.VE[(nk + 1):(nk + ne)]
        }
        if (isNullObject(ME)) {
            temp.ME <- findFactorMean(temp.BE, c(KA, AL))
            ME <- temp.ME[(nk + 1):(nk + ne)]
        }
        if (isNullObject(AL)) {
            temp.AL <- findFactorIntercept(temp.BE, c(KA, ME))
            AL <- temp.AL[(nk + 1):(nk + ne)]
        }
        if (isNullObject(PS)) 
            PS <- suppressWarnings(cor2cov(RPS, suppressWarnings(sqrt(VPS))))
        if (isNullObject(PH)) 
            PH <- suppressWarnings(cor2cov(RPH, suppressWarnings(sqrt(VPH))))
        nk <- nrow(PH)
        ne <- nrow(PS)
        facCov <- findFactorTotalCov(combinePathExoEndo(GA, BE), combineLatentCorExoEndo(PH, PS))
        if (isNullObject(VTE)) 
            VTE <- findIndResidualVar(LY, facCov[(nk + 1):(nk + ne), (nk + 1):(nk + ne)], VY)
        if (isNullObject(VY)) 
            VY <- findIndTotalVar(LY, facCov[(nk + 1):(nk + ne), (nk + 1):(nk + ne)], VTE)
        if (isNullObject(MY)) 
            MY <- findIndMean(LY, ME, TY)
        if (isNullObject(TY)) 
            TY <- findIndIntercept(LY, ME, MY)
        if (isNullObject(VTD)) 
            VTD <- findIndResidualVar(LX, facCov[1:nk, 1:nk], VX)
        if (isNullObject(VX)) 
            VX <- findIndTotalVar(LX, facCov[1:nk, 1:nk], VTD)
        if (isNullObject(MX)) 
            MX <- findIndMean(LX, KA, TX)
        if (isNullObject(TX)) 
            TX <- findIndIntercept(LX, KA, MX)
        if (isNullObject(TE)) 
            TE <- suppressWarnings(cor2cov(RTE, suppressWarnings(sqrt(VTE))))
        if (isNullObject(TD)) 
            TD <- suppressWarnings(cor2cov(RTD, suppressWarnings(sqrt(VTD))))
        if (isNullObject(TH)) {
            TH <- suppressWarnings(sqrt(diag(VTD)) %*% RTH %*% sqrt(diag(VTE)))
        } else {
            RTH <- suppressWarnings(solve(sqrt(diag(VTD))) %*% TH %*% solve(sqrt(diag(VTE))))
        }
    }
    out <- new("MatrixSet", modelType = modelType, LY = LY, VTE = VTE, TE = TE, RTE = RTE, VY = VY, TY = TY, MY = MY, BE = BE, VPS = VPS, PS = PS, RPS = RPS, VE = VE, AL = AL, ME = ME, LX = LX, VTD = VTD, 
        TD = TD, RTD = RTD, VX = VX, TX = TX, MX = MX, GA = GA, VPH = VPH, PH = PH, RPH = RPH, KA = KA, TH = TH, RTH = RTH)
    return(out)
}

runMisspec <- function(object, misspec, SimEqualCon = new("NullSimEqualCon")) {
    Output1 <- NULL
    Output2 <- NULL
    # Should create the misspec here: then compare with the MACS and see what is going on!
    if (misspec@optMisfit == "none") {
        Mis <- list(run(misspec))
    } else {
        Mis <- lapply(1:misspec@numIter, function(obj, m) run(m), m = misspec)
    }
    if (isNullObject(SimEqualCon)) {
        if (misspec@misBeforeFill) {
            paramSet <- run(object, makeList = TRUE)
            Output1 <- paramSet[[1]]
            param <- lapply(Mis, combineObject, object1 = paramSet[[2]])
            Output2 <- lapply(param, fillParam, modelType = object@modelType)
        } else {
            paramSet <- run(object, makeList = TRUE)
            Output1 <- paramSet[[1]]
            param <- lapply(Mis, combineObject, object1 = paramSet[[1]])
            Output2 <- lapply(param, fillParam, modelType = object@modelType)
        }
    } else {
        if (object@modelType != SimEqualCon@modelType) 
            stop("Please provide same tags of SimSet and constraint")
        if (misspec@misBeforeFill & misspec@conBeforeMis & SimEqualCon@conBeforeFill) {
            # 2) Con, AddMis, fill
            paramSet <- run(object, SimEqualCon, makeList = TRUE)
            Output1 <- paramSet[[1]]
            param <- lapply(Mis, combineObject, object1 = paramSet[[2]])
            Output2 <- lapply(param, fillParam, modelType = object@modelType)
        } else if (!misspec@misBeforeFill & misspec@conBeforeMis & SimEqualCon@conBeforeFill) {
            # 1) Con, fillBefore, AddMis
            paramSet <- run(object, SimEqualCon, makeList = TRUE)
            Output1 <- paramSet[[1]]
            param <- lapply(Mis, combineObject, object1 = paramSet[[1]])
            Output2 <- lapply(param, fillParam, modelType = object@modelType)
        } else if (!misspec@misBeforeFill & misspec@conBeforeMis & !SimEqualCon@conBeforeFill) {
            # 3) fillBefore, Con, AddMis misspec@misBeforeFill=F & misspec@conBeforeMis=T SimEqualCon@conBeforeFill=F; FTF
            paramSet <- run(object, SimEqualCon, makeList = TRUE)
            Output1 <- paramSet[[1]]
            param <- lapply(Mis, combineObject, object1 = paramSet[[2]])
            Output2 <- lapply(param, fillParam, modelType = object@modelType)
        } else if (!misspec@misBeforeFill & !misspec@conBeforeMis & !SimEqualCon@conBeforeFill) {
            # 4) fillBefore, AddMis, Con isBeforeFill=F & misspec@conBeforeMis=F SimEqualCon@conBeforeFill=F; FFF
            paramSet <- run(object, makeList = TRUE)
            Output1 <- paramSet[[1]]
            param <- lapply(Mis, combineObject, object1 = paramSet[[1]])
            param <- lapply(param, constrainMatrices, SimEqualCon = SimEqualCon)
            Output2 <- lapply(param, fillParam, modelType = object@modelType)
        } else if (misspec@misBeforeFill & !misspec@conBeforeMis & SimEqualCon@conBeforeFill) {
            # 5) AddMis, Con, fill misspec@misBeforeFill=T & misspec@conBeforeMis=F SimEqualCon@conBeforeFill=T; TFT
            paramSet <- run(object, makeList = TRUE)
            Output1 <- paramSet[[1]]
            param <- lapply(Mis, combineObject, object1 = paramSet[[2]])
            param <- lapply(param, constrainMatrices, SimEqualCon = SimEqualCon)
            Output2 <- lapply(param, fillParam, modelType = object@modelType)
        } else if (misspec@misBeforeFill & !misspec@conBeforeMis & !SimEqualCon@conBeforeFill) {
            # 6) AddMis, fill, Con misspec@misBeforeFill=T & misspec@conBeforeMis=F SimEqualCon@conBeforeFill=F; TFF ; No FFT and TTF
            paramSet <- run(object, makeList = TRUE)
            Output1 <- paramSet[[1]]
            param <- lapply(Mis, combineObject, object1 = paramSet[[2]])
            param <- lapply(param, fillParam, modelType = object@modelType)
            param <- lapply(param, constrainMatrices, SimEqualCon = SimEqualCon)
            Output2 <- lapply(param, fillParam, modelType = object@modelType)
          } else {
            stop("The specifications of 'misBeforeFill' and 'conBeforeMis' in the SimMisspec object and the 'conBeforeFill' in the SimEqualCon are not consistent. Change one of those specifications.")
        }
    }
    if (length(Output2) > 1) {
        macsMis <- lapply(Output2, createImpliedMACS)
        macsPop <- createImpliedMACS(Output1)
        p <- length(macsPop$M)
        nElements <- p + (p * (p + 1)/2)
        nFree <- countFreeParameters(object)
        if (!isNullObject(SimEqualCon)) 
            nFree <- nFree + countFreeParameters(SimEqualCon)
        dfParam <- nElements - nFree
        misfit <- sapply(macsMis, popMisfit, param = macsPop, dfParam = dfParam, fit.measures = misspec@misfitType)
        element <- NULL
        if (misspec@optMisfit == "min") {
            element <- which(misfit == min(misfit))
        } else if (misspec@optMisfit == "max") {
            element <- which(misfit == max(misfit))
        } else {
            stop("Something is wrong in the runMisspec function!")
        }
        Output2 <- Output2[[element]]
        Mis <- Mis[[element]]
    } else {
        Output2 <- Output2[[1]]
        Mis <- Mis[[1]]
    }
    return(list(param = Output1, misspec = Output2, misspecAdd = Mis))
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

createImpliedMACS <- function(reducedParamSet) {
    implied.mean <- NULL
    implied.covariance <- NULL
    ID <- matrix(0, nrow(reducedParamSet$PS), nrow(reducedParamSet$PS))
    diag(ID) <- 1
    if (reducedParamSet$modelType == "CFA") {
        implied.mean <- reducedParamSet$LY %*% reducedParamSet$AL + reducedParamSet$TY
        implied.covariance <- (reducedParamSet$LY %*% reducedParamSet$PS %*% t(reducedParamSet$LY)) + reducedParamSet$TE
    } else if (reducedParamSet$modelType == "Path" | reducedParamSet$modelType == "SEM") {
        implied.mean <- solve(ID - reducedParamSet$BE) %*% reducedParamSet$AL
        implied.covariance <- solve(ID - reducedParamSet$BE) %*% reducedParamSet$PS %*% t(solve(ID - reducedParamSet$BE))
        if (reducedParamSet$modelType == "SEM") {
            implied.mean <- reducedParamSet$TY + (reducedParamSet$LY %*% implied.mean)
            implied.covariance <- (reducedParamSet$LY %*% implied.covariance %*% t(reducedParamSet$LY)) + reducedParamSet$TE
        }
    } 
    return(list(M = as.vector(implied.mean), CM = implied.covariance))
})

popMisfitMACS <- function(paramM, paramCM, misspecM, misspecCM, dfParam = NULL, fit.measures = "all") {
    if (fit.measures == "all") {
        fit.measures <- getKeywords()$usedFitPop
        if (is.null(dfParam)) 
            fit.measures <- fit.measures[c(1, 3)]
    }
    p <- length(paramM)
    fit.measures <- tolower(fit.measures)
    f0 <- popDiscrepancy(paramM, paramCM, misspecM, misspecCM)
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
