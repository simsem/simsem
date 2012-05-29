library(simsem)

# temporary, for development only
s <- function(dir) {
  sourceDir <- function(path, trace = TRUE, ...) {
     for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
		if(nm != "AllClass.R" & nm != "AllGenerics.R") {
        if(trace) cat(nm,":") 
        source(file.path(path, nm), ...)
        if(trace) cat("\n")
		}
     }
   }
 source(paste(dir, "AllClass.R", sep=""))
 source(paste(dir, "AllGenerics.R", sep=""))
 sourceDir(dir)
}

models <- function(ex) {
  # Basic CFA
  ex1 <- function() {
    loading <- matrix(0, 6, 2)
    loading[1:3, 1] <- NA
    loading[4:6, 2] <- NA
    LY <- simMatrix(loading, 0.7)

    latent.cor <- matrix(NA, 2, 2)
    diag(latent.cor) <- 1
    RPS <- symMatrix(latent.cor, 0.5)

    error.cor <- matrix(0, 6, 6)
    diag(error.cor) <- 1
    RTE <- symMatrix(error.cor)

    #CFA.Model <- buildCFA(LY = LY, RPS = RPS, RTE = RTE)

    SimData <- simData(CFA.Model, 200)

    #SimModel <- simModel(CFA.Model)
    return(list(LX,RPH,RTD))
  }
  # CFA with more matrices
  ex2 <- function() {
    loading <- matrix(0, 9, 3)
    loading[1:3, 1] <- c(1, NA, NA)
    loading[4:6, 2] <- c(1, NA, NA)
    loading[7:9, 3] <- c(1, NA, NA)
    loadingVal <- matrix(0, 9, 3)
    loadingVal[2:3, 1] <- c(0.6, 0.7)
    loadingVal[5:6, 2] <- c(1.1, 0.9)
    loadingVal[8:9, 3] <- c(1.2, 1.1)
    LY <- simMatrix(loading, loadingVal)

    facCov <- matrix(NA, 3, 3)
    facCovVal <- diag(c(0.8, 0.9, 0.4))
    facCovVal[lower.tri(facCovVal)] <- c(0.4, 0.2, 0.3)
    facCovVal[upper.tri(facCovVal)] <- c(0.4, 0.2, 0.3)
    PS <- symMatrix(facCov, facCovVal)

    errorCov <- diag(NA, 9)
    errorCovVal <- diag(c(0.5, 1.1, 0.8, 0.4, 0.4, 0.8, 0.8, 0.5, 0.6))
    TE <- symMatrix(errorCov, errorCovVal)

    AL <- simVector(rep(NA, 3), 0)
    TY <- simVector(c(0, NA, NA, 0, NA, NA, 0, NA, NA), 0)

    #HS.Model <- (LY=LY, PS=PS, TE=TE, AL=AL, TY=TY)
    SimData <- simData(HS.Model, 200)
    #SimModel <- simModel(HS.Model)
    return(list(LY=LY,PS=PS,TE=TE,AL=AL,TY=TY))
  }
  # Path
  ex3 <- function() {
    path.BE <- matrix(0, 4, 4)
    path.BE[3, 1:2] <- NA
    path.BE[4, 3] <- NA
    starting.BE <- matrix("", 4, 4)
    starting.BE[3, 1:2] <- "runif(1,0.3,0.5)"
    starting.BE[4, 3] <- "runif(1,0.5,0.7)"
    BE <- simMatrix(path.BE, starting.BE)

    residual.error <- diag(4)
    residual.error[1,2] <- residual.error[2,1] <- NA
    RPS <- symMatrix(residual.error, "rnorm(1,0.3,0.1)")

    ME <- simVector(rep(NA, 4), 0)
    #Path.Model <- simSetPath(RPS = RPS, BE = BE, ME = ME)
    return(list(BE=BE,RPS=RPS,ME=ME))
  }
  # SEM
  ex4 <- function() {
    loading <- matrix(0, 8, 3)
    loading[1:3, 1] <- NA
    loading[4:6, 2] <- NA
    loading[7:8, 3] <- NA
    loading.start <- matrix("", 8, 3)
    loading.start[1:3, 1] <- 0.7
    loading.start[4:6, 2] <- 0.7
    loading.start[7:8, 3] <- "rnorm(1,0.6,0.05)"
    LY <- simMatrix(loading, loading.start)

    RTE <- symMatrix(diag(8))

    factor.cor <- diag(3)
    factor.cor[1, 2] <- factor.cor[2, 1] <- NA
    RPS <- symMatrix(factor.cor, 0.5)

    path <- matrix(0, 3, 3)
    path[3, 1:2] <- NA
    path.start <- matrix(0, 3, 3)
    path.start[3, 1] <- "rnorm(1,0.6,0.05)"
    path.start[3, 2] <- "runif(1,0.3,0.5)"
    BE <- simMatrix(path, path.start)

    #SEM.model <- simSetSEM(BE=BE, LY=LY, RPS=RPS, RTE=RTE)
    return(list(LY=LY,RTE=RTE,RPS=RPS,BE=BE))
  }
    
  if(ex == 1) {return(ex1())}
  if(ex == 2) {return(ex2())}
  if(ex == 3) {return(ex3())}
  if(ex == 4) {return(ex4())}
}

dir <- "/nfs/home/patr1ckm/repos/simsem/simsem/R/"

#s(dir)
#cfa1 <- models(ex=1)
#fa2 <- models(ex=2)
#path <- models(ex=3)
#sem <- models(ex=4)
# need exogenous

# So my basic idea so far is that a user will be able to build all the possible
# model types with one call to simSet and an argument "modelType."
# Next, we'll cut out the intermediate step of creating a "SimSet" object, and
# instead create a parameter table.

simSet <- function(...,modelType=NULL) {
  modelMats <- list()
  if(!is.null(modelType)) {
    if(modelType == "CFA") { modelMats <- buildCFA(...) }
    else if(modelType == "SEM") { modelMats <- buildSEM(...,exo) }
    else if(modelType == "Path") { modelMats <- buildPath(...,exo) }
    else stop("modelType not recognized. Possible options are: \"CFA\", \"SEM\", or \"Path\"")
  } else { stop("Must specify model type. Possible options are: \"CFA\", \"SEM\", or \"Path\"") }


  
  
  return(modelMats)
  
}

buildPT <- function(modelMats) {

}


buildCFA <- function(...) {
    W <- getKeywords()
    List <- list(...)
    Names <- names(List)
    keywords <- list(W$loading, W$errorCor, W$facCor, W$errorVar, W$indicatorVar, W$intercept, W$facMean, W$indicatorMean, W$facVar, W$errorCov, W$facCov)  # 11 total
    position <- matchKeywords(Names, keywords)
    if (length(position) != length(unique(position))) 
        stop("Some objects were identified more than once.")
    ifelse(1 %in% position, LY <- List[position == 1], stop("No loading object in CFA"))
    ni <- nrow(run(LY[[1]]))
    nk <- ncol(run(LY[[1]]))
    if (10 %in% position) {
        TE <- List[position == 10]
        ifelse(2 %in% position, stop("Error covariance and error correlation cannot be specified at the same time!"), RTE <- list(new("NullSymMatrix")))
        ifelse(4 %in% position, stop("Error covariance and error variance cannot be specified at the same time!"), VTE <- list(new("NullSimVector")))
        ifelse(5 %in% position, stop("Error covariance and total indicator variance cannot be specified at the same time!"), VY <- list(new("NullSimVector")))
    } else {
        TE <- list(new("NullSymMatrix"))
        ifelse(2 %in% position, RTE <- List[position == 2], stop("No error correlation object in CFA"))
        ifelse(4 %in% position, VTE <- List[position == 4], VTE <- list(new("NullSimVector")))
        ifelse(5 %in% position, VY <- List[position == 5], ifelse(isNullObject(VTE[[1]]), {
            VY <- list(freeVector(1, ni))
            comment(VY[[1]]) <- "default"
        }, VY <- list(new("NullSimVector"))))
    }
    if (11 %in% position) {
        PS <- List[position == 11]
        ifelse(3 %in% position, stop("Factor covariance and factor correlation cannot be specified at the same time!"), RPS <- list(new("NullSymMatrix")))
        ifelse(9 %in% position, stop("Factor covariance and factor variance cannot be specified at the same time!"), VE <- list(new("NullSimVector")))
    } else {
        PS <- list(new("NullSymMatrix"))
        ifelse(3 %in% position, RPS <- List[position == 3], stop("No latent variables correlation object in CFA"))
        ifelse(9 %in% position, VE <- List[position == 9], {
            VE <- list(constantVector(1, nk))
            comment(VE[[1]]) <- "default"
        })
    }
    ifelse(8 %in% position, MY <- List[position == 8], MY <- list(new("NullSimVector")))
    ifelse(6 %in% position, TY <- List[position == 6], ifelse(isNullObject(MY[[1]]), {
        TY <- list(freeVector(0, ni))
        comment(TY[[1]]) <- "default"
    }, TY <- list(new("NullSimVector"))))
    ifelse(7 %in% position, ME <- List[position == 7], {
        ME <- list(constantVector(0, nk))
        comment(ME[[1]]) <- "default"
    })
    out <- list(LY = LY[[1]], PS = PS[[1]], RPS = RPS[[1]], TE = TE[[1]], RTE = RTE[[1]], VE = VE[[1]], VPS = VE[[1]], VTE = VTE[[1]], VY = VY[[1]], TY = TY[[1]], MY = MY[[1]], ME = ME[[1]], 
        AL = ME[[1]])
    return(out)
}

# New version - no exogenous. W00t.
buildPath <- function(...) {
    W <- getKeywords()
    List <- list(...)
    Names <- names(List)
    keywords <- NULL

    #if (exo == FALSE) {
    #    keywords <- list(W$BE, W$RPS, W$VPS, W$VE, W$AL, W$ME, W$PS)  # Length = 7
    #} else {
    #   keywords <- list(W$BE, W$RPS, W$VPS, W$VE, W$AL, W$ME, W$PS, W$GA, W$RPH, W$VPH, W$KA, W$PH)  # Length = 12
    #}
    

    keywords <- list(W$BE, W$RPS, W$VPS, W$VE, W$AL, W$ME, W$PS)
    position <- matchKeywords(Names, keywords)
    if (length(position) != length(unique(position))) 
        stop("Some objects were identified more than once.")
    ifelse(1 %in% position, BE <- List[position == 1], stop("No path coefficient object between factor.ETA"))
    ne <- ncol(run(BE[[1]]))
    if (7 %in% position) {
        PS <- List[position == 7]
        ifelse(2 %in% position, stop("Covariance and correlation cannot be specified at the same time!"), RPS <- list(new("NullSymMatrix")))
        ifelse(3 %in% position, stop("Covariance and variance cannot be specified at the same time!"), VPS <- list(new("NullSimVector")))
        ifelse(4 %in% position, stop("Covariance and total indicator variance cannot be specified at the same time!"), VE <- list(new("NullSimVector")))
    } else {
        PS <- list(new("NullSymMatrix"))
        ifelse(2 %in% position, RPS <- List[position == 2], stop("No residual correlation object between factor.ETA"))
        ifelse(3 %in% position, VPS <- List[position == 3], VPS <- list(new("NullSimVector")))
        ifelse(4 %in% position, VE <- List[position == 4], ifelse(isNullObject(VPS[[1]]), {
            VE <- list(freeVector(1, ne))
            comment(VE[[1]]) <- "default"
        }, VE <- list(new("NullSimVector"))))
    }
    ifelse(6 %in% position, ME <- List[position == 6], ME <- list(new("NullSimVector")))
    ifelse(5 %in% position, AL <- List[position == 5], ifelse(isNullObject(ME[[1]]), {
        AL <- list(freeVector(0, ne))
        comment(AL[[1]]) <- "default"
    }, AL <- list(new("NullSimVector"))))
    Output <- NULL
    #if (exo) {
    #    ifelse(8 %in% position, GA <- List[position == 8], stop("No path coefficient object from Factor.KSI to Factor.ETA"))
    #    nk <- ncol(run(GA[[1]]))
    #    if (12 %in% position) {
    #        PH <- List[position == 12]
    #        ifelse(9 %in% position, stop("Covariance and correlation cannot be specified at the same time!"), RPH <- list(new("NullSymMatrix")))
    #        ifelse(10 %in% position, stop("Covariance and variance cannot be specified at the same time!"), VPH <- list(new("NullSimVector")))
    #    } else {
    #        PH <- list(new("NullSymMatrix"))
    #        ifelse(9 %in% position, RPH <- List[position == 9], stop("No correlation object between factor.KSI"))
    #        ifelse(10 %in% position, VPH <- List[position == 10], {
    #            VPH <- list(freeVector(1, nk))
    #            comment(VPH[[1]]) <- "default"
    #        })
    #    }
    #    ifelse(11 %in% position, KA <- List[position == 11], {
    #        KA <- list(freeVector(0, nk))
    #        comment(KA[[1]]) <- "default"
    #    })
    #    Output <- list(BE = BE[[1]], PS = PS[[1]], RPS = RPS[[1]], VPS = VPS[[1]], VE = VE[[1]],
    #                   AL = AL[[1]], ME = ME[[1]], GA = GA[[1]], PH = PH[[1]], RPH = RPH[[1]],
    #                   VPH = VPH[[1]], KA = KA[[1]], modelType="Path.exo")
    #} else {
    #    Output <-  list(BE = BE[[1]], PS = PS[[1]], RPS = RPS[[1]], VPS = VPS[[1]],
    #                    VE = VE[[1]], AL = AL[[1]], ME = ME[[1]], modelType="Path")
    #}
    Output <-  list(BE = BE[[1]], PS = PS[[1]], RPS = RPS[[1]], VPS = VPS[[1]],
                    VE = VE[[1]], AL = AL[[1]], ME = ME[[1]], modelType="Path")
    return(Output)
}


# New version, no exogenous.
buildSEM <- function(...) {
    W <- getKeywords()
    List <- list(...)
    Names <- names(List)
    keywords <- NULL
    #if (exo == FALSE) {
    #    keywords <- list(W$LY, W$RTE, W$VTE, W$VY, W$TY, W$MY, W$BE, W$RPS, W$VPS, W$VE, W$AL, W$ME, W$TE, W$PS)  #Length = 14
    #} else {
    #    keywords <- list(W$LY, W$RTE, W$VTE, W$VY, W$TY, W$MY, W$BE, W$RPS, W$VPS, W$VE, W$AL, W$ME, W$TE, W$PS, W$LX, W$RTD, W$VTD, W$VX, W$TX, W$MX, W$GA, W$RPH, W$VPH, W$KA, W$RTH, W$TD, W$PH, 
    #        W$TH)  #Length = 28
    #}
    keywords <- list(W$LY, W$RTE, W$VTE, W$VY, W$TY, W$MY, W$BE, W$RPS, W$VPS, W$VE, W$AL, W$ME, W$TE, W$PS)  #Length = 14
    position <- matchKeywords(Names, keywords)
    if (length(position) != length(unique(position))) 
        stop("Some objects were identified more than once.")
    ifelse(1 %in% position, LY <- List[position == 1], stop("No loading object of indicator.Y from factor.ETA in SEM"))
    ny <- nrow(run(LY[[1]]))
    ne <- ncol(run(LY[[1]]))
    if (13 %in% position) {
        TE <- List[position == 13]
        ifelse(2 %in% position, stop("Error covariance and error correlation cannot be specified at the same time!"), RTE <- list(new("NullSymMatrix")))
        ifelse(3 %in% position, stop("Error covariance and error variance cannot be specified at the same time!"), VTE <- list(new("NullSimVector")))
        ifelse(4 %in% position, stop("Error covariance and total indicator variance cannot be specified at the same time!"), VY <- list(new("NullSimVector")))
    } else {
        TE <- list(new("NullSymMatrix"))
        ifelse(2 %in% position, RTE <- List[position == 2], stop("No measurement error correlation object between indicator.Y"))
        ifelse(3 %in% position, VTE <- List[position == 3], VTE <- list(new("NullSimVector")))
        ifelse(4 %in% position, VY <- List[position == 4], ifelse(isNullObject(VTE[[1]]), {
            VY <- list(freeVector(1, ny))
            comment(VY[[1]]) <- "default"
        }, VY <- list(new("NullSimVector"))))
    }
    ifelse(6 %in% position, MY <- List[position == 6], MY <- list(new("NullSimVector")))
    ifelse(5 %in% position, TY <- List[position == 5], ifelse(isNullObject(MY[[1]]), {
        TY <- list(freeVector(0, ny))
        comment(TY[[1]]) <- "default"
    }, TY <- list(new("NullSimVector"))))
    ifelse(7 %in% position, BE <- List[position == 7], stop("No path coefficient object between factor.ETA"))
    if (14 %in% position) {
        PS <- List[position == 14]
        ifelse(8 %in% position, stop("Covariance and correlation cannot be specified at the same time!"), RPS <- list(new("NullSymMatrix")))
        ifelse(9 %in% position, stop("Covariance and variance cannot be specified at the same time!"), VPS <- list(new("NullSimVector")))
        ifelse(10 %in% position, stop("Covariance and total indicator variance cannot be specified at the same time!"), VE <- list(new("NullSimVector")))
    } else {
        PS <- list(new("NullSymMatrix"))
        ifelse(8 %in% position, RPS <- List[position == 8], stop("No residual correlation object between factor.ETA"))
        ifelse(9 %in% position, VPS <- List[position == 9], VPS <- list(new("NullSimVector")))
        ifelse(10 %in% position, VE <- List[position == 10], ifelse(isNullObject(VPS[[1]]), {
            VE <- list(constantVector(1, ne))
            comment(VE[[1]]) <- "default"
        }, VE <- list(new("NullSimVector"))))
    }
    ifelse(12 %in% position, ME <- List[position == 12], ME <- list(new("NullSimVector")))
    ifelse(11 %in% position, AL <- List[position == 11], ifelse(isNullObject(ME[[1]]), {
        AL <- list(constantVector(0, ne))
        comment(AL[[1]]) <- "default"
    }, AL <- list(new("NullSimVector"))))
    Output <- NULL
    #if (exo) {
    #    ifelse(15 %in% position, LX <- List[position == 15], stop("No loading object of indicator.X from factor.KSI in SEM"))
    #    nx <- nrow(run(LX[[1]]))
    #    nk <- ncol(run(LX[[1]]))
    #    if (26 %in% position) {
    #        TD <- List[position == 26]
    #        ifelse(16 %in% position, stop("Error covariance and error correlation cannot be specified at the same time!"), RTD <- list(new("NullSymMatrix")))
    #        ifelse(17 %in% position, stop("Error covariance and error variance cannot be specified at the same time!"), VTD <- list(new("NullSimVector")))
    #        ifelse(18 %in% position, stop("Error covariance and total indicator variance cannot be specified at the same time!"), VX <- list(new("NullSimVector")))
    #    } else {
    #        TD <- list(new("NullSymMatrix"))
    #        ifelse(16 %in% position, RTD <- List[position == 16], stop("No measurement error correlation object between indicator.Y"))
    #        ifelse(17 %in% position, VTD <- List[position == 17], VTD <- list(new("NullSimVector")))
    #        ifelse(18 %in% position, VX <- List[position == 18], ifelse(isNullObject(VTD[[1]]), {
    #            VX <- list(freeVector(1, nx))
    #            comment(VX[[1]]) <- "default"
    #        }, VX <- list(new("NullSimVector"))))
    #    }
    #    ifelse(20 %in% position, MX <- List[position == 20], MX <- list(new("NullSimVector")))
    #    ifelse(19 %in% position, TX <- List[position == 19], ifelse(isNullObject(MX[[1]]), {
    #        TX <- list(freeVector(0, nx))
    #        comment(TX[[1]]) <- "default"
    #    }, TX <- list(new("NullSimVector"))))
    #    
    #    ifelse(21 %in% position, GA <- List[position == 21], stop("No path coefficient object from Factor.KSI to Factor.ETA"))
    #    if (27 %in% position) {
    #        PH <- List[position == 27]
    #        ifelse(22 %in% position, stop("Covariance and correlation cannot be specified at the same time!"), RPH <- list(new("NullSymMatrix")))
    #        ifelse(23 %in% position, stop("Covariance and variance cannot be specified at the same time!"), VPH <- list(new("NullSimVector")))
    #    } else {
    #        PH <- list(new("NullSymMatrix"))
    #        ifelse(22 %in% position, RPH <- List[position == 22], stop("No correlation object between factor.KSI"))
    #        ifelse(23 %in% position, VPH <- List[position == 23], {
    #            VPH <- list(constantVector(1, nk))
    #            comment(VPH[[1]]) <- "default"
    #        })
    #    }
    #    ifelse(24 %in% position, KA <- List[position == 24], {
    #        KA <- list(constantVector(0, nk))
    #        comment(KA[[1]]) <- "default"
    #    })
    #    if (28 %in% position) {
    #        ifelse(25 %in% position, stop("TH and RTH cannot be specified at the same time!"), RTH <- list(new("NullSimMatrix")))
    #        TH <- List[position == 28]
    #        temp <- run(TH[[1]])
    #        if (!((nrow(temp) == nx) & (ncol(temp) == ny))) 
    #            stop("The number of rows is not equal the number of exogenous indicators or the number of columns is not equal the number of endogenous indicators.")
    #    } else {
    #        TH <- list(new("NullSimMatrix"))
    #        if (25 %in% position) {
    #            RTH <- List[position == 25]
    #            temp <- run(RTH[[1]])
    #            if (!((nrow(temp) == nx) & (ncol(temp) == ny))) 
    #              stop("The number of rows is not equal the number of exogenous indicators or the number of columns is not equal the number of endogenous indicators.")
    #        } else {
    #            RTH.Data <- matrix(0, nx, ny)
    #            RTH.Labels <- matrix(NA, nx, ny)
    #            RTH <- list(new("SimMatrix", free = RTH.Data, value = RTH.Labels))
    #            comment(RTH[[1]]) <- "default"
    #        }
    #    }
    #    Output <- list(LY = LY[[1]], TE = TE[[1]], RTE = RTE[[1]], VTE = VTE[[1]], VY = VY[[1]], MY = MY[[1]], TY = TY[[1]], BE = BE[[1]], PS = PS[[1]], RPS = RPS[[1]], VPS = VPS[[1]], VE = VE[[1]], 
    #        AL = AL[[1]], ME = ME[[1]], LX = LX[[1]], TD = TD[[1]], RTD = RTD[[1]], VTD = VTD[[1]], VX = VX[[1]], MX = MX[[1]], TX = TX[[1]], GA = GA[[1]], PH = PH[[1]], RPH = RPH[[1]], VPH = VPH[[1]], 
    #        KA = KA[[1]], TH = TH[[1]], RTH = RTH[[1]], modelType="SEM.exo")
    #} else {
    #    Output <-  list(LY = LY[[1]], TE = TE[[1]], RTE = RTE[[1]], VTE = VTE[[1]], VY = VY[[1]], MY = MY[[1]], TY = TY[[1]],
    # BE = BE[[1]], PS = PS[[1]], RPS = RPS[[1]], VPS = VPS[[1]], VE = VE[[1]],  AL = AL[[1]], ME = ME[[1]],modelType="SEM")
    #}
    Output <-  list(LY = LY[[1]], TE = TE[[1]], RTE = RTE[[1]], VTE = VTE[[1]], VY = VY[[1]], MY = MY[[1]], TY = TY[[1]],
                    BE = BE[[1]], PS = PS[[1]], RPS = RPS[[1]], VPS = VPS[[1]], VE = VE[[1]], AL = AL[[1]], ME = ME[[1]],modelType="SEM")
    return(Output)
} 
