

dir <- "/nfs/home/patr1ckm/repos/simsem/simsem/R/"

# We should be able to infer the model type from the matrices given. For now I leave a modelType argument.
# We also need special inference stuff for multiple groups.

model <- function(LY = NULL,PS = NULL,RPS = NULL, TE = NULL,RTE = NULL, BE = NULL, VTE = NULL, VY = NULL, VPS = NULL, TY = NULL, AL = NULL, MY = NULL,
                  ME = NULL, modelType=NULL) {
  
  paramSet <- list(LY, PS, RPS, TE, RTE, BE, VTE, VY, VPS, TY, AL, MY, ME)
  if(!is.null(modelType)) {
    if(modelType == "CFA") { modelMats <- buildCFA(paramSet) }
    else if(modelType == "SEM") { modelMats <- buildSEM(paramSet) }
    else if(modelType == "Path") { modelMats <- buildPath(paramSet) }
    else stop("modelType not recognized. Possible options are: \"CFA\", \"SEM\", or \"Path\"")
  } else { stop("Must specify model type. Possible options are: \"CFA\", \"SEM\", or \"Path\"") }

  pt <- buildPT(paramSet)
  
  return(paramSet)

}

buildPT <- function(paramSet) {

  object <- collapseExo(object, label = TRUE)
    constraint <- collapseExo(constraint, label = TRUE, value = NA)  ###################Have some zeros
    if (!isNullObject(object@LY)) {
        for (i in 1:ncol(object@LY)) {
            temp <- paste(colnames(object@LY)[i], "=~")
            something <- FALSE
            for (j in 1:nrow(object@LY)) {
                if (is.na(object@LY[j, i]) | object@LY[j, i] != 0 | !is.na(constraint@LY[j, i])) {
                  content <- paste(object@LY[j, i], "*", sep = "")
                  if (!is.na(constraint@LY[j, i])) 
                    content <- constraint@LY[j, i]
                  temp <- paste(temp, " ", content, rownames(object@LY)[j], sep = "")
                  something <- TRUE
                }
                if (something && j != nrow(object@LY) && (is.na(object@LY[j + 1, i]) | object@LY[j + 1, i] != 0)) 
                  temp <- paste(temp, "+")
            }
            if ((sum(is.na(object@LY[, i])) == 0) && (sum(object@LY[, i]) == 0)) 
                temp <- paste(temp, "0*", rownames(object@LY)[1], sep = "")
            if (!is.null(aux)) {
                noVar <- !(is.na(diag(object@TE)) | diag(object@TE) != 0)
                lab <- "0"
                if (sum(noVar) > 0) {
                  LYsingle <- extract(object@LY, which(noVar), i)
                  LYsingle <- is.na(LYsingle) | LYsingle != 0
                  if (sum(LYsingle) > 0) 
                    lab <- "NA"
                }
                temp <- paste(temp, paste(paste(" + ", lab, "*", aux, sep = ""), collapse = ""))
            }
            result <- paste(result, temp, "\n")
        }
    }
    if (!isNullObject(object@BE)) {
        for (i in 1:nrow(object@BE)) {
            temp <- NULL
            for (j in 1:ncol(object@BE)) {
                if (is.na(object@BE[i, j]) | object@BE[i, j] != 0 | !is.na(constraint@BE[i, j])) {
                  content <- paste(object@BE[i, j], "*", sep = "")
                  if (!is.na(constraint@BE[i, j])) 
                    content <- constraint@BE[i, j]
                  temp <- paste(temp, " ", content, colnames(object@BE)[j], sep = "")
                }
                if (!is.null(temp) && j != ncol(object@BE) && (is.na(object@BE[i, j + 1]) | object@BE[i, j + 1] != 0)) 
                  temp <- paste(temp, "+")
            }
            if (!is.null(temp)) {
                temp2 <- paste(rownames(object@BE)[i], "~")
                result <- paste(result, temp2, temp, "\n")
            }
        }
        if (isNullObject(object@LY) && !is.null(aux)) {
            set <- findRecursiveSet(object@BE)
            target <- colnames(object@BE)[set[[1]]]
            for (i in 1:length(aux)) {
                temp <- paste(aux[1], " ~ ", paste(paste("NA*", target), collapse = " + "), "\n", sep = "")
                result <- paste(result, temp)
            }
        }
    }
    if (!isNullObject(object@PS)) {
        var.code <- NULL
        for (i in 1:length(diag(object@PS))) {
            if (!is.na(object@PS[i, i]) | !is.na(constraint@PS[i, i])) {
                content <- paste(object@PS[i, i], "*", sep = "")
                if (!is.na(constraint@PS[i, i])) 
                  content <- constraint@PS[i, i]
                var.code <- paste(var.code, colnames(object@PS)[i], " ~~ ", content, colnames(object@PS)[i], " \n", sep = "")
            }
        }
        cov.code <- NULL
        if (nrow(object@PS) > 1) {
            for (i in 2:nrow(object@PS)) {
                for (j in 1:(i - 1)) {
                  if (is.na(object@PS[i, j]) | object@PS[i, j] != 0 | !is.na(constraint@PS[i, j])) {
                    content <- paste(object@PS[i, j], "*", sep = "")
                    if (!is.na(constraint@PS[i, j])) 
                      content <- constraint@PS[i, j]
                    if (isNullObject(object@BE)) {
                      cov.code <- paste(cov.code, rownames(object@PS)[i], " ~~ ", content, colnames(object@PS)[j], " \n", sep = "")
                    } else {
                      exo.set <- findRecursiveSet(object@BE)[[1]]
                      if (!(is.element(i, exo.set) & is.element(j, exo.set))) {
                        cov.code <- paste(cov.code, rownames(object@PS)[i], " ~~ ", content, colnames(object@PS)[j], " \n", sep = "")
                      }
                    }
                  } else {
                    content <- paste(object@PS[i, j], "*", sep = "")
                    if (isNullObject(object@BE)) {
                      cov.code <- paste(cov.code, rownames(object@PS)[i], " ~~ ", content, colnames(object@PS)[j], " \n", sep = "")
                    } else {
                      auxFac <- which(apply(object@BE, 1, function(x) all(!is.na(x) & (x == 0))) & apply(object@BE, 2, function(x) all(!is.na(x) & (x == 0))))
                      if (is.element(i, auxFac) | is.element(j, auxFac)) {
                        cov.code <- paste(cov.code, rownames(object@PS)[i], " ~~ 0*", colnames(object@PS)[j], " \n", sep = "")
                      }
                    }
                  }
                }
            }
        }
        result <- paste(result, var.code, cov.code)
        if (isNullObject(object@LY) && !is.null(aux)) {
            set <- findRecursiveSet(object@BE)
            target <- colnames(object@BE)[-set[[1]]]
            varCode <- paste(paste(aux, " ~~ NA*", aux, sep = ""), collapse = "\n")
            result <- paste(result, varCode, "\n")
            corCode <- paste(outer(aux, target, paste, sep = " ~~ NA*"), collapse = "\n")
            result <- paste(result, corCode, "\n")
        }
    }
    if (!isNullObject(object@TE)) {
        var.code <- NULL
        for (i in 1:length(diag(object@TE))) {
            if (!is.na(object@TE[i, i]) | !is.na(constraint@TE[i, i])) {
                content <- paste(object@TE[i, i], "*", sep = "")
                if (!is.na(constraint@TE[i, i])) 
                  content <- constraint@TE[i, i]
                var.code <- paste(var.code, colnames(object@TE)[i], " ~~ ", content, colnames(object@TE)[i], " \n", sep = "")
            }
        }
        cov.code <- NULL
        for (i in 2:nrow(object@TE)) {
            for (j in 1:(i - 1)) {
                if (is.na(object@TE[i, j]) | object@TE[i, j] != 0 | !is.na(constraint@TE[i, j])) {
                  content <- paste(object@TE[i, j], "*", sep = "")
                  if (!is.na(constraint@TE[i, j])) 
                    content <- constraint@TE[i, j]
                  cov.code <- paste(cov.code, rownames(object@TE)[i], " ~~ ", content, colnames(object@TE)[j], " \n", sep = "")
                }
            }
        }
        result <- paste(result, var.code, cov.code)
        if (!isNullObject(object@LY) && !is.null(aux)) {
            nonConstant <- is.na(diag(object@TE)) | diag(object@TE) != 0
            target <- colnames(object@TE)[nonConstant]
            varCode <- paste(paste(aux, " ~~ NA*", aux, sep = ""), collapse = "\n")
            result <- paste(result, varCode, "\n")
            corCode <- paste(outer(aux, target, paste, sep = " ~~ NA*"), collapse = "\n")
            result <- paste(result, corCode, "\n")
        }
    }
    if (!is.null(aux) && length(aux) > 1) {
        corCode <- outer(aux, aux, paste, sep = " ~~ NA*")
        diag(corCode) <- ""
        corCode <- corCode[lower.tri(corCode)]
        corCode <- paste(paste(corCode, collapse = "\n"), "\n")
        result <- paste(result, corCode)
    }
    if (!isNullObject(object@AL)) {
        mean.code <- NULL
        for (i in 1:length(object@AL)) {
            if (!(object@modelType == "Path" | object@modelType == "Path.exo") | !is.na(object@AL[i]) | !is.na(constraint@AL[i])) {
                content <- paste(object@AL[i], "*", sep = "")
                if (!is.na(constraint@AL[i])) 
                  content <- constraint@AL[i]
                mean.code <- paste(mean.code, names(object@AL)[i], " ~ ", content, "1 \n", sep = "")
            }
        }
        result <- paste(result, mean.code)
    }
    if (!isNullObject(object@TY)) {
        mean.code <- NULL
        for (i in 1:length(object@TY)) {
            content <- paste(object@TY[i], "*", sep = "")
            if (!is.na(constraint@TY[i])) 
                content <- constraint@TY[i]
            mean.code <- paste(mean.code, names(object@TY)[i], " ~ ", content, "1 \n", sep = "")
        }
        result <- paste(result, mean.code)
    }
    if (!is.null(aux)) {
        temp <- paste(paste(aux, " ~ NA*1 \n"), collapse = "")
        result <- paste(result, temp)
    }
    return(result)
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
    out <- list(LY = LY[[1]], PS = PS[[1]], RPS = RPS[[1]], TE = TE[[1]], RTE = RTE[[1]], VE = VE[[1]], VPS = VE[[1]], VTE = VTE[[1]], VY = VY[[1]], TY = TY[[1]], MY = MY[[1]], ME = ME[[1]], AL = ME[[1]])
    return(out)
}

# New version - no exogenous. W00t.
buildPath <- function(paramSet) {
   
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
