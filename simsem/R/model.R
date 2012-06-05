source("AllClass.R")
source("bind.R")
source("utils.R")


  cfa <- function() {
    loading <- matrix(0, 6, 2)
    loading[1:3, 1] <- NA
    loading[4:6, 2] <- "a1"
    LY <- bind(loading, 0.7)

    latent.cor <- matrix(NA, 2, 2)
    diag(latent.cor) <- 1
    RPS <- bind(latent.cor, 0.5)

    error.cor <- matrix(0, 6, 6)
    diag(error.cor) <- 1
    RTE <- bind(error.cor)

    return(list(LY=LY,RPS=RPS,RTE=RTE))
  }
  # CFA with more matrices
  cfa2 <- function() {
    loading <- matrix(0, 9, 3)
    loading[1:3, 1] <- c(1, NA, NA)
    loading[4:6, 2] <- c(1, NA, NA)
    loading[7:9, 3] <- c(1, NA, NA)
    loadingVal <- matrix(0, 9, 3)
    loadingVal[2:3, 1] <- c(0.6, 0.7)
    loadingVal[5:6, 2] <- c(1.1, 0.9)
    loadingVal[8:9, 3] <- c(1.2, 1.1)
    LY <- bind(loading, loadingVal)

    facCov <- matrix(NA, 3, 3)
    facCovVal <- diag(c(0.8, 0.9, 0.4))
    facCovVal[lower.tri(facCovVal)] <- c(0.4, 0.2, 0.3)
    facCovVal[upper.tri(facCovVal)] <- c(0.4, 0.2, 0.3)
    PS <- bind(facCov, facCovVal)

    errorCov <- diag(NA, 9)
    errorCovVal <- diag(c(0.5, 1.1, 0.8, 0.4, 0.4, 0.8, 0.8, 0.5, 0.6))
    TE <- bind(errorCov, errorCovVal)

    AL <- bind(rep(NA, 3), 0)
    TY <- bind(c(0, NA, NA, 0, NA, NA, 0, NA, NA), 0)

    return(list(LY=LY,PS=PS,TE=TE,AL=AL,TY=TY))
  }
  # Path
  path <- function() {
    path.BE <- matrix(0, 4, 4)
    path.BE[3, 1:2] <- NA
    path.BE[4, 3] <- NA
    starting.BE <- matrix("", 4, 4)
    starting.BE[3, 1:2] <- "runif(1,0.3,0.5)"
    starting.BE[4, 3] <- "runif(1,0.5,0.7)"
    BE <- bind(path.BE, starting.BE)

    residual.error <- diag(4)
    residual.error[1,2] <- residual.error[2,1] <- NA
    RPS <- bind(residual.error, "rnorm(1,0.3,0.1)")

    ME <- bind(rep(NA, 4), 0)
   
    return(list(BE=BE,RPS=RPS,ME=ME))
  }
  # SEM
  sem <- function() {
    loading <- matrix(0, 8, 3)
    loading[1:3, 1] <- NA
    loading[4:6, 2] <- NA
    loading[7:8, 3] <- NA
    loading.start <- matrix("", 8, 3)
    loading.start[1:3, 1] <- 0.7
    loading.start[4:6, 2] <- 0.7
    loading.start[7:8, 3] <- "rnorm(1,0.6,0.05)"
    LY <- bind(loading, loading.start)

    RTE <- bind(diag(8))

    factor.cor <- diag(3)
    factor.cor[1, 2] <- factor.cor[2, 1] <- NA
    RPS <- bind(factor.cor, 0.5)

    path <- matrix(0, 3, 3)
    path[3, 1:2] <- NA
    path.start <- matrix(0, 3, 3)
    path.start[3, 1] <- "rnorm(1,0.6,0.05)"
    path.start[3, 2] <- "runif(1,0.3,0.5)"
    BE <- bind(path, path.start)

    
    return(list(LY=LY,RTE=RTE,RPS=RPS,BE=BE))
  }

cfa <- cfa()
cfa2 <- cfa2()
path <- path()
sem <- sem()

dir <- "/nfs/home/patr1ckm/repos/simsem/simsem/R/"

model(LY=cfa$LY,RPS=cfa$RPS,RTE=cfa$RTE, modelType="CFA")
model(LY=cfa2$LY,PS=cfa2$PS,TE=cfa2$TE,AL=cfa2$AL,TY=cfa2$TY, modelType="CFA")
model(BE=path$BE, RPS=path$RPS, ME=path$ME, modelType="Path")
model(LY=sem$LY, RTE=sem$RTE, RPS=sem$RPS, BE=sem$BE, modelType="SEM")
# We should be able to infer the model type from the matrices given. For now I leave a modelType argument.
# We also need special inference stuff for multiple groups.
# Auxiliary variables?

model <- function(LY = NULL,PS = NULL,RPS = NULL, TE = NULL,RTE = NULL, BE = NULL, VTE = NULL, VY = NULL,
                  VPS = NULL, TY = NULL, AL = NULL, MY = NULL, ME = NULL, modelType=NULL, indLab=NULL, facLab=NULL) {
  
  paramSet <- list(LY=LY, PS=PS, RPS=RPS, TE=TE, RTE=RTE, BE=BE, VTE=VTE, VY=VY, VPS=VPS, TY=TY, AL=AL, MY=MY,ME=ME)
  if(!is.null(modelType)) {
    if(modelType == "CFA") { paramSet <- buildCFA(paramSet) }
    else if(modelType == "SEM") { paramSet <- buildSEM(paramSet) }
    else if(modelType == "Path") { paramSet <- buildPath(paramSet) }
    else stop("modelType not recognized. Possible options are: \"CFA\", \"SEM\", or \"Path\"")
  } else { stop("Must specify model type. Possible options are: \"CFA\", \"SEM\", or \"Path\"") }

#  pt <- buildPT(paramSet)
  
  return(paramSet)

}

buildCFA <- function(paramSet) {
 
  if(is.null(paramSet$LY)) stop("No loading object in CFA")
  
  ni <- nrow(paramSet$LY@free)
  nk <- ncol(paramSet$LY@free)
  
  if (!is.null(paramSet$TE)) {
    if(!is.null(paramSet$RTE)) stop("Error covariance and error correlation cannot be specified at the same time!")
    if(!is.null(paramSet$VTE)) stop("Error covariance and error variance cannot be specified at the same time!")
    if(!is.null(paramSet$VY)) stop("Error covariance and total indicator variance cannot be specified at the same time!")
  } else {
        
    if(is.null(paramSet$RTE)) stop("No error correlation object in CFA")
    if(is.null(paramSet$VY)) { paramSet$VY <- bind(rep(NA,ni),popParam=1) } # Set variance of indicators to be free, pop value of 1
  }

  if(!is.null(paramSet$PS)) {
    if(!is.null(paramSet$RPS)) stop("Factor covariance and factor correlation cannot be specified at the same time!")
    if(!is.null(paramSet$VE)) stop("Factor covariance and factor variance cannot be specified at the same time!")
    } else {
        if(is.null(paramSet$RPS)) stop("No latent variables correlation object in CFA")
        if(is.null(paramSet$VE)) { paramSet$VE <- bind(free=rep(1,nk)) } # Set the latent variances to be fixed to 1  
    }
  
  if(is.null(paramSet$TY)) { paramSet$TY <- bind(free=rep(NA,ni),popParam=0) } # Set measurement intercept to be free, pop value of 0
  if(is.null(paramSet$ME)) { paramSet$ME <- bind(free=rep(0,nk)) } # Set means of indicators to be fixed to 0

  return(paramSet)
}

buildPath <- function(paramSet) {

    if(is.null(paramSet$BE)) stop("No path coefficient object between factor.ETA")
    ne <- ncol(paramSet$BE@free)
    if(!is.null(paramSet$PS)) {
      if(!is.null(paramSet$RPS)) stop("Covariance and correlation cannot be specified at the same time!")
      if(!is.null(paramSet$VPS)) stop("Covariance and variance cannot be specified at the same time!")
      if(!is.null(paramSet$VE)) stop("Covariance and total indicator variance cannot be specified at the same time!")
    } else {
      if(is.null(paramSet$RPS)) stop("No residual correlation object between factor.ETA")
      if(is.null(paramSet$VE)) { paramSet$VE <- bind(free=rep(NA,ne),popParam = 1) } # Set latent variance to be free, pop value = 1
    }
    if(is.null(paramSet$AL)) { AL <- bind(rep(NA,ne),popParam=0) } # Set factor means to be free, pop value = 0
 
    return(paramSet)
}

buildSEM <- function(paramSet) {

  if(is.null(paramSet$LY)) stop("No loading object of indicator.Y from factor.ETA in SEM")
  ny <- nrow(paramSet$LY@free)
  ne <- ncol(paramSet$LY@free)

  if(!is.null(paramSet$TE)) {
    if(!is.null(paramSet$RTE)) stop("Error covariance and error correlation cannot be specified at the same time!")
    if(!is.null(paramSet$VTE)) stop("Error covariance and error variance cannot be specified at the same time!")
    if(!is.null(paramSet$VY)) stop("Error covariance and total indicator variance cannot be specified at the same time!")
  } else {
    if(is.null(paramSet$RTE)) stop("No measurement error correlation object between indicator.Y")
    if(is.null(paramSet$VY)) { paramSet$VY <- bind(rep(NA,ny),popParam=1) } # Set indicator variance to be free, pop value at 1
  }

  if(is.null(paramSet$TY)) { paramSet$TY <- bind(rep(NA,ny),popParam=0) } # Set measurement intercepts to be free, pop value at 0
  
  if(is.null(paramSet$BE)) stop("No path coefficient object between factor.ETA")

   if(!is.null(paramSet$PS)) {
    if(!is.null(paramSet$RPS)) stop("Error covariance and error correlation cannot be specified at the same time!")
    if(!is.null(paramSet$VPS)) stop("Error covariance and error variance cannot be specified at the same time!")
    if(!is.null(paramSet$VE)) stop("Error covariance and total indicator variance cannot be specified at the same time!")
  } else {
    if(is.null(paramSet$RPS)) stop("No measurement error correlation object between indicator.Y")
    if(is.null(paramSet$VE)) { paramSet$VE <- bind(rep(1,ne)) } # Set factor variance to be fixed at 1
  }
 if(is.null(paramSet$AL)) { AL <- bind(rep(0,ne)) } # Set factor means to be fixed at 0
    return(paramSet)
} 

# All parameters are built into the pt
buildPT <- function(paramSet, facLab=NULL, indLab=NULL) {
  
  if(!is.null(paramSet$LY)) {
    LY.f <- paramSet$LY@free
    LY.pop <- paramSet$LY@popParam
    LY.mis <- paramSet$LY@misspec

    nf <- ncol(LY.f)
    ni <- nrow(LY.f)

    id <- 1:(ni*nf)
    if(is.null(facLab)) {
      lhs <- sort(rep(paste("y",1:nf,sep=""),ni))
    } else {
      lhs <- sort(rep(facLab,ni))
    }

    op <- rep("=~",length(id))

    if(is.null(indLab)) {
      rhs <- rep(paste("x",1:ni,sep=""),nf)
    } else {
      rhs <- rep(indLab,nf)
    }

    # user?

    group <- rep(1,length(id))

    free.log <- as.vector(is.free(LY.f))
    free.idx <- 1:sum(free.log)
    free <- mapply(free.log,free.idx, FUN=function(x,y) {ifelse(x,y,0) })
    
    ustart <- as.vector(LY.f)

    exo <- rep(0,length(id))

    label.log <- is.label(LY.f))
    label <- 
  
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

freeIdx <- function(mat) {
  flat <- as.vector(mat)
  free.idx <- NULL
  isLabel <- is.label(flat)
  
  for(i in 1:length(flat) {
    if(is.na(flat[i])) {
      free.idx <- c(free.idx,i)
    } else if(isLabel[i]) {
      label <- flat[i]
      if(is.null(conList[label])
        conList <- c(conList,i)
        names(conList) <- c(names,
      
  
}
