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
buildPT <- function(paramSet, facLab=NULL, indLab=NULL, ngroup=1) {

  # Convert a chunk at a time - starting with LY
  if(is.list(paramSet$LY)) {
    dims <- dim(paramSet$LY[[1]]@free)
    nparams <- dims[1] * dims[2]
    ngroups <- length(paramSet$LY)
    startIdx <- seq(1,ngroups*nparams,by=nparams)
    mapply(paramSet$LY,group=1:ngroups,start=startIdx, FUN=parseFree,SIMPLIFY=FALSE)
  } 
  
}

# Returns a data frame of parsed SimMatrix
parseFree <- function(simMat,group,start,facLab=NULL,indLab=NULL) {
  free <- simMat@free
  nf <- ncol(free)
  ni <- nrow(free)
  tot <- nf*ni

  id <- start:(start+(ni*nf)-1)

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

  user <- rep(0,tot)
  group <- rep(group,tot)
  free <- freeIdx(free,start=start)   
  ustart <- startingVal(free)
  exo <- rep(0,length(id))
  eq.id <- eqIdx(free,id)
  label <- names(eq.id)
  unco <- uncoIdx(free,start=start)
  return(data.frame(id,lhs,op,rhs,user,group,free,ustart,exo,eq.id,label,unco))
}
  
# Takes a matrix, and returns a logical matrix indicating what elements are labels.
is.label <- function(mat) {
  flat <- as.vector(mat)

  # The basic idea is to parse and evaluate the character string in the global namespace. If the object doesn't exist,
  # it is a constraint label.
  # However, this is a little sketchy. For instance:
  # If the TemporaryVariableName were x instead, if a label was x, this test would fail.
  maybeLabel <- sapply(flat, FUN= function(TemporaryVariableName) { tryCatch(eval(parse(text=TemporaryVariableName)),
                                  error = function(e) 1)})
  isLabel <- tryCatch(as.logical(maybeLabel), error=function(e)
                      stop("Invalid constraint: Label might be a function name or object in global namespace"))
  isLabel[is.na(isLabel)] <- FALSE

  return(isLabel)
}


# Takes a matrix, and returns a logical matrix indicating what elements are free (either NA or label)
is.free <- function(mat) {
  if(is.character(mat)) {
    isFree <- is.na(mat) | is.label(mat)
  } else {
    isFree <- is.na(mat)
  }
  return(isFree)
}


# Calculates the indices of free parameters by lavaan rules.
# 1. Each unique free parameter (NA) gets a unique index
# 2. The first constrained free parameter gets a unique index
# 3. Constrained parameters with identical labels get identical indices
# 4. Fixed parameters are 0
freeIdx <- function(mat, start = 1) {
  flat <- as.vector(mat)
  free.idx <- rep(0,length(flat))
  isLabel <- is.label(flat)
  avail <- seq.int(start,start+length(flat)-1,by=1)
  conList <- NULL
  
  for(i in seq_along(flat)) {
    if(is.na(flat[i])) {
      j <- i
      free.idx[i] <- avail[j]
    } else if(isLabel[i]) {
      label <- flat[i]
      if(is.null(conList[label]) || is.na(conList[label])) {
        j <- j+1
        conList <- c(conList,avail[j])
        names(conList)[length(conList)] <- label
        free.idx[i] <- avail[j]
       } else {
         idx <- conList[label]
         free.idx[i] <- idx
       }
    } else {
      # Do nothing
    }
  }
  return(free.idx)
}

# Calculates the indices for unconstrained parameters
uncoIdx <- function(mat, start=1) {
  flat <- as.vector(mat)
  avail <- seq.int(start,start+length(flat)-1,by=1)
  log <- is.na(flat) | is.label(flat)
  uncoIdx <- rep(0,length(flat))

  if(all(log)) {
    return(avail)
  }  else {
    j <- 1
    for(i in seq_along(flat)) {
      if(log[i]) {
        uncoIdx[i] <- avail[j]
        j <- j+1
      }
    }
  }
  return(uncoIdx)
}

# The parameter index of labels that are the same
eqIdx <- function(mat,id) {
  flat <- as.vector(mat)
  eq.idx <- rep(0,length(flat))
  isLabel <- is.label(flat)
  conList <- NULL

  for(i in seq_along(flat)) {
    if(isLabel[i]) {
      label <- flat[i]
      if(is.null(conList[label]) || is.na(conList[label])) {
        conList <- c(conList,id[i])
        names(conList)[length(conList)] <- label
        eq.idx[i] <- id[i]
        names(eq.idx)[i] <- label
      } else {
         idx <- conList[label]
         eq.idx[i] <- idx
         names(eq.idx)[i] <- label
       }
    } else {
      names(eq.idx)[i] <- ""
    }

  }
  return(eq.idx)
}

## Calculate starting values. Could potentially be made smarter by using population values.
startingVal <- function(free) {
 flat <- as.vector(free)
 flat[is.label(flat)] <- NA
 as.numeric(flat)
}
