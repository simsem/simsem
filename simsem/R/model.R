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

paramSet <- model(LY=cfa$LY,RPS=cfa$RPS,RTE=cfa$RTE, modelType="CFA")
paramSet <- model(LY=cfa2$LY,PS=cfa2$PS,TE=cfa2$TE,AL=cfa2$AL,TY=cfa2$TY, modelType="CFA")
model(BE=path$BE, RPS=path$RPS, ME=path$ME, modelType="Path")
paramSet <- model(LY=sem$LY, RTE=sem$RTE, RPS=sem$RPS, BE=sem$BE, modelType="SEM")
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
        
    if(is.null(paramSet$RTE)) {
      stop("No error correlation object in CFA")
    } else {                              
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

  #2. Convert a chunk at a time - starting with LY - factor loading
  pt <- NULL
  if(is.list(paramSet$LY)) {
    if(is.null(facLab)){lhs <- paste("y",1:ncol(paramSet$LY[[1]]@free),sep="")} else { lhs <- facLab}
    if(is.null(indLab)){rhs <- paste("x",1:nrow(paramSet$LY[[1]]@free),sep="")} else { rhs <- indLab}
    for(i in seq_along(paramSet$LY)) {      
      if(i == 1) { 
        pt <- parseFree(paramSet$LY[[i]],group=i,pt=NULL,op="=~",lhs,rhs)
      } else {
        pt <- rbind(pt, parseFree(paramSet$LY[[i]], group=i, op="=~",pt=pt,lhs, rhs))
      }
    }
  } else if (!is.null(paramSet$LY)) {
    if(is.null(facLab)){lhs <- paste("y",1:ncol(paramSet$LY@free),sep="")} else { lhs <- facLab}
    if(is.null(indLab)){rhs <- paste("x",1:nrow(paramSet$LY@free),sep="")} else { rhs <- indLab}
    pt <- parseFree(paramSet$LY, group=1, pt=NULL,op="=~",lhs,rhs)
  }

  # PS - factor covariance
  if(is.list(paramSet$PS)) {
    if(is.null(facLab)){lhs <- rhs <- paste("y",1:ncol(paramSet$PS[[1]]@free),sep="")} else { lhs <- rhs <- facLab}
     for(i in seq_along(paramSet$PS)) {
       pt <- rbind(pt, parseFree(paramSet$PS[[i]], group=i, op="~~",pt=pt,lhs,rhs))
     }
  } else if (!is.null(paramSet$PS)) {
    if(is.null(facLab)) {lhs <- rhs <- paste("y",1:ncol(paramSet$PS@free),sep="")} else { lhs <- rhs <- facLab}
    pt <- rbind(pt, parseFree(paramSet$PS, group=1, pt=pt,op="~~",lhs,rhs))
  }
  # RPS - factor correlation (same as PS)
  if(is.list(paramSet$RPS)) {
    if(is.null(facLab)){lhs <- rhs <- paste("y",1:ncol(paramSet$RPS[[1]]@free),sep="")} else { lhs <- rhs <- facLab}
     for(i in seq_along(paramSet$RPS)) {
       pt <- rbind(pt, parseFree(paramSet$RPS[[i]], group=i, op="~~",pt=pt,lhs,rhs))
     }
  } else if (!is.null(paramSet$RPS)) {
    if(is.null(facLab)) {lhs <- rhs <- paste("y",1:ncol(paramSet$RPS@free),sep="")} else { lhs <- rhs <- facLab}
    pt <- rbind(pt, parseFree(paramSet$RPS, group=1, pt=pt,op="~~",lhs,rhs))
  }

  # TE - Covariance of measurement error
   if(is.list(paramSet$TE)) {
    if(is.null(facLab)){lhs <- rhs <- paste("x",1:ncol(paramSet$TE[[1]]@free),sep="")} else { lhs <- rhs <- indLab}
     for(i in seq_along(paramSet$TE)) {
       pt <- rbind(pt, parseFree(paramSet$TE[[i]], group=i, op="~~",pt=pt,lhs,rhs))
     }
  } else if (!is.null(paramSet$TE)) {
    if(is.null(facLab)) {lhs <- rhs <- paste("x",1:ncol(paramSet$TE@free),sep="")} else { lhs <- rhs <- indLab}
    pt <- rbind(pt, parseFree(paramSet$TE, group=1, pt=pt,op="~~",lhs,rhs))
  }
  # RTE - Correlation of measurment error
   if(is.list(paramSet$RTE)) {
    if(is.null(facLab)){lhs <- rhs <- paste("x",1:ncol(paramSet$RTE[[1]]@free),sep="")} else { lhs <- rhs <- indLab}
     for(i in seq_along(paramSet$RTE)) {
       pt <- rbind(pt, parseFree(paramSet$RTE[[i]], group=i, op="~~",pt=pt,lhs,rhs))
     }
  } else if (!is.null(paramSet$RTE)) {
    if(is.null(facLab)) {lhs <- rhs <- paste("x",1:ncol(paramSet$RTE@free),sep="")} else { lhs <- rhs <- indLab}
    pt <- rbind(pt, parseFree(paramSet$RTE, group=1, pt=pt,op="~~",lhs,rhs))
  }

  # BE - Regressions among factors
  if(is.list(paramSet$BE)) {
    if(is.null(facLab)){lhs <- rhs <- paste("y",1:ncol(paramSet$BE[[1]]@free),sep="")} else { lhs <- rhs <- facLab}
     for(i in seq_along(paramSet$BE)) {
       pt <- rbind(pt, parseFree(paramSet$BE[[i]], group=i, op="~",pt=pt,lhs,rhs))
     }
  } else if (!is.null(paramSet$BE)) {
    if(is.null(facLab)) {lhs <- rhs <- paste("y",1:ncol(paramSet$BE@free),sep="")} else { lhs <- rhs <- facLab}
    pt <- rbind(pt, parseFree(paramSet$BE, group=1, pt=pt,op="~",lhs,rhs))
  }

  ## AL - factor intercept
   if(is.list(paramSet$AL)) {
    if(is.null(facLab)){lhs <- paste("y",1:length(paramSet$AL[[1]]@free),sep="")} else { lhs <- facLab}
    rhs <- rep("",length(paramSet$AL[[1]]@free))
     for(i in seq_along(paramSet$AL)) {
       pt <- rbind(pt, parseFree(paramSet$AL[[i]], group=i, op="~1",pt=pt,lhs,rhs))
     }
  } else if (!is.null(paramSet$AL)) {
    if(is.null(facLab)) {lhs <- paste("y",1:length(paramSet$AL@free),sep="")} else { lhs <- facLab}
    rhs <- rep("",length(paramSet$AL@free))
    pt <- rbind(pt, parseFree(paramSet$AL, group=1, pt=pt,op="~1",lhs,rhs))
  }

  # TY - indicator intercept
   if(is.list(paramSet$TY)) {
    if(is.null(facLab)){lhs <- paste("x",1:length(paramSet$TY[[1]]@free),sep="")} else { lhs <- indLab}
    rhs <- rep("",length(paramSet$TY[[1]]@free))
     for(i in seq_along(paramSet$TY)) {
       pt <- rbind(pt, parseFree(paramSet$TY[[i]], group=i, op="~1",pt=pt,lhs,rhs = ""))
     }
  } else if (!is.null(paramSet$TY)) {
    if(is.null(facLab)) {lhs <- paste("x",1:length(paramSet$TY@free),sep="")} else { lhs <- indLab}
    rhs <- rep("",length(paramSet$TY@free))
    pt <- rbind(pt, parseFree(paramSet$TY, group=1, pt=pt,op="~1",lhs,rhs))
  }
  return(pt)  
}

# Returns a data frame of parsed SimMatrix
parseFree <- function(simDat,group,pt,op,lhs=NULL,rhs=NULL) {
  # Calculate starting indices from previous pt
  if(!is.null(pt)) {
    startId <- max(pt$id)+1
    startFree <- max(pt$free)+1
    startUnco <- max(pt$unco)+1
  } else {
    startId <- 1
    startFree <- 1
    startUnco <- 1
  }

  freeDat <- simDat@free
  
  if(class(simDat) == "SimVector") {
    tot = length(simDat)
  } else {
      nf <- ncol(freeDat)
      ni <- nrow(freeDat)
      tot <- nf*ni
    }  

  id <- startId:(startId+(tot)-1)
  op <- rep(op,length(id))
  user <- rep(0,tot)
  group <- rep(group,tot)
  free <- freeIdx(freeDat,start=startFree)   
  ustart <- startingVal(freeDat)
  exo <- rep(0,length(id))
  eq.id <- eqIdx(freeDat,id)
  label <- names(eq.id)
  unco <- uncoIdx(freeDat,start=startUnco)
  return(data.frame(id,lhs,op,rhs,user,group,free,ustart,exo,eq.id,label,unco))
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

  j <- 1
  for(i in seq_along(flat)) {
    if(is.na(flat[i])) {
      free.idx[i] <- avail[j]
      j <- j+1
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

# Takes a matrix or vector, and returns a logical vector indicating what elements are labels.
is.label <- function(mat) {
  flat <- as.vector(mat)
  flat[is.na(flat)] <- 0
  isLabel <- sapply(flat, FUN= function(x) {suppressWarnings(is.na(as.numeric(x))) })
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
