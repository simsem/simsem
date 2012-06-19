## Takes model specification matrices of type SimMatrix (or lists of these matrices for multiple groups).
## Returns a SimSem object that contains templates for data generation and analyis.
model <- function(LY = NULL,PS = NULL,RPS = NULL, TE = NULL,RTE = NULL, BE = NULL, VTE = NULL, VY = NULL,
                  VPS = NULL, TY = NULL, AL = NULL, MY = NULL, ME = NULL, modelType, indLab=NULL, facLab=NULL, ngroups=1) {
  
  paramSet <- list(LY=LY, PS=PS, RPS=RPS, TE=TE, RTE=RTE, BE=BE, VTE=VTE, VY=VY, VPS=VPS, TY=TY, AL=AL, MY=MY,ME=ME)
  if(!is.null(modelType)) {

    mg <- NULL
    mgidx <- which(sapply(paramSet,is.list))
    mg <- names(mgidx)
    sgidx <- which(sapply(paramSet,FUN=function(x) {class(x) == "SimMatrix" || class(x) == "SimVector"}))
    sg <- names(sgidx)
    n <- max(sapply(paramSet,length))
    matNames <- names(paramSet)
    
    if(length(mg) > 0 || ngroups > 1) {

      if(ngroups > 1 && (length(sg) == sum(!sapply(paramSet,is.null))) ) { # ngroups specified, but no mats are lists
       paramSet <- buildModel(paramSet,modelType)
       psl <- rep(list(paramSet),ngroups)
             
      } else { # 1 or more matrices is a list
        
        if(!length(unique(sapply(paramSet[mgidx],length))) == 1) stop("Multiple group lists have differing lengths")

        # Repeat single matrices
        for(i in seq_along(sgidx)) {
          temp <- NULL
          if(class(paramSet[sgidx][[i]]) == "SimMatrix") {
            temp <- paramSet[sgidx][[i]]
            paramSet[sgidx][[i]] <- replicate(n,new("SimMatrix",free=temp@free,popParam=temp@popParam,misspec=temp@misspec,symmetric=temp@symmetric))
          } else {
            temp <- paramSet[sgidx][[i]]
            paramSet[sgidx][[i]] <- replicate(n,new("SimVector",free=temp@free,popParam=temp@popParam,misspec=temp@misspec))
          }
        }

        # Transform to paramSet to list of parameter sets
        psl <- list()
        for(i in 1:n) { psl[[i]] <- lapply(paramSet,"[[",i) } 
        
        psl <- lapply(psl,buildModel,modelType="CFA")
      
      }
      # Create pt for MG
      pt <- NULL
       
       for(i in seq_along(psl)) {
         if (i == 1) {
           pt <- buildPT(psl[[i]], pt=pt, group=i,facLab=NULL, indLab=NULL)
         } else {
           pt <- mapply(pt,buildPT(psl[[i]], pt=pt, group=i,facLab=NULL, indLab=NULL),FUN=c,SIMPLIFY=FALSE)
         }
       }

      # Adjust indices for between group constraints
      pt <- btwGroupCons(pt)

      return(new("SimSem",pt=pt,dgen=psl,modelType=modelType))
      
    } else { # ngroups = 1, and no matrices are lists
     paramSet <- buildModel(paramSet,modelType)
     pt <- buildPT(paramSet)
     return(new("SimSem",pt=pt,dgen=paramSet,modelType=modelType))
   }
  } else { stop("Must specify model type") } 
}



## Takes a list of simMatrix/simVector (sg) and a model type and completes necessary matrices and checks
## the validity of the model specification.
buildModel <- function(paramSet,modelType) {
  
 if(modelType == "CFA") {
   
  if(is.null(paramSet$LY)) stop("No loading object in CFA")
  
  ni <- nrow(paramSet$LY@free)
  nk <- ncol(paramSet$LY@free)
  
  if (!is.null(paramSet$TE)) {
    stopifnot(paramSet$TE@symmetric)
    if(!is.null(paramSet$RTE)) stop("Error covariance and error correlation cannot be specified at the same time!")
    if(!is.null(paramSet$VTE)) stop("Error covariance and error variance cannot be specified at the same time!")
    if(!is.null(paramSet$VY)) stop("Error covariance and total indicator variance cannot be specified at the same time!")
  } else {
    if(is.null(paramSet$RTE)) stop("No error correlation object in CFA")
    stopifnot(paramSet$RTE@symmetric)
    if(is.null(paramSet$VY)) { paramSet$VY <- bind(rep(NA,ni),popParam=1) } ## Set variance of indicators to be free, pop value of 1
  }

  if(!is.null(paramSet$PS)) {
    stopifnot(paramSet$PS@symmetric)
    if(!is.null(paramSet$RPS)) stop("Factor covariance and factor correlation cannot be specified at the same time!")
    if(!is.null(paramSet$VE)) stop("Factor covariance and factor variance cannot be specified at the same time!")
  } else {
    if(is.null(paramSet$RPS)) stop("No latent variables correlation object in CFA")
    stopifnot(paramSet$RPS@symmetric)
    if(is.null(paramSet$VE)) { paramSet$VE <- bind(free=rep(1,nk)) } # Set the latent variances to be fixed to 1  
  }
  
  if(is.null(paramSet$TY)) { paramSet$TY <- bind(free=rep(NA,ni),popParam=0) } # Set measurement intercept to be free, pop value of 0
  #if(is.null(paramSet$ME)) { paramSet$ME <- bind(free=rep(0,nk)) } # Set means of factors to be fixed to 0
  if(is.null(paramSet$AL)) { paramSet$AL <- bind(free=rep(0,nk)) } # Set factor intercepts to be fixed to 0
  
} else if (modelType == "Path" ) {
  
  if(is.null(paramSet$BE)) stop("No path coefficient object between factor.ETA")
  ne <- ncol(paramSet$BE@free)
  if(!is.null(paramSet$PS)) {
    stopifnot(paramSet$PS@symmetric)
    if(!is.null(paramSet$RPS)) stop("Covariance and correlation cannot be specified at the same time!")
    if(!is.null(paramSet$VPS)) stop("Covariance and variance cannot be specified at the same time!")
    if(!is.null(paramSet$VE)) stop("Covariance and total indicator variance cannot be specified at the same time!")
  } else {
    if(is.null(paramSet$RPS)) stop("No residual correlation object between factor.ETA")
    stopifnot(paramSet$RPS@symmetric)
    if(is.null(paramSet$VE)) { paramSet$VE <- bind(free=rep(NA,ne),popParam = 1) } ## Set latent variance to be free, pop value = 1
  }
  if(is.null(paramSet$AL)) { AL <- bind(rep(NA,ne),popParam=0) } ## Set factor intercepts to be free, pop value = 0
  
} else if (modelType == "SEM") {
  
  if(is.null(paramSet$LY)) stop("No loading object of indicator.Y from factor.ETA in SEM")
  ny <- nrow(paramSet$LY@free)
  ne <- ncol(paramSet$LY@free)

  if(!is.null(paramSet$TE)) {
    stopifnot(paramSet$TE@symmetric)
    if(!is.null(paramSet$RTE)) stop("Error covariance and error correlation cannot be specified at the same time!")
    if(!is.null(paramSet$VTE)) stop("Error covariance and error variance cannot be specified at the same time!")
    if(!is.null(paramSet$VY)) stop("Error covariance and total indicator variance cannot be specified at the same time!")
  } else {
    if(is.null(paramSet$RTE)) stop("No measurement error correlation object between indicator.Y")
    stopifnot(paramSet$RTE@symmetric)
    if(is.null(paramSet$VY)) { paramSet$VY <- bind(rep(NA,ny),popParam=1) } ## Set indicator variance to be free, pop value at 1
  }

  if(is.null(paramSet$TY)) { paramSet$TY <- bind(rep(NA,ny),popParam=0) } ## Set measurement intercepts to be free, pop value at 0
  
  if(is.null(paramSet$BE)) stop("No path coefficient object between factor.ETA")

  if(!is.null(paramSet$PS)) {
    stopifnot(paramSet$PS@symmetric)
    if(!is.null(paramSet$RPS)) stop("Error covariance and error correlation cannot be specified at the same time!")
    if(!is.null(paramSet$VPS)) stop("Error covariance and error variance cannot be specified at the same time!")
    if(!is.null(paramSet$VE)) stop("Error covariance and total indicator variance cannot be specified at the same time!")
  } else {
    if(is.null(paramSet$RPS)) stop("No measurement error correlation object between indicator.Y")
    stopifnot(paramSet$RPS@symmetric)
    if(is.null(paramSet$VE)) { paramSet$VE <- bind(rep(1,ne)) } ## Set factor variance to be fixed at 1
  }
  if(is.null(paramSet$ME)) { ME <- bind(rep(0,ne)) } ## Set factor means to be fixed at 0
  #if(is.null(paramSet$AL)) { AL <- bind(rep(0,ne)) } ## Set factor intercepts to be fixed at 0
} else { stop("modelType not recognized. Possible options are: \"CFA\", \"SEM\", or \"Path\"") }

  return(paramSet)
}

## Takes a list of named simMatrix/simVector objects (paramSet) that are optionally also lists.
## Returns a table of parameters to be used for analysis with lavaan.
## This time, PT will only take a sg paramSet. And while I'm at it, I'm taking out the df stuff.

buildPT <- function(paramSet, pt=NULL, group=1,facLab=NULL, indLab=NULL) {

  ## Convert a chunk at a time - starting with LY - factor loading. At least LY,PS/RPS must be specified.
  
  if (!is.null(paramSet$LY)) {
    nf <- ncol(paramSet$LY@free)
    ni <- nrow(paramSet$LY@free)
    if(is.null(facLab)){lhs <- rep(paste("y",1:nf,sep=""),each=ni) } else { lhs <- rep(facLab,each=ni)}
    if(is.null(indLab)){rhs <- rep(paste("x",1:ni,sep=""),times=nf) } else { rhs <- rep(indLab,times=nf)}
    pt <- parseFree(paramSet$LY, group=group, pt=pt,op="=~",lhs,rhs)
  }

  ## PS - factor covariance: Symmetric
  if (!is.null(paramSet$PS)) {
    nf <- ncol(paramSet$PS@free)
    if(is.null(facLab)){
      lhs <- paste0("y",rep(1:nf,nf:1))
      rhs <- paste0("y",unlist(lapply(1:nf,function(k) (1:nf)[k:nf])))
    } else {
      lhs <- rep(facLab,nf:1)
      rhs <- unlist(lapply(1:nf,function(k) facLab[k:nf]))
    }
    if(!is.null(pt)) {
      pt <- mapply(pt, parseFree(paramSet$PS, group=group, pt=pt,op="~~",lhs,rhs),FUN=c,SIMPLIFY=FALSE)
    } else {
      pt <- parseFree(paramSet$PS, group=group, pt=pt,op="~~",lhs,rhs)
    }
  }

  ## RPS - factor correlation (same as PS): Symmetric
  if (!is.null(paramSet$RPS)) {
    nf <- ncol(paramSet$RPS@free)
    if(is.null(facLab)){
      lhs <- paste0("y",rep(1:nf,nf:1))
      rhs <- paste0("y",unlist(lapply(1:nf,function(k) (1:nf)[k:nf])))
    } else {
      lhs <- rep(facLab,nf:1)
      rhs <- unlist(lapply(1:nf,function(k) facLab[k:nf]))
    }
    if(!is.null(pt)) {
      pt <- mapply(pt, parseFree(paramSet$RPS, group=group, pt=pt,op="~~",lhs,rhs),FUN=c,SIMPLIFY=FALSE)
    } else {
      pt <- parseFree(paramSet$RPS, group=group, pt=pt,op="~~",lhs,rhs)
    }
  }

  
  ## TE - Covariance of measurement error: Symmetric
   if (!is.null(paramSet$TE)) {
    ni <- ncol(paramSet$TE@free)
    if(is.null(indLab)){
      lhs <- paste0("x",rep(1:ni,ni:1))
      rhs <- paste0("x",unlist(lapply(1:ni,function(k) (1:ni)[k:ni])))
    } else {
      lhs <- rep(indLab,ni:1)
      rhs <- unlist(lapply(1:ni,function(k) indLab[k:ni]))
    }
    pt <- mapply(pt, parseFree(paramSet$TE, group=group, pt=pt,op="~~",lhs,rhs),FUN=c,SIMPLIFY=FALSE)
  }
  
  ## RTE - Correlation of measurment error: Symmetric
 if (!is.null(paramSet$RTE)) {
     ni <- ncol(paramSet$RTE@free)
     if(is.null(indLab)){
       lhs <- paste0("x",rep(1:ni,ni:1))
       rhs <- paste0("x",unlist(lapply(1:ni,function(k) (1:ni)[k:ni])))
     } else {
       lhs <- rep(indLab,ni:1)
       rhs <- unlist(lapply(1:ni,function(k) indLab[k:ni]))
     }
    pt <- mapply(pt, parseFree(paramSet$RTE, group=group, pt=pt,op="~~",lhs,rhs),FUN=c,SIMPLIFY=FALSE)
  }
  

  ## BE - Regressions among factors
  if (!is.null(paramSet$BE)) {
    nf <- ncol(paramSet$BE@free)
    if(is.null(facLab)){
      lhs <- rep(paste("y",1:nf,sep=""),each=nf)
      rhs <- rep(paste("y",1:nf,sep=""),times=nf)
    } else {
      lhs <- rep(facLab,each=nf)
      rhs <- rep(facLab,times=nf)
    }
    pt <- mapply(pt, parseFree(paramSet$BE, group=group, pt=pt,op="~",lhs,rhs),FUN=c,SIMPLIFY=FALSE)
  }
  

  ## AL - factor intercept
 if (!is.null(paramSet$AL)) {
    nf <- length(paramSet$AL@free)
    if(is.null(facLab)){
      lhs <- paste("y",1:nf,sep="")
      rhs <- rep("",times=nf)
    } else {
      lhs <- facLab
      rhs <- rep("",times=nf)
    }
    pt <- mapply(pt, parseFree(paramSet$AL, group=group, pt=pt,op="~1",lhs,rhs),FUN=c,SIMPLIFY=FALSE)
  }    

  ## TY - indicator intercept
  if (!is.null(paramSet$TY)) {
     ni <- length(paramSet$TY@free)
     if(is.null(indLab)){
       lhs <- paste("x",1:ni,sep="")
       rhs <- rep("",times=ni)
     } else {
       lhs <- indLab
       rhs <- rep("",times=ni)
     }
    pt <- mapply(pt, parseFree(paramSet$TY, group=group, pt=pt,op="~1",lhs,rhs),FUN=c,SIMPLIFY=FALSE)
  }
  return(pt)  
}

## Returns a pt (list) of parsed SimMatrix/SimVector
parseFree <- function(simDat,group,pt,op,lhs=NULL,rhs=NULL) {
  ## Calculate starting indices from previous pt
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
  numElem <- NULL
  
  if(class(simDat) == "SimVector") {
    numElem <- length(freeDat)
  } else if (simDat@symmetric) { # Just get lower tri
    numElem <- nrow(freeDat)*(nrow(freeDat)+1)/2
  } else {
    numElem <- nrow(freeDat)*ncol(freeDat)
  }

  id <- startId:(startId+(numElem)-1)
  op <- rep(op,length(id))
  user <- rep(0,numElem)
  group <- rep(group,numElem)
  free <- freeIdx(freeDat,start=startFree)   
  ustart <- startingVal(freeDat,simDat@popParam)
  exo <- rep(0,length(id))
  eq.id <- eqIdx(freeDat,id)
  label <- names(eq.id)
  eq.id <- as.vector(eq.id)
  unco <- uncoIdx(freeDat,start=startUnco)
  return(list(id=id,lhs=as.character(lhs),op=as.character(op),rhs=as.character(rhs),user=user,group=as.integer(group),
              free=as.integer(free),ustart=ustart,exo=exo,eq.id=eq.id,label=as.character(label),unco=as.integer(unco)))
}

##  Calculates the indices of free parameters by lavaan rules.
## 1. Each unique free parameter (NA) gets a unique index
## 2. The first constrained free parameter gets a unique index
## 3. Constrained parameters with identical labels get identical indices
## 4. Fixed parameters are 0
freeIdx <- function(mat, start = 1) {
  if(is.matrix(mat) && isSymmetric(mat)) {
    flat <- as.vector(mat[lower.tri(mat,diag=TRUE)])
  } else {
    flat <- as.vector(mat)
  }
  
  free.idx <- rep(0,length(flat))
  avail <- seq.int(start,start+length(flat)-1,by=1)
  isLabel <- is.label(flat)
  
  conList <- NULL

  j <- 1
  for(i in seq_along(flat)) {
    if(is.na(flat[i])) {
      free.idx[i] <- avail[j]
      j <- j+1
    } else if(isLabel[i]) {
      label <- flat[i]
      if(is.null(conList[label]) || is.na(conList[label])) {       
        conList <- c(conList,avail[j])
        names(conList)[length(conList)] <- label
        free.idx[i] <- avail[j]
        j <- j+1
      } else {
        idx <- conList[label]
        free.idx[i] <- idx
      }
    } else {
      ## Do nothing
    }
  }
  return(free.idx)
}

## Calculates the indices for unconstrained parameters
uncoIdx <- function(mat, start=1) {

  if(is.matrix(mat) && isSymmetric(mat)) {
    flat <- as.vector(mat[lower.tri(mat,diag=TRUE)])
  } else {
    flat <- as.vector(mat)
  }
  
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

## The parameter index of labels that are the same
eqIdx <- function(mat,id) {

  if(is.matrix(mat) && isSymmetric(mat)) {
    flat <- as.vector(mat[lower.tri(mat,diag=TRUE)])
  } else {
    flat <- as.vector(mat)
  }
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

## Calculate starting values. Needs work, but no time to finish yet.
startingVal <- function(free,popParam) {
  if(is.matrix(free) && isSymmetric(free)) {
    flat <- as.vector(free[lower.tri(free,diag=TRUE)])
    flat[is.label(flat)] <- NA
    flat <- as.numeric(flat)
    
    if(all(!is.nan(popParam))) { # Check if popParam was specified
      flatPop <- as.vector(popParam[lower.tri(popParam,diag=TRUE)])
      suppressWarnings(flatPop <- as.numeric(flatPop))
      flat[is.na(flat)] <- flatPop[is.na(flat)]
    }
  } else {
    flat <- as.vector(free)
    flat[is.label(flat)] <- NA
    flat <- as.numeric(flat)
    
    if(all(!is.nan(popParam))) { # Check if popParam was specified
      flatPop <- as.vector(popParam)
      suppressWarnings(flatPop <- as.numeric(flatPop))
      flat[is.na(flat)] <- flatPop[is.na(flat)]
    }
  }
  flat
}

# Adjusts pt for between group constraints (if they exist). Adjusting here is not very elegant and should probably be reworked into everything else.
btwGroupCons <- function(pt) {

  ngroups <- max(pt$group)
  labelids <- which(pt$eq.id != 0)
  labels <- pt$label[labelids]
  paramsPerGroup <- max(pt$id)/ngroups
  updatedRows <- NULL
  usedFreeId <- NULL  

  for(i in seq_along(unique(labels))) {
    ident <- labelids[labels==unique(labels)[i]]
    pt$eq.id[ident] <- rep(pt$eq.id[ident][1],length(pt$eq.id[ident]))
    free <- pt$free[ident][1]
    pt$free[ident] <- rep(free,length(pt$free[ident]))
    if(length(ident) != 1) {
      updatedRows <- append(updatedRows,ident)
      usedFreeId <- append(usedFreeId,free)
    }
  }
  
  elRows <- pt$id[which(pt$free != 0)] # Rows that are free
  elRows <- elRows[-match(updatedRows,elRows)] # Remove rows that have been updated
  pt$free[elRows] <- (1:(length(elRows)+length(usedFreeId)))[-usedFreeId] #Remove used free ids from available list of ids
  return(pt)
}
