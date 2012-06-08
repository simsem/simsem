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
  RPS <- bind(latent.cor, 0.5,"runif(1,0,1)")

  error.cor <- matrix(0, 6, 6)
  diag(error.cor) <- NA
  RTE <- bind(error.cor,popParam=1,misspec="runif(1,0,1)")

  return(list(LY=LY,RPS=RPS,RTE=RTE))
}

## CFA with more matrices
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

holz <- function() {
  loading <- matrix(0,9,3)
  loading[2:3,1] <- NA
  loading[5:6,2] <- NA
  loading[8:9,3] <- NA
  loading[1,1] <- 1
  loading[4,2] <- 1
  loading[7,3] <- 1
  LY <- bind(loading)

  factor.cor <- diag(3)
  diag(factor.cor) <- NA
  factor.cor[lower.tri(factor.cor)] <- NA
  RPS <- bind(factor.cor)

  rte <- diag(9)
  diag(rte) <- NA
  RTE <- bind(rte)

  pt1 <- model(LY=LY,RPS=RPS,RTE=RTE,modelType="CFA")
  
  fit <- lavaan(pt1,data=HolzingerSwineford1939)
}

cfa <- cfa()
cfa2 <- cfa2()
path <- path()
sem <- sem()

tcfa <- model(LY=cfa$LY,RPS=cfa$RPS,RTE=cfa$RTE, modelType="CFA")
tcfa2 <- model(LY=cfa2$LY,PS=cfa2$PS,TE=cfa2$TE,AL=cfa2$AL,TY=cfa2$TY, modelType="CFA")
tpath <- model(BE=path$BE, RPS=path$RPS, ME=path$ME, modelType="Path")
tsem <- model(LY=sem$LY, RTE=sem$RTE, RPS=sem$RPS, BE=sem$BE, modelType="SEM")
tcfamg <- model(LY=list(cfa2$LY,cfa2$LY),PS=list(cfa2$PS,cfa2$PS),TE=cfa2$TE,AL=cfa2$AL,TY=cfa2$TY,modelType="CFA")
tcfamg3 <- model(LY=cfa2$LY,PS=cfa2$PS,TE=cfa2$TE,AL=cfa2$AL,TY=cfa2$TY, modelType="CFA",ngroups=3)


## Takes model specification matrices of type SimMatrix (or lists of these matrices for multiple groups).
## Returns a SimSem object that contains templates for data generation and analyis.
model <- function(LY = NULL,PS = NULL,RPS = NULL, TE = NULL,RTE = NULL, BE = NULL, VTE = NULL, VY = NULL,
                  VPS = NULL, TY = NULL, AL = NULL, MY = NULL, ME = NULL, modelType=NULL, indLab=NULL, facLab=NULL, ngroups=1) {
  
  paramSet <- list(LY=LY, PS=PS, RPS=RPS, TE=TE, RTE=RTE, BE=BE, VTE=VTE, VY=VY, VPS=VPS, TY=TY, AL=AL, MY=MY,ME=ME)
  if(!is.null(modelType)) {
    mg <- NULL
    mgidx <- which(sapply(paramSet,is.list))
    mg <- names(mgidx)
    sgidx <- which(sapply(paramSet,FUN=function(x) {class(x) == "SimMatrix" || class(x) == "SimVector"}))
    sg <- names(sgidx)
    n <- length(mg)
    matNames <- names(paramSet)
    
    if(length(mg) > 0 || ngroups > 1) {

      if(ngroups > 1 && (length(sg) == sum(!sapply(paramSet,is.null))) ) { # ngroups specified, but no mats are lists
       paramSet <- buildModel(paramSet,modelType)
       # Recompute SG indices
       sgidx <- which(sapply(paramSet,FUN=function(x) {class(x) == "SimMatrix" || class(x) == "SimVector"}))
       for(i in seq_along(sgidx)) {
         temp <- NULL
          if(class(paramSet[sgidx][[i]]) == "SimMatrix") {
            temp <- paramSet[sgidx][[i]]
            paramSet[sgidx][[i]] <- replicate(ngroups,new("SimMatrix",free=temp@free,popParam=temp@popParam,misspec=temp@misspec))
          } else {
            temp <- paramSet[sgidx][[i]]
            paramSet[sgidx][[i]] <- replicate(ngroups,new("SimVector",free=temp@free,popParam=temp@popParam,misspec=temp@misspec))
          }
        }
      } else { # 1 or more matrices is a list
        
        if(!length(unique(sapply(paramSet[mgidx],length))) == 1) stop("Multiple group lists have differing lengths")

        for(i in seq_along(sgidx)) {
          temp <- NULL
          if(class(paramSet[sgidx][[i]]) == "SimMatrix") {
            temp <- paramSet[sgidx][[i]]
            paramSet[sgidx][[i]] <- replicate(n,new("SimMatrix",free=temp@free,popParam=temp@popParam,misspec=temp@misspec))
          } else {
            temp <- paramSet[sgidx][[i]]
            paramSet[sgidx][[i]] <- replicate(n,new("SimVector",free=temp@free,popParam=temp@popParam,misspec=temp@misspec))
          }
        }

        temp <- list()
        # Check Models. temp is a list of paramSets of a single group.
        for(i in 1:n) {
          temp[[i]] <- buildModel(lapply(paramSet,"[[",i),modelType)
        }
        # Transform temp back to a paramSet that contains lists of named matrices.
        paramSet <- psetTrans(temp)
      
      }
    } else { # ngroups = 1, and no matrices are lists
     paramSet <- buildModel(paramSet,modelType)
   }
  } else { stop("Must specify model type") }
  
  pt <- buildPT(paramSet)
  
  return(new("SimSem",pt=pt,dgen=combine(paramSet)))
}



## Takes a list of simMatrix/simVector (sg) and a model type and completes necessary matrices and checks
## the validity of the model specification.
buildModel <- function(paramSet,modelType) {
  
 if(modelType == "CFA") {
   
  if(is.null(paramSet$LY)) stop("No loading object in CFA")
  
  ni <- nrow(paramSet$LY@free)
  nk <- ncol(paramSet$LY@free)
  
  if (!is.null(paramSet$TE)) {
    if(!is.null(paramSet$RTE)) stop("Error covariance and error correlation cannot be specified at the same time!")
    if(!is.null(paramSet$VTE)) stop("Error covariance and error variance cannot be specified at the same time!")
    if(!is.null(paramSet$VY)) stop("Error covariance and total indicator variance cannot be specified at the same time!")
  } else {
    if(is.null(paramSet$RTE)) stop("No error correlation object in CFA")
    if(is.null(paramSet$VY)) { paramSet$VY <- bind(rep(NA,ni),popParam=1) } ## Set variance of indicators to be free, pop value of 1
  }

  if(!is.null(paramSet$PS)) {
    if(!is.null(paramSet$RPS)) stop("Factor covariance and factor correlation cannot be specified at the same time!")
    if(!is.null(paramSet$VE)) stop("Factor covariance and factor variance cannot be specified at the same time!")
  } else {
    if(is.null(paramSet$RPS)) stop("No latent variables correlation object in CFA")
    if(is.null(paramSet$VE)) { paramSet$VE <- bind(free=rep(1,nk)) } # Set the latent variances to be fixed to 1  
  }
  
  if(is.null(paramSet$TY)) { paramSet$TY <- bind(free=rep(NA,ni),popParam=0) } # Set measurement intercept to be free, pop value of 0
  if(is.null(paramSet$ME)) { paramSet$ME <- bind(free=rep(0,nk)) } # Set means of factors to be fixed to 0
  if(is.null(paramSet$AL)) { paramSet$AL <- bind(free=rep(0,nk)) } # Set factor intercepts to be fixed to 0
  
} else if (modelType == "Path" ) {
  
  if(is.null(paramSet$BE)) stop("No path coefficient object between factor.ETA")
  ne <- ncol(paramSet$BE@free)
  if(!is.null(paramSet$PS)) {
    if(!is.null(paramSet$RPS)) stop("Covariance and correlation cannot be specified at the same time!")
    if(!is.null(paramSet$VPS)) stop("Covariance and variance cannot be specified at the same time!")
    if(!is.null(paramSet$VE)) stop("Covariance and total indicator variance cannot be specified at the same time!")
  } else {
    if(is.null(paramSet$RPS)) stop("No residual correlation object between factor.ETA")
    if(is.null(paramSet$VE)) { paramSet$VE <- bind(free=rep(NA,ne),popParam = 1) } ## Set latent variance to be free, pop value = 1
  }
  if(is.null(paramSet$AL)) { AL <- bind(rep(NA,ne),popParam=0) } ## Set factor intercepts to be free, pop value = 0
  
} else if (modelType == "SEM") {
  
  if(is.null(paramSet$LY)) stop("No loading object of indicator.Y from factor.ETA in SEM")
  ny <- nrow(paramSet$LY@free)
  ne <- ncol(paramSet$LY@free)

  if(!is.null(paramSet$TE)) {
    if(!is.null(paramSet$RTE)) stop("Error covariance and error correlation cannot be specified at the same time!")
    if(!is.null(paramSet$VTE)) stop("Error covariance and error variance cannot be specified at the same time!")
    if(!is.null(paramSet$VY)) stop("Error covariance and total indicator variance cannot be specified at the same time!")
  } else {
    if(is.null(paramSet$RTE)) stop("No measurement error correlation object between indicator.Y")
    if(is.null(paramSet$VY)) { paramSet$VY <- bind(rep(NA,ny),popParam=1) } ## Set indicator variance to be free, pop value at 1
  }

  if(is.null(paramSet$TY)) { paramSet$TY <- bind(rep(NA,ny),popParam=0) } ## Set measurement intercepts to be free, pop value at 0
  
  if(is.null(paramSet$BE)) stop("No path coefficient object between factor.ETA")

  if(!is.null(paramSet$PS)) {
    if(!is.null(paramSet$RPS)) stop("Error covariance and error correlation cannot be specified at the same time!")
    if(!is.null(paramSet$VPS)) stop("Error covariance and error variance cannot be specified at the same time!")
    if(!is.null(paramSet$VE)) stop("Error covariance and total indicator variance cannot be specified at the same time!")
  } else {
    if(is.null(paramSet$RPS)) stop("No measurement error correlation object between indicator.Y")
    if(is.null(paramSet$VE)) { paramSet$VE <- bind(rep(1,ne)) } ## Set factor variance to be fixed at 1
  }
  if(is.null(paramSet$ME)) { ME <- bind(rep(0,ne)) } ## Set factor means to be fixed at 0
  if(is.null(paramSet$AL)) { AL <- bind(rep(0,ne)) } ## Set factor intercepts to be fixed at 0
} else { stop("modelType not recognized. Possible options are: \"CFA\", \"SEM\", or \"Path\"") }

  return(paramSet)
}

## Takes a list of named simMatrix/simVector objects (paramSet) that are optionally also lists.
## Returns a table of parameters to be used for analysis with lavaan.
buildPT <- function(paramSet, facLab=NULL, indLab=NULL) {

  ##2. Convert a chunk at a time - starting with LY - factor loading
  pt <- NULL
  
  if(is.list(paramSet$LY)) {
    nf <- ncol(paramSet$LY[[1]]@free)
    ni <- nrow(paramSet$LY[[1]]@free)
                 
    if(is.null(facLab)){lhs <- rep(paste("y",1:nf,sep=""),each=ni)} else { lhs <- rep(facLab,each=ni)}
    if(is.null(indLab)){rhs <- rep(paste("x",1:ni,sep=""),times=nf)} else { rhs <- rep(indLab,times=nf)}
    for(i in seq_along(paramSet$LY)) {      
      if(i == 1) { 
        pt <- parseFree(paramSet$LY[[i]],group=i,pt=NULL,op="=~",lhs,rhs)
      } else {
        pt <- rbind(pt, parseFree(paramSet$LY[[i]], group=i, op="=~",pt=pt,lhs, rhs))
      }
    }
  } else if (!is.null(paramSet$LY)) {
    nf <- ncol(paramSet$LY@free)
    ni <- nrow(paramSet$LY@free)
    if(is.null(facLab)){lhs <- rep(paste("y",1:nf,sep=""),each=ni) } else { lhs <- rep(facLab,each=ni)}
    if(is.null(indLab)){rhs <- rep(paste("x",1:ni,sep=""),times=nf) } else { rhs <- rep(indLab,times=nf)}
    pt <- parseFree(paramSet$LY, group=1, pt=NULL,op="=~",lhs,rhs)
  }

  ## PS - factor covariance
  if(is.list(paramSet$PS)) {
    nf <- ncol(paramSet$PS[[1]]@free)
    if(is.null(facLab)){
      lhs <- rep(paste("y",1:nf,sep=""),each=nf)
      rhs <- rep(paste("y",1:nf,sep=""),times=nf)
    } else {
      lhs <- rep(facLab,each=nf)
      rhs <- rep(facLab,times=nf)
    }
    for(i in seq_along(paramSet$PS)) {
      pt <- rbind(pt, parseFree(paramSet$PS[[i]], group=i, op="~~",pt=pt,lhs,rhs))
    }
  } else if (!is.null(paramSet$PS)) {
    nf <- ncol(paramSet$PS@free)
    if(is.null(facLab)){
      lhs <- rep(paste("y",1:nf,sep=""),each=nf)
      rhs <- rep(paste("y",1:nf,sep=""),times=nf)
    } else {
      lhs <- rep(facLab,each=nf)
      rhs <- rep(facLab,times=nf)
    }
    pt <- rbind(pt, parseFree(paramSet$PS, group=1, pt=pt,op="~~",lhs,rhs))
  }

  ## RPS - factor correlation (same as PS)
 if(is.list(paramSet$RPS)) {
    nf <- ncol(paramSet$RPS[[1]]@free)
    if(is.null(facLab)){
      lhs <- rep(paste("y",1:nf,sep=""),each=nf)
      rhs <- rep(paste("y",1:nf,sep=""),times=nf)
    } else {
      lhs <- rep(facLab,each=nf)
      rhs <- rep(facLab,times=nf)
    }
    for(i in seq_along(paramSet$RPS)) {
      pt <- rbind(pt, parseFree(paramSet$RPS[[i]], group=i, op="~~",pt=pt,lhs,rhs))
    }
  } else if (!is.null(paramSet$RPS)) {
    nf <- ncol(paramSet$RPS@free)
    if(is.null(facLab)){
      lhs <- rep(paste("y",1:nf,sep=""),each=nf)
      rhs <- rep(paste("y",1:nf,sep=""),times=nf)
    } else {
      lhs <- rep(facLab,each=nf)
      rhs <- rep(facLab,times=nf)
    }
    pt <- rbind(pt, parseFree(paramSet$RPS, group=1, pt=pt,op="~~",lhs,rhs))
  }

  
  ## TE - Covariance of measurement error
  if(is.list(paramSet$TE)) {
    ni <- ncol(paramSet$TE[[1]]@free)
    if(is.null(facLab)){
      lhs <- rep(paste("x",1:ni,sep=""),each=ni)
      rhs <- rep(paste("x",1:ni,sep=""),times=ni)
    } else {
      lhs <- rep(indLab,each=ni)
      rhs <- rep(indLab,times=ni)
    }
    for(i in seq_along(paramSet$TE)) {
      pt <- rbind(pt, parseFree(paramSet$TE[[i]], group=i, op="~~",pt=pt,lhs,rhs))
    }
  } else if (!is.null(paramSet$TE)) {
     ni <- ncol(paramSet$TE@free)
    if(is.null(facLab)){
      lhs <- rep(paste("x",1:ni,sep=""),each=ni)
      rhs <- rep(paste("x",1:ni,sep=""),times=ni)
    } else {
      lhs <- rep(indLab,each=ni)
      rhs <- rep(indLab,times=ni)
    }
    pt <- rbind(pt, parseFree(paramSet$TE, group=1, pt=pt,op="~~",lhs,rhs))
  }
  
  ## RTE - Correlation of measurment error
  if(is.list(paramSet$RTE)) {
    ni <- ncol(paramSet$RTE[[1]]@free)
    if(is.null(facLab)){
      lhs <- rep(paste("x",1:ni,sep=""),each=ni)
      rhs <- rep(paste("x",1:ni,sep=""),times=ni)
    } else {
      lhs <- rep(indLab,each=ni)
      rhs <- rep(indLab,times=ni)
    }
    for(i in seq_along(paramSet$RTE)) {
      pt <- rbind(pt, parseFree(paramSet$RTE[[i]], group=i, op="~~",pt=pt,lhs,rhs))
    }
  } else if (!is.null(paramSet$RTE)) {
     ni <- ncol(paramSet$RTE@free)
    if(is.null(facLab)){
      lhs <- rep(paste("x",1:ni,sep=""),each=ni)
      rhs <- rep(paste("x",1:ni,sep=""),times=ni)
    } else {
      lhs <- rep(indLab,each=ni)
      rhs <- rep(indLab,times=ni)
    }
    pt <- rbind(pt, parseFree(paramSet$RTE, group=1, pt=pt,op="~~",lhs,rhs))
  }
  

  ## BE - Regressions among factors
  if(is.list(paramSet$BE)) {
    nf <- ncol(paramSet$BE[[1]]@free)
    if(is.null(facLab)){
      lhs <- rep(paste("y",1:nf,sep=""),each=nf)
      rhs <- rep(paste("y",1:nf,sep=""),times=nf)
    } else {
      lhs <- rep(facLab,each=nf)
      rhs <- rep(facLab,times=nf)
    }
    for(i in seq_along(paramSet$BE)) {
      pt <- rbind(pt, parseFree(paramSet$BE[[i]], group=i, op="~",pt=pt,lhs,rhs))
    }
  } else if (!is.null(paramSet$BE)) {
    nf <- ncol(paramSet$BE@free)
    if(is.null(facLab)){
      lhs <- rep(paste("y",1:nf,sep=""),each=nf)
      rhs <- rep(paste("y",1:nf,sep=""),times=nf)
    } else {
      lhs <- rep(facLab,each=nf)
      rhs <- rep(facLab,times=nf)
    }
    pt <- rbind(pt, parseFree(paramSet$BE, group=1, pt=pt,op="~",lhs,rhs))
  }
  

  ## AL - factor intercept
 if(is.list(paramSet$AL)) {
    nf <- length(paramSet$AL[[1]]@free)
    if(is.null(facLab)){
      lhs <- paste("y",1:nf,sep="")
      rhs <- rep("",times=nf)
    } else {
      lhs <- facLab
      rhs <- rep("",times=nf)
    }
    for(i in seq_along(paramSet$AL)) {
      pt <- rbind(pt, parseFree(paramSet$AL[[i]], group=i, op="~1",pt=pt,lhs,rhs))
    }
    
  } else if (!is.null(paramSet$AL)) {
    nf <- length(paramSet$AL@free)
    if(is.null(facLab)){
      lhs <- paste("y",1:nf,sep="")
      rhs <- rep("",times=nf)
    } else {
      lhs <- facLab
      rhs <- rep("",times=nf)
    }
    pt <- rbind(pt, parseFree(paramSet$AL, group=1, pt=pt,op="~1",lhs,rhs))
  }    

  ## TY - indicator intercept
 if(is.list(paramSet$TY)) {
    ni <- length(paramSet$TY[[1]]@free)
    if(is.null(indLab)){
      lhs <- paste("x",1:ni,sep="")
      rhs <- rep("",times=ni)
    } else {
      lhs <- indLab
      rhs <- rep("",times=ni)
    }
    for(i in seq_along(paramSet$TY)) {
      pt <- rbind(pt, parseFree(paramSet$TY[[i]], group=i, op="~1",pt=pt,lhs,rhs))
    }
  } else if (!is.null(paramSet$TY)) {
     ni <- length(paramSet$TY@free)
     if(is.null(indLab)){
       lhs <- paste("x",1:ni,sep="")
       rhs <- rep("",times=ni)
     } else {
       lhs <- indLab
       rhs <- rep("",times=ni)
     }
    pt <- rbind(pt, parseFree(paramSet$TY, group=1, pt=pt,op="~1",lhs,rhs))
  }
  pt <- as.list(pt)
  LIST <- list(id=pt$id, lhs = as.character(pt$lhs),op = as.character(pt$op), rhs=as.character(pt$rhs),
               user=pt$user, group=as.integer(pt$group), free=as.integer(pt$free), ustart=pt$ustart, exo=pt$exo,
               label=as.character(pt$label), eq.id=pt$eq.id, unco=as.integer(pt$unco))
  return(LIST)  
}

## Returns a data frame of parsed SimMatrix/SimVector
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
  
  if(class(simDat) == "SimVector") {
    tot = length(freeDat)
  } else {
    tot <- nrow(freeDat)*ncol(freeDat)
  }  

  id <- startId:(startId+(tot)-1)
  op <- rep(op,length(id))
  user <- rep(0,tot)
  group <- rep(group,tot)
  free <- freeIdx(freeDat,start=startFree)   
  ustart <- startingVal(freeDat,simDat@popParam)
  exo <- rep(0,length(id))
  eq.id <- eqIdx(freeDat,id)
  label <- names(eq.id)
  unco <- uncoIdx(freeDat,start=startUnco)
  return(data.frame(id,lhs,op,rhs,user,group,free,ustart,exo,eq.id,label,unco))
}

##  Calculates the indices of free parameters by lavaan rules.
## 1. Each unique free parameter (NA) gets a unique index
## 2. The first constrained free parameter gets a unique index
## 3. Constrained parameters with identical labels get identical indices
## 4. Fixed parameters are 0
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
      ## Do nothing
    }
  }
  return(free.idx)
}

## Calculates the indices for unconstrained parameters
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

## The parameter index of labels that are the same
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

## Calculate starting values. Needs work, but no time to finish yet.
startingVal <- function(free,popParam) {
  
 flat <- as.vector(free)
 flat[is.label(flat)] <- NA
 flat <- as.numeric(flat)
 if(all(!is.nan(popParam))) { # Check if popParam was specified
    flatPop <- as.vector(popParam)
    suppressWarnings(flatPop <- as.numeric(flatPop))
    flat[is.na(flat)] <- flatPop[is.na(flat)]
  }
 return(flat)
}

## Some stupid data transformation to get me out of the hole I dug myself in.
## Goes from list of paramSet to the original specification of a paramSet that contains lists of named matrices.
psetTrans <- function(x) {
  expand <- unlist(x)
  matNames <- names(expand)
  ind <- sapply(matNames,grep,matNames) # calculate matrix of list indices with a common name. nrows = ngroups
  numMat <- length(unique(matNames))
  ind <- ind[,1:numMat]

  newlist <- list()
  ## For each common index, assign to a list with the correct matrix name
  for(i in 1:numMat) {
    temp <- list()
    for(j in ind[,i]) {
      temp <- c(temp,expand[[j]])
    }
    newlist[[matNames[i]]] <- temp
  }
  ## Return full paramSet.
  paramSet <- list(LY=newlist$LY, PS=newlist$PS, RPS=newlist$RPS, TE=newlist$TE, RTE=newlist$RTE,
                   BE=newlist$BE, VTE=newlist$VTE, VY=newlist$VY, VPS=newlist$VPS, TY=newlist$TY, AL=newlist$AL,
                   MY=newlist$MY, ME=newlist$ME)
  return(paramSet)
}

## Takes a paramSet, and reduces each SimMatrix to just one character or numeric matrix for data generation or list of this matrix (for mg)
combine <- function(paramSet) {
  if(any(sapply(paramSet,is.list))) {
    ng <- max(sapply(paramSet,length))

    for(i in seq_along(paramSet)) {
      if(!is.null(paramSet[[i]])) {
        for(j in 1:ng) {
          pop <- paramSet[[i]][[j]]@popParam
          mis <- paramSet[[i]][[j]]@misspec
          if(all(!is.nan(mis)) && (length(mis) != 0)) {
            pop[is.empty(pop)] <- mis[!is.empty(pop)]
          }
          paramSet[[i]][[j]] <- pop             
        }
      }
    }
  } else {
    for(i in seq_along(paramSet)) {
      if(!is.null(paramSet[[i]])) {
        pop <- paramSet[[i]]@popParam
        mis <- paramSet[[i]]@misspec
        if(all(!is.nan(mis)) && (length(mis) != 0)) {
            pop[is.empty(pop)] <- mis[is.empty(pop)]
          }
        paramSet[[i]] <- pop
      }
    }
  }

  return(paramSet)
}

      
