## Takes model specification matrices of type SimMatrix (or lists of these
## matrices for multiple groups).  Returns a SimSem object that contains
## templates for data generation and analyis.
model <- function(LY = NULL, PS = NULL, RPS = NULL, TE = NULL, RTE = NULL, BE = NULL, 
    VTE = NULL, VY = NULL, VPS = NULL, VE = NULL, TY = NULL, AL = NULL, MY = NULL, 
    ME = NULL, modelType, indLab = NULL, facLab = NULL, groupLab = "group", ngroups = 1, 
    smartStart = TRUE) {
    
    paramSet <- list(LY = LY, PS = PS, RPS = RPS, TE = TE, RTE = RTE, BE = BE, VTE = VTE, 
        VY = VY, VPS = VPS, VE = VE, TY = TY, AL = AL, MY = MY, ME = ME)
    if (!is.null(modelType)) {
        
        mg <- NULL
        mgidx <- which(sapply(paramSet, is.list))
        mg <- names(mgidx)
        sgidx <- which(sapply(paramSet, FUN = function(x) {
            class(x) == "SimMatrix" || class(x) == "SimVector"
        }))
        sg <- names(sgidx)
        n <- max(sapply(paramSet, length))
        matNames <- names(paramSet)
        
        if (length(mg) > 0 || ngroups > 1) {
            
            if (ngroups > 1 && (length(sg) == sum(!sapply(paramSet, is.null)))) {
                # ngroups specified, but no mats are lists
                paramSet <- buildModel(paramSet, modelType)
                psl <- rep(list(paramSet), ngroups)
                
            } else {
                # 1 or more matrices is a list
                
                if (!length(unique(sapply(paramSet[mgidx], length))) == 1) 
                  stop("Multiple group lists have differing lengths")
                
                # Repeat single matrices
                for (i in seq_along(sgidx)) {
                  temp <- NULL
                  if (class(paramSet[sgidx][[i]]) == "SimMatrix") {
                    temp <- paramSet[sgidx][[i]]
                    paramSet[sgidx][[i]] <- replicate(n, new("SimMatrix", free = temp@free, 
                      popParam = temp@popParam, misspec = temp@misspec, symmetric = temp@symmetric))
                  } else {
                    temp <- paramSet[sgidx][[i]]
                    paramSet[sgidx][[i]] <- replicate(n, new("SimVector", free = temp@free, 
                      popParam = temp@popParam, misspec = temp@misspec))
                  }
                }
                
                # Transform to paramSet to list of parameter sets
                psl <- list()
                for (i in 1:n) {
                  psl[[i]] <- lapply(paramSet, "[[", i)
                }
                
                psl <- lapply(psl, buildModel, modelType = modelType)
                
            }
            # Create pt for MG
            pt <- NULL
            for (i in seq_along(psl)) {
                if (i == 1) {
                  pt <- buildPT(psl[[i]], pt = pt, group = i, facLab = facLab, indLab = indLab, 
                    smart = smartStart)
                } else {
                  pt <- mapply(pt, buildPT(psl[[i]], pt = pt, group = i, facLab = facLab, 
                    indLab = indLab, smart = smartStart), FUN = c, SIMPLIFY = FALSE)
                }
            }
            
            # Adjust indices for between group constraints
            pt <- btwGroupCons(pt)
            # nullpt <- nullpt(psl[[1]], ngroups=n)
            
            return(new("SimSem", pt = pt, dgen = psl, modelType = modelType, groupLab = groupLab))
            
        } else {
            # ngroups = 1, and no matrices are lists
            paramSet <- buildModel(paramSet, modelType)
            pt <- buildPT(paramSet, smart = smartStart)
            # nullpt <- nullpt(paramSet)
            return(new("SimSem", pt = pt, dgen = paramSet, modelType = modelType, 
                groupLab = groupLab))
        }
    } else {
        stop("Must specify model type")
    }
}



## Takes a list of simMatrix/simVector (sg) and a model type and completes
## necessary matrices and checks the validity of the model specification.
buildModel <- function(paramSet, modelType) {
    
    if (modelType == "CFA") {
        
        if (is.null(paramSet$LY)) 
            stop("No loading object in CFA")
        
        ni <- nrow(paramSet$LY@free)
        nk <- ncol(paramSet$LY@free)
        
        if (!is.null(paramSet$TE)) {
            stopifnot(paramSet$TE@symmetric)
            if (!is.null(paramSet$RTE)) 
                stop("Error covariance and error correlation cannot be specified at the same time!")
            if (!is.null(paramSet$VTE)) 
                stop("Error covariance and error variance cannot be specified at the same time!")
            if (!is.null(paramSet$VY)) 
                stop("Error covariance and total indicator variance cannot be specified at the same time!")
        } else {
            if (is.null(paramSet$RTE)) 
                stop("No error correlation object in CFA")
            stopifnot(paramSet$RTE@symmetric)
            if (is.null(paramSet$VTE) && is.null(paramSet$VY)) 
                {
                  paramSet$VY <- bind(rep(NA, ni), popParam = 1)
                }  ## Set variance of indicators to be free, pop value of 1
        }
        
        if (!is.null(paramSet$PS)) {
            stopifnot(paramSet$PS@symmetric)
            if (!is.null(paramSet$RPS)) 
                stop("Factor covariance and factor correlation cannot be specified at the same time!")
            if (!is.null(paramSet$VE)) 
                stop("Factor covariance and total factor variance cannot be specified at the same time!")
        } else {
            if (is.null(paramSet$RPS)) 
                stop("In CFA, must specify either PS or RPS")
            stopifnot(paramSet$RPS@symmetric)
            if (!is.null(paramSet$VPS)) {
                paramSet$VE <- paramSet$VPS
            }
            if (is.null(paramSet$VE)) 
                {
                  paramSet$VE <- bind(free = rep(1, nk))
                }  # Set the latent variances to be fixed to 1
            if (is.null(paramSet$VPS)) 
                paramSet$VPS <- paramSet$VE
        }
        
        if (is.null(paramSet$MY) && is.null(paramSet$TY)) 
            {
                paramSet$TY <- bind(free = rep(NA, ni), popParam = 0)
            }  # Set measurement intercept to be free, pop value of 0
        # if(is.null(paramSet$ME)) { paramSet$ME <- bind(free=rep(0,nk)) } # Set means
        # of factors to be fixed to 0
        if (!is.null(paramSet$ME)) {
            paramSet$AL <- paramSet$ME
        }
        if (is.null(paramSet$AL)) 
            {
                paramSet$AL <- bind(free = rep(0, nk))
            }  # Set factor intercepts to be fixed to 0
        if (is.null(paramSet$ME)) {
            paramSet$ME <- paramSet$AL
        }
        
    } else if (modelType == "Path") {
        
        if (is.null(paramSet$BE)) 
            stop("No path coefficient object between factor.ETA")
        ne <- ncol(paramSet$BE@free)
        if (!is.null(paramSet$PS)) {
            stopifnot(paramSet$PS@symmetric)
            if (!is.null(paramSet$RPS)) 
                stop("Covariance and correlation cannot be specified at the same time!")
            if (!is.null(paramSet$VPS)) 
                stop("Covariance and variance cannot be specified at the same time!")
            if (!is.null(paramSet$VE)) 
                stop("Covariance and total factor variance cannot be specified at the same time!")
        } else {
            if (is.null(paramSet$RPS)) 
                stop("No residual correlation object between factor.ETA")
            stopifnot(paramSet$RPS@symmetric)
            if (is.null(paramSet$VPS) && is.null(paramSet$VE)) 
                {
                  paramSet$VE <- bind(free = rep(NA, ne), popParam = 1)
                }  ## Set latent variance to be free, pop value = 1
        }
        if (is.null(paramSet$ME) && is.null(paramSet$AL)) 
            {
                paramSet$ME <- bind(rep(NA, ne), popParam = 0)
            }  ## Set factor intercepts to be free, pop value = 0
        
    } else if (modelType == "SEM") {
        
        if (is.null(paramSet$LY)) 
            stop("No loading object of indicator.Y from factor.ETA in SEM")
        ny <- nrow(paramSet$LY@free)
        ne <- ncol(paramSet$LY@free)
        
        if (!is.null(paramSet$TE)) {
            stopifnot(paramSet$TE@symmetric)
            if (!is.null(paramSet$RTE)) 
                stop("Error covariance and error correlation cannot be specified at the same time!")
            if (!is.null(paramSet$VTE)) 
                stop("Error covariance and error variance cannot be specified at the same time!")
            if (!is.null(paramSet$VY)) 
                stop("Error covariance and total indicator variance cannot be specified at the same time!")
        } else {
            if (is.null(paramSet$RTE)) 
                stop("No measurement error correlation object between indicator.Y")
            stopifnot(paramSet$RTE@symmetric)
            if (is.null(paramSet$VTE) && is.null(paramSet$VY)) 
                {
                  paramSet$VY <- bind(rep(NA, ny), popParam = 1)
                }  ## Set indicator variance to be free, pop value at 1
        }
        
        if (is.null(paramSet$MY) && is.null(paramSet$TY)) 
            {
                paramSet$TY <- bind(rep(NA, ny), popParam = 0)
            }  ## Set measurement intercepts to be free, pop value at 0
        
        if (is.null(paramSet$BE)) 
            stop("No path coefficient object between factor.ETA")
        
        if (!is.null(paramSet$PS)) {
            stopifnot(paramSet$PS@symmetric)
            if (!is.null(paramSet$RPS)) 
                stop("Error covariance and error correlation cannot be specified at the same time!")
            if (!is.null(paramSet$VPS)) 
                stop("Error covariance and error variance cannot be specified at the same time!")
            if (!is.null(paramSet$VE)) 
                stop("Error covariance and total indicator variance cannot be specified at the same time!")
        } else {
            if (is.null(paramSet$RPS)) 
                stop("No measurement error correlation object between indicator.Y")
            stopifnot(paramSet$RPS@symmetric)
            if (is.null(paramSet$VPS) && is.null(paramSet$VE)) 
                {
                  paramSet$VE <- bind(rep(1, ne))
                }  ## Set factor variance to be fixed at 1
        }
        if (is.null(paramSet$AL) && is.null(paramSet$ME)) 
            {
                paramSet$ME <- bind(rep(0, ne))
            }  ## Set factor means to be fixed at 0
        # if(is.null(paramSet$AL)) { AL <- bind(rep(0,ne)) } ## Set factor intercepts
        # to be fixed at 0
    } else {
        stop("modelType not recognized. Possible options are: \"CFA\", \"SEM\", or \"Path\"")
    }
    
    return(paramSet)
}

## Takes a list of named simMatrix/simVector objects (paramSet) that are
## optionally also lists.  Returns a table of parameters to be used for
## analysis with lavaan.  This time, PT will only take a sg paramSet. And while
## I'm at it, I'm taking out the df stuff.

buildPT <- function(paramSet, pt = NULL, group = 1, facLab = NULL, indLab = NULL, 
    smart = TRUE) {
    
    ## Convert a chunk at a time - starting with LY - factor loading. At least
    ## LY,PS/RPS must be specified.
    if (!is.null(paramSet$LY)) {
        nf <- ncol(paramSet$LY@free)
        ni <- nrow(paramSet$LY@free)
        if (is.null(facLab)) {
            lhs <- rep(paste("f", 1:nf, sep = ""), each = ni)
        } else {
            lhs <- rep(facLab, each = ni)
        }
        if (is.null(indLab)) {
            rhs <- rep(paste("y", 1:ni, sep = ""), times = nf)
        } else {
            rhs <- rep(indLab, times = nf)
        }
        pt <- parseFree(paramSet$LY, group = group, pt = pt, op = "=~", lhs, rhs, 
            smart = smart)
    }
    
    ## PS - factor covariance: Symmetric
    if (!is.null(paramSet$PS)) {
        nf <- ncol(paramSet$PS@free)
        if (is.null(facLab)) {
            lhs <- paste0("f", rep(1:nf, nf:1))
            rhs <- paste0("f", unlist(lapply(1:nf, function(k) (1:nf)[k:nf])))
        } else {
            lhs <- rep(facLab, nf:1)
            rhs <- unlist(lapply(1:nf, function(k) facLab[k:nf]))
        }
        if (!is.null(pt)) {
            if (!is.null(paramSet$LY)) {
                pt <- mapply(pt, parseFree(paramSet$PS, group = group, pt = pt, op = "~~", 
                  lhs, rhs, smart = smart), FUN = c, SIMPLIFY = FALSE)
            } else {
                pt <- parseFree(paramSet$PS, group = group, pt = pt, op = "~~", lhs, 
                  rhs, smart = smart)
            }
        } else {
            pt <- parseFree(paramSet$PS, group = group, pt = pt, op = "~~", lhs, 
                rhs, smart = smart)
        }
    }
    
    ## RPS - factor correlation (same as PS): Symmetric
    if (!is.null(paramSet$RPS)) {
        # Step 1: parse variance information to the RPS
        if (!is.null(paramSet$VPS)) {
            diag(paramSet$RPS@free) <- paramSet$VPS@free
        } else if (!is.null(paramSet$VE)) {
            # Intentionally use else if to select either VPS or VE
            diag(paramSet$RPS@free) <- paramSet$VE@free
        }
        
        # Step 2: create pt
        nf <- ncol(paramSet$RPS@free)
        if (is.null(facLab)) {
            lhs <- paste0("f", rep(1:nf, nf:1))
            rhs <- paste0("f", unlist(lapply(1:nf, function(k) (1:nf)[k:nf])))
        } else {
            lhs <- rep(facLab, nf:1)
            rhs <- unlist(lapply(1:nf, function(k) facLab[k:nf]))
        }
        if (!is.null(pt)) {
            if (!is.null(paramSet$LY)) {
                pt <- mapply(pt, parseFree(paramSet$RPS, group = group, pt = pt, 
                  op = "~~", lhs, rhs, smart = FALSE), FUN = c, SIMPLIFY = FALSE)
            } else {
                pt <- parseFree(paramSet$RPS, group = group, pt = pt, op = "~~", 
                  lhs, rhs, smart = FALSE)
            }
        } else {
            pt <- parseFree(paramSet$RPS, group = group, pt = pt, op = "~~", lhs, 
                rhs, smart = FALSE)
        }
    }
    
    
    ## TE - Covariance of measurement error: Symmetric
    if (!is.null(paramSet$TE)) {
        ni <- ncol(paramSet$TE@free)
        if (is.null(indLab)) {
            lhs <- paste0("y", rep(1:ni, ni:1))
            rhs <- paste0("y", unlist(lapply(1:ni, function(k) (1:ni)[k:ni])))
        } else {
            lhs <- rep(indLab, ni:1)
            rhs <- unlist(lapply(1:ni, function(k) indLab[k:ni]))
        }
        if (!is.null(pt)) {
            pt <- mapply(pt, parseFree(paramSet$TE, group = group, pt = pt, op = "~~", 
                lhs, rhs, smart = smart), FUN = c, SIMPLIFY = FALSE)
        } else {
            pt <- parseFree(paramSet$TE, group = group, pt = pt, op = "~~", lhs, 
                rhs, smart = smart)
        }
    }
    
    ## RTE - Correlation of measurment error: Symmetric
    if (!is.null(paramSet$RTE)) {
        # Step 1: parse variance information to the RTE
        if (!is.null(paramSet$VTE)) {
            diag(paramSet$RTE@free) <- paramSet$VTE@free
        } else if (!is.null(paramSet$VY)) {
            # Intentionally use else if to select either VPS or VE
            diag(paramSet$RTE@free) <- paramSet$VY@free
        }
        
        # Step 2: create pt
        ni <- ncol(paramSet$RTE@free)
        if (is.null(indLab)) {
            lhs <- paste0("y", rep(1:ni, ni:1))
            rhs <- paste0("y", unlist(lapply(1:ni, function(k) (1:ni)[k:ni])))
        } else {
            lhs <- rep(indLab, ni:1)
            rhs <- unlist(lapply(1:ni, function(k) indLab[k:ni]))
        }
        pt <- mapply(pt, parseFree(paramSet$RTE, group = group, pt = pt, op = "~~", 
            lhs, rhs, smart = FALSE), FUN = c, SIMPLIFY = FALSE)
    }
    
    
    ## BE - Regressions among factors
    if (!is.null(paramSet$BE)) {
        nf <- ncol(paramSet$BE@free)
        if (is.null(facLab)) {
            lhs <- rep(paste("f", 1:nf, sep = ""), each = nf)
            rhs <- rep(paste("f", 1:nf, sep = ""), times = nf)
        } else {
            lhs <- rep(facLab, each = nf)
            rhs <- rep(facLab, times = nf)
        }
        pt <- mapply(pt, parseFree(paramSet$BE, group = group, pt = pt, op = "~", 
            lhs, rhs, smart = smart, swap = TRUE), FUN = c, SIMPLIFY = FALSE)
    }
    
    
    ## AL - factor intercept
    
    # if ME is not null but AL is null
    tempSmart <- smart
    if (!is.null(paramSet$ME) && is.null(paramSet$AL)) {
        paramSet$AL <- paramSet$ME
        tempSmart <- FALSE
    }
    
    # Create pt
    if (!is.null(paramSet$AL)) {
        nf <- length(paramSet$AL@free)
        if (is.null(facLab)) {
            lhs <- paste("f", 1:nf, sep = "")
            rhs <- rep("", times = nf)
        } else {
            lhs <- facLab
            rhs <- rep("", times = nf)
        }
        pt <- mapply(pt, parseFree(paramSet$AL, group = group, pt = pt, op = "~1", 
            lhs, rhs, smart = tempSmart), FUN = c, SIMPLIFY = FALSE)
    }
    
    ## TY - indicator intercept
    
    # if MY is not null but TY is null
    tempSmart <- smart
    if (!is.null(paramSet$MY) && is.null(paramSet$TY)) {
        paramSet$TY <- paramSet$MY
        tempSmart <- FALSE
    }
    
    # Create pt
    if (!is.null(paramSet$TY)) {
        ni <- length(paramSet$TY@free)
        if (is.null(indLab)) {
            lhs <- paste("y", 1:ni, sep = "")
            rhs <- rep("", times = ni)
        } else {
            lhs <- indLab
            rhs <- rep("", times = ni)
        }
        pt <- mapply(pt, parseFree(paramSet$TY, group = group, pt = pt, op = "~1", 
            lhs, rhs, smart = smart), FUN = c, SIMPLIFY = FALSE)
    }
    
    return(pt)
}

## Returns a pt (list) of parsed SimMatrix/SimVector
parseFree <- function(simDat, group, pt, op, lhs = NULL, rhs = NULL, smart = TRUE, 
    swap = FALSE) {
    ## Calculate starting indices from previous pt
    if (!is.null(pt)) {
        startId <- max(pt$id) + 1
        startFree <- max(pt$free) + 1
        startUnco <- max(pt$unco) + 1
    } else {
        startId <- 1
        startFree <- 1
        startUnco <- 1
    }
    
    freeDat <- simDat@free
    popParamDat <- simDat@popParam
    if (swap) {
        freeDat <- t(freeDat)
        popParamDat <- t(popParamDat)
    }
    numElem <- NULL
    
    if (class(simDat) == "SimVector") {
        numElem <- length(freeDat)
    } else if (simDat@symmetric) {
        # Just get lower tri
        numElem <- nrow(freeDat) * (nrow(freeDat) + 1)/2
    } else {
        numElem <- nrow(freeDat) * ncol(freeDat)
    }
    
    id <- startId:(startId + (numElem) - 1)
    op <- rep(op, length(id))
    user <- rep(0, numElem)
    group <- rep(group, numElem)
    free <- freeIdx(freeDat, start = startFree)
    ustart <- startingVal(freeDat, popParamDat, smart = smart)
    exo <- rep(0, length(id))
    eq.id <- eqIdx(freeDat, id)
    label <- names(eq.id)
    eq.id <- as.vector(eq.id)
    unco <- uncoIdx(freeDat, start = startUnco)
    return(list(id = id, lhs = as.character(lhs), op = as.character(op), rhs = as.character(rhs), 
        user = user, group = as.integer(group), free = as.integer(free), ustart = ustart, 
        exo = exo, eq.id = eq.id, label = as.character(label), unco = as.integer(unco)))
}

## Calculates the indices of free parameters by lavaan rules.  1. Each unique
## free parameter (NA) gets a unique index 2. The first constrained free
## parameter gets a unique index 3. Constrained parameters with identical
## labels get identical indices 4. Fixed parameters are 0
freeIdx <- function(mat, start = 1) {
    if (is.matrix(mat) && isSymmetric(mat)) {
        flat <- as.vector(mat[lower.tri(mat, diag = TRUE)])
    } else {
        flat <- as.vector(mat)
    }
    
    free.idx <- rep(0, length(flat))
    avail <- seq.int(start, start + length(flat) - 1, by = 1)
    isLabel <- is.label(flat)
    
    conList <- NULL
    
    j <- 1
    for (i in seq_along(flat)) {
        if (is.na(flat[i])) {
            free.idx[i] <- avail[j]
            j <- j + 1
        } else if (isLabel[i]) {
            label <- flat[i]
            if (is.null(conList[label]) || is.na(conList[label])) {
                conList <- c(conList, avail[j])
                names(conList)[length(conList)] <- label
                free.idx[i] <- avail[j]
                j <- j + 1
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
uncoIdx <- function(mat, start = 1) {
    
    if (is.matrix(mat) && isSymmetric(mat)) {
        flat <- as.vector(mat[lower.tri(mat, diag = TRUE)])
    } else {
        flat <- as.vector(mat)
    }
    
    avail <- seq.int(start, start + length(flat) - 1, by = 1)
    log <- is.na(flat) | is.label(flat)
    uncoIdx <- rep(0, length(flat))
    
    if (all(log)) {
        return(avail)
    } else {
        j <- 1
        for (i in seq_along(flat)) {
            if (log[i]) {
                uncoIdx[i] <- avail[j]
                j <- j + 1
            }
        }
    }
    return(uncoIdx)
}

## The parameter index of labels that are the same
eqIdx <- function(mat, id) {
    
    if (is.matrix(mat) && isSymmetric(mat)) {
        flat <- as.vector(mat[lower.tri(mat, diag = TRUE)])
    } else {
        flat <- as.vector(mat)
    }
    eq.idx <- rep(0, length(flat))
    isLabel <- is.label(flat)
    conList <- NULL
    
    for (i in seq_along(flat)) {
        if (isLabel[i]) {
            label <- flat[i]
            if (is.null(conList[label]) || is.na(conList[label])) {
                conList <- c(conList, id[i])
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
startingVal <- function(free, popParam, smart = TRUE) {
    if (is.matrix(free) && isSymmetric(free)) {
        flat <- as.vector(free[lower.tri(free, diag = TRUE)])
        flat[is.label(flat)] <- NA
        flat <- as.numeric(flat)
        
        if (all(!is.nan(popParam)) && smart) {
            # Check if popParam was specified
            flatPop <- as.vector(popParam[lower.tri(popParam, diag = TRUE)])
            suppressWarnings(flatPop <- as.numeric(flatPop))
            flat[is.na(flat)] <- flatPop[is.na(flat)]
        }
    } else {
        flat <- as.vector(free)
        flat[is.label(flat)] <- NA
        flat <- as.numeric(flat)
        
        if (all(!is.nan(popParam)) && smart) {
            # Check if popParam was specified
            flatPop <- as.vector(popParam)
            suppressWarnings(flatPop <- as.numeric(flatPop))
            flat[is.na(flat)] <- flatPop[is.na(flat)]
        }
    }
    flat
}

# Adjusts pt for between group constraints (if they exist). Adjusting here is
# not very elegant and should probably be reworked into everything else.
btwGroupCons <- function(pt) {
    ngroups <- max(pt$group)
    labelids <- which(pt$eq.id != 0)
    labels <- pt$label[labelids]
    paramsPerGroup <- max(pt$id)/ngroups
    updatedRows <- NULL
    usedFreeId <- NULL
    
    for (i in seq_along(unique(labels))) {
        ident <- labelids[labels == unique(labels)[i]]
        pt$eq.id[ident] <- rep(pt$eq.id[ident][1], length(pt$eq.id[ident]))
        free <- pt$free[ident][1]
        pt$free[ident] <- rep(free, length(pt$free[ident]))
        if (length(ident) != 1) {
            updatedRows <- append(updatedRows, ident)
            usedFreeId <- append(usedFreeId, free)
        }
    }
    
    elRows <- pt$id[which(pt$free != 0)]  # Rows that are free
    elRows <- elRows[-match(updatedRows, elRows)]  # Remove rows that have been updated
    if (!is.null(usedFreeId)) 
        {
            pt$free[elRows] <- (1:(length(elRows) + length(usedFreeId)))[-usedFreeId]
        }  #Remove used free ids from available list of ids
    return(pt)
}

######################## Create some shortcuts ########################

# model <- function(LY = NULL,PS = NULL,RPS = NULL, TE = NULL,RTE = NULL, BE =
# NULL, VTE = NULL, VY = NULL, VPS = NULL, VE=NULL, TY = NULL, AL = NULL, MY =
# NULL, ME = NULL, modelType, indLab=NULL, facLab=NULL, ngroups=1,
# smartStart=TRUE) {

model.cfa <- function(LY = NULL, PS = NULL, RPS = NULL, TE = NULL, RTE = NULL, VTE = NULL, 
    VY = NULL, VPS = NULL, VE = NULL, TY = NULL, AL = NULL, MY = NULL, ME = NULL, 
    indLab = NULL, facLab = NULL, groupLab = "group", ngroups = 1, smartStart = TRUE) {
    model(LY = LY, PS = PS, RPS = RPS, TE = TE, RTE = RTE, BE = NULL, VTE = VTE, 
        VY = VY, VPS = VPS, VE = VE, TY = TY, AL = AL, MY = MY, ME = ME, modelType = "CFA", 
        indLab = indLab, facLab = facLab, groupLab = groupLab, ngroups = ngroups, 
        smartStart = smartStart)
}

model.path <- function(PS = NULL, RPS = NULL, BE = NULL, VPS = NULL, VE = NULL, AL = NULL, 
    ME = NULL, indLab = NULL, facLab = NULL, groupLab = "group", ngroups = 1, smartStart = TRUE) {
    model(LY = NULL, PS = PS, RPS = RPS, TE = NULL, RTE = NULL, BE = BE, VTE = NULL, 
        VY = NULL, VPS = VPS, VE = VE, TY = NULL, AL = AL, MY = NULL, ME = ME, modelType = "Path", 
        indLab = indLab, facLab = facLab, groupLab = groupLab, ngroups = ngroups, 
        smartStart = smartStart)
}

model.sem <- function(LY = NULL, PS = NULL, RPS = NULL, TE = NULL, RTE = NULL, BE = NULL, 
    VTE = NULL, VY = NULL, VPS = NULL, VE = NULL, TY = NULL, AL = NULL, MY = NULL, 
    ME = NULL, indLab = NULL, facLab = NULL, groupLab = "group", ngroups = 1, smartStart = TRUE) {
    model(LY = LY, PS = PS, RPS = RPS, TE = TE, RTE = RTE, BE = BE, VTE = VTE, VY = VY, 
        VPS = VPS, VE = VE, TY = TY, AL = AL, MY = MY, ME = ME, modelType = "SEM", 
        indLab = indLab, facLab = facLab, groupLab = groupLab, ngroups = ngroups, 
        smartStart = smartStart)
}

estmodel <- function(LY = NULL, PS = NULL, RPS = NULL, TE = NULL, RTE = NULL, BE = NULL, 
    VTE = NULL, VY = NULL, VPS = NULL, VE = NULL, TY = NULL, AL = NULL, MY = NULL, 
    ME = NULL, modelType, indLab = NULL, facLab = NULL, groupLab = "group", ngroups = 1, 
    smartStart = TRUE) {
    if (is.null(modelType)) 
        stop("Must specify model type")
    if (modelType == "CFA") {
        if (!is.list(LY)) 
            LY <- list(LY)
        ne <- ncol(LY[[1]])
        ny <- nrow(LY[[1]])
        LY <- lapply(LY, bind)
        if (is.null(PS)) {
            if (is.null(RPS)) {
                temp <- rep(list(matrix(NA, ne, ne)), length(LY))
                temp2 <- lapply(LY, function(y) !apply(y@free, 2, function(x) any(!is.free(x) & 
                  (!is.na(x) & x != 0))))
                temp <- mapply(function(ps, x) {
                  diag(ps)[x] <- 1
                  return(ps)
                }, ps = temp, x = temp2, SIMPLIFY = FALSE)
                PS <- lapply(temp, binds)
            } else {
                if (!is.list(RPS)) 
                  RPS <- list(RPS)
                RPS <- binds(RPS)
                if (!is.null(VPS)) 
                  if (is.list(VPS)) {
                    VPS <- lapply(VPS, bind)
                  } else {
                    VPS <- list(bind(VPS))
                  }
                if (!is.null(VE)) 
                  if (is.list(VE)) {
                    VE <- lapply(VE, bind)
                  } else {
                    VE <- list(bind(VE))
                  }
            }
        } else {
            if (is.list(PS)) {
                PS <- lapply(PS, binds)
            } else {
                PS <- list(binds(PS))
            }
        }
        if (is.null(TE)) {
            if (is.null(RTE)) {
                TE <- rep(list(diag(NA, ny)), length(LY))
                TE <- lapply(TE, binds)
            } else {
                if (!is.list(RTE)) 
                  RTE <- list(RTE)
                RTE <- lapply(RTE, binds)
                if (!is.null(VTE)) 
                  if (is.list(VTE)) {
                    VTE <- lapply(VTE, bind)
                  } else {
                    VTE <- list(bind(VTE))
                  }
                if (!is.null(VY)) 
                  if (is.list(VY)) {
                    VY <- lapply(VY, bind)
                  } else {
                    VY <- list(bind(VY))
                  }
            }
        } else {
            if (is.list(TE)) {
                TE <- lapply(TE, binds)
            } else {
                TE <- list(binds(TE))
            }
        }
        if (is.null(TY)) {
            if (is.null(MY)) {
                TY <- lapply(rep(list(rep(NA, ny)), length(LY)), bind)
            } else {
                if (is.list(MY)) {
                  MY <- lapply(MY, bind)
                } else {
                  MY <- list(bind(MY))
                }
            }
        } else {
            if (is.list(TY)) {
                TY <- lapply(TY, bind)
            } else {
                TY <- list(bind(TY))
            }
        }
        if (is.null(AL)) {
            if (is.null(ME)) {
                AL <- lapply(rep(list(rep(0, ne)), length(LY)), bind)
            } else {
                if (is.list(ME)) {
                  ME <- lapply(ME, bind)
                } else {
                  ME <- list(bind(ME))
                }
            }
        } else {
            if (is.list(AL)) {
                AL <- lapply(AL, bind)
            } else {
                AL <- list(bind(AL))
            }
        }
    } else if (modelType == "Path") {
        if (!is.list(BE)) 
            BE <- list(BE)
        ne <- ncol(BE[[1]])
        BE <- lapply(BE, bind)
        if (is.null(PS)) {
            if (is.null(RPS)) {
                temp <- rep(list(matrix(0, ne, ne)), length(BE))
                temp <- lapply(temp, function(x) {
                  diag(x) <- NA
                  return(x)
                })
                set1 <- lapply(BE, function(x) findRecursiveSet(x@free)[[1]])
                temp <- mapply(function(x, y) {
                  x[y, y] <- NA
                  return(x)
                }, x = temp, y = set1, SIMPLIFY = FALSE)
                PS <- lapply(temp, binds)
            } else {
                if (!is.list(RPS)) 
                  RPS <- list(RPS)
                RPS <- binds(RPS)
                if (!is.null(VPS)) 
                  if (is.list(VPS)) {
                    VPS <- lapply(VPS, bind)
                  } else {
                    VPS <- list(bind(VPS))
                  }
                if (!is.null(VE)) 
                  if (is.list(VE)) {
                    VE <- lapply(VE, bind)
                  } else {
                    VE <- list(bind(VE))
                  }
            }
        } else {
            if (is.list(PS)) {
                PS <- lapply(PS, binds)
            } else {
                PS <- list(binds(PS))
            }
        }
        if (is.null(AL)) {
            if (is.null(ME)) {
                AL <- lapply(rep(list(rep(NA, ne)), length(BE)), bind)
            } else {
                if (is.list(ME)) {
                  ME <- lapply(ME, bind)
                } else {
                  ME <- list(bind(ME))
                }
            }
        } else {
            if (is.list(AL)) {
                AL <- lapply(AL, bind)
            } else {
                AL <- list(bind(AL))
            }
        }
    } else if (modelType == "SEM") {
        if (!is.list(LY)) 
            LY <- list(LY)
        ne <- ncol(LY[[1]])
        ny <- nrow(LY[[1]])
        LY <- lapply(LY, bind)
        if (is.list(BE)) {
            BE <- lapply(BE, bind)
        } else {
            BE <- rep(list(bind(BE)), length(LY))
        }
        if (is.null(PS)) {
            if (is.null(RPS)) {
                temp <- rep(list(matrix(0, ne, ne)), length(LY))
                temp <- lapply(temp, function(x) {
                  diag(x) <- NA
                  return(x)
                })
                set1 <- lapply(BE, function(x) findRecursiveSet(x@free)[[1]])
                temp <- mapply(function(x, y) {
                  x[y, y] <- NA
                  return(x)
                }, x = temp, y = set1, SIMPLIFY = FALSE)
                temp2 <- lapply(LY, function(y) !apply(y@free, 2, function(x) any(!is.free(x) & 
                  (!is.na(x) & x != 0))))
                temp <- mapply(function(ps, x) {
                  diag(ps)[x] <- 1
                  return(ps)
                }, ps = temp, x = temp2, SIMPLIFY = FALSE)
                PS <- lapply(temp, binds)
            } else {
                if (!is.list(RPS)) 
                  RPS <- list(RPS)
                RPS <- binds(RPS)
                if (!is.null(VPS)) 
                  if (is.list(VPS)) {
                    VPS <- lapply(VPS, bind)
                  } else {
                    VPS <- list(bind(VPS))
                  }
                if (!is.null(VE)) 
                  if (is.list(VE)) {
                    VE <- lapply(VE, bind)
                  } else {
                    VE <- list(bind(VE))
                  }
            }
        } else {
            if (is.list(PS)) {
                PS <- lapply(PS, binds)
            } else {
                PS <- list(binds(PS))
            }
        }
        if (is.null(TE)) {
            if (is.null(RTE)) {
                TE <- rep(list(diag(NA, ny)), length(LY))
                TE <- lapply(TE, binds)
            } else {
                if (!is.list(RTE)) 
                  RTE <- list(RTE)
                RTE <- lapply(RTE, binds)
                if (!is.null(VTE)) 
                  if (is.list(VTE)) {
                    VTE <- lapply(VTE, bind)
                  } else {
                    VTE <- list(bind(VTE))
                  }
                if (!is.null(VY)) 
                  if (is.list(VY)) {
                    VY <- lapply(VY, bind)
                  } else {
                    VY <- list(bind(VY))
                  }
            }
        } else {
            if (is.list(TE)) {
                TE <- lapply(TE, binds)
            } else {
                TE <- list(binds(TE))
            }
        }
        if (is.null(TY)) {
            if (is.null(MY)) {
                TY <- lapply(rep(list(rep(NA, ny)), length(LY)), bind)
            } else {
                if (is.list(MY)) {
                  MY <- lapply(MY, bind)
                } else {
                  MY <- list(bind(MY))
                }
            }
        } else {
            if (is.list(TY)) {
                TY <- lapply(TY, bind)
            } else {
                TY <- list(bind(TY))
            }
        }
        if (is.null(AL)) {
            if (is.null(ME)) {
                AL <- lapply(rep(list(rep(0, ne)), length(LY)), bind)
            } else {
                if (is.list(ME)) {
                  ME <- lapply(ME, bind)
                } else {
                  ME <- list(bind(ME))
                }
            }
        } else {
            if (is.list(AL)) {
                AL <- lapply(AL, bind)
            } else {
                AL <- list(bind(AL))
            }
        }
    } else {
        stop("The modelType specification is incorrect.")
    }
    if (!is.null(LY) && (ngroups != length(LY))) 
        LY <- rep(LY, ngroups)
    if (!is.null(PS) && (ngroups != length(PS))) 
        PS <- rep(PS, ngroups)
    if (!is.null(RPS) && (ngroups != length(RPS))) 
        RPS <- rep(RPS, ngroups)
    if (!is.null(TE) && (ngroups != length(TE))) 
        TE <- rep(TE, ngroups)
    if (!is.null(RTE) && (ngroups != length(RTE))) 
        RTE <- rep(RTE, ngroups)
    if (!is.null(BE) && (ngroups != length(BE))) 
        BE <- rep(BE, ngroups)
    if (!is.null(VTE) && (ngroups != length(VTE))) 
        VTE <- rep(VTE, ngroups)
    if (!is.null(VY) && (ngroups != length(VY))) 
        VY <- rep(VY, ngroups)
    if (!is.null(VPS) && (ngroups != length(VPS))) 
        VPS <- rep(VPS, ngroups)
    if (!is.null(VE) && (ngroups != length(VE))) 
        VE <- rep(VE, ngroups)
    if (!is.null(TY) && (ngroups != length(TY))) 
        TY <- rep(TY, ngroups)
    if (!is.null(AL) && (ngroups != length(AL))) 
        AL <- rep(AL, ngroups)
    if (!is.null(MY) && (ngroups != length(MY))) 
        MY <- rep(MY, ngroups)
    if (!is.null(ME) && (ngroups != length(ME))) 
        ME <- rep(ME, ngroups)
    
    model(LY = LY, PS = PS, RPS = RPS, TE = TE, RTE = RTE, BE = BE, VTE = VTE, VY = VY, 
        VPS = VPS, VE = VE, TY = TY, AL = AL, MY = MY, ME = ME, modelType = modelType, 
        indLab = indLab, facLab = facLab, groupLab = groupLab, ngroups = ngroups, 
        smartStart = smartStart)
}

estmodel.cfa <- function(LY = NULL, PS = NULL, RPS = NULL, TE = NULL, RTE = NULL, 
    VTE = NULL, VY = NULL, VPS = NULL, VE = NULL, TY = NULL, AL = NULL, MY = NULL, 
    ME = NULL, indLab = NULL, facLab = NULL, groupLab = "group", ngroups = 1, smartStart = TRUE) {
    estmodel(LY = LY, PS = PS, RPS = RPS, TE = TE, RTE = RTE, BE = NULL, VTE = VTE, 
        VY = VY, VPS = VPS, VE = VE, TY = TY, AL = AL, MY = MY, ME = ME, modelType = "CFA", 
        indLab = indLab, facLab = facLab, groupLab = groupLab, ngroups = ngroups, 
        smartStart = smartStart)
}

estmodel.path <- function(PS = NULL, RPS = NULL, BE = NULL, VPS = NULL, VE = NULL, 
    AL = NULL, ME = NULL, indLab = NULL, facLab = NULL, groupLab = "group", ngroups = 1, 
    smartStart = TRUE) {
    estmodel(LY = NULL, PS = PS, RPS = RPS, TE = NULL, RTE = NULL, BE = BE, VTE = NULL, 
        VY = NULL, VPS = VPS, VE = VE, TY = NULL, AL = AL, MY = NULL, ME = ME, modelType = "Path", 
        indLab = indLab, facLab = facLab, groupLab = groupLab, ngroups = ngroups, 
        smartStart = smartStart)
}

estmodel.sem <- function(LY = NULL, PS = NULL, RPS = NULL, TE = NULL, RTE = NULL, 
    BE = NULL, VTE = NULL, VY = NULL, VPS = NULL, VE = NULL, TY = NULL, AL = NULL, 
    MY = NULL, ME = NULL, indLab = NULL, facLab = NULL, groupLab = "group", ngroups = 1, 
    smartStart = TRUE) {
    estmodel(LY = LY, PS = PS, RPS = RPS, TE = TE, RTE = RTE, BE = BE, VTE = VTE, 
        VY = VY, VPS = VPS, VE = VE, TY = TY, AL = AL, MY = MY, ME = ME, modelType = "SEM", 
        indLab = indLab, facLab = facLab, groupLab = groupLab, ngroups = ngroups, 
        smartStart = smartStart)
}

# Not finish
model.lavaan <- function(object, std = FALSE, LY = NULL, PS = NULL, RPS = NULL, TE = NULL, 
    RTE = NULL, BE = NULL, VTE = NULL, VY = NULL, VPS = NULL, VE = NULL, TY = NULL, 
    AL = NULL, MY = NULL, ME = NULL, smartStart = TRUE) {
    ngroups <- object@Model@ngroups
    name <- names(object@Model@GLIST)
    modelType <- NULL
    indLab <- NULL
    facLab <- NULL
    if (isTRUE(all.equal(object@Model@dimNames[[1]][[1]], object@Model@dimNames[[1]][[2]]))) {
        indLab <- object@Model@dimNames[[1]][[1]]
        modelType <- "Path"
    } else {
        indLab <- object@Model@dimNames[[1]][[1]]
        facLab <- object@Model@dimNames[[1]][[2]]
        if ("beta" %in% name) {
            modelType <- "SEM"
        } else {
            modelType <- "CFA"
        }
    }
    
    if (std) {
        est <- standardize(object)
        free <- lapply(est, function(x) {
            x[!(round(x, 4) == 1 | round(x, 4) == 0)] <- NA
            x
        })
        freeUnstd <- inspect(object, "free")
        if (!is.null(PS)) 
            stop("Misspecification is not allowed in PS if 'std' is TRUE.")
        if (!is.null(TE)) 
            stop("Misspecification is not allowed in TE if 'std' is TRUE.")
        if (!is.null(VE)) 
            stop("Misspecification is not allowed in VE if 'std' is TRUE.")
        if (!is.null(VY)) 
            stop("Misspecification is not allowed in VY if 'std' is TRUE.")
        
        FUN <- function(x, y = NULL, z = NULL, h) {
            x[is.na(x) & (h != 0)] <- paste0("simcon", h[is.na(x) & (h != 0)])
            y[!is.free(x)] <- ""
            bind(x, y, z)
        }
        FUNS <- function(x, y, z = NULL, h) {
            x[is.na(x) & (h != 0)] <- paste0("simcon", h[is.na(x) & (h != 0)])
            diag(x) <- 1
            if (is.matrix(y)) 
                y[!is.free(x)] <- ""
            binds(x, y, z)
        }
        FUNV <- function(x, y = NULL, z = NULL, h) {
            h <- diag(h)
            x[is.na(x) & (h != 0)] <- paste0("simcon", h[is.na(x) & (h != 0)])
            y[!is.free(x)] <- ""
            bind(x, y, z)
        }
        
        if (!is.list(RPS)) 
            RPS <- rep(list(RPS), ngroups)
        RPS <- mapply(FUNS, x = free[names(free) == "psi"], y = est[names(est) == 
            "psi"], z = RPS, h = freeUnstd[names(freeUnstd) == "psi"], SIMPLIFY = FALSE)
        
        if (modelType %in% c("CFA", "SEM")) {
            ne <- ncol(est[["lambda"]])
            ny <- nrow(est[["lambda"]])
            if (!is.list(LY)) 
                LY <- rep(list(LY), ngroups)
            LY <- mapply(FUN, x = free[names(free) == "lambda"], y = est[names(est) == 
                "lambda"], z = LY, h = freeUnstd[names(freeUnstd) == "lambda"], SIMPLIFY = FALSE)
            
            if (!is.list(AL)) 
                AL <- rep(list(AL), ngroups)
            if (!("alpha" %in% names(freeUnstd))) 
                freeUnstd <- c(freeUnstd, rep(list(alpha = rep(0, ne)), ngroups))
            AL <- mapply(FUN, x = rep(list(rep(0, ne)), ngroups), z = AL, h = freeUnstd[names(freeUnstd) == 
                "alpha"], SIMPLIFY = FALSE)
            VE <- mapply(FUNV, x = rep(list(rep(1, ne)), ngroups), h = freeUnstd[names(freeUnstd) == 
                "psi"], SIMPLIFY = FALSE)
            if (!is.list(RTE)) 
                RTE <- rep(list(RTE), ngroups)
            RTE <- mapply(FUNS, x = free[names(free) == "theta"], y = est[names(est) == 
                "theta"], z = RTE, h = freeUnstd[names(freeUnstd) == "theta"], SIMPLIFY = FALSE)
            VY <- mapply(FUNV, x = rep(list(rep(NA, ny)), ngroups), y = rep(list(rep(1, 
                ny)), ngroups), h = freeUnstd[names(freeUnstd) == "theta"], SIMPLIFY = FALSE)
            if (!is.list(TY)) 
                TY <- rep(list(TY), ngroups)
            if (!("nu" %in% names(freeUnstd))) 
                freeUnstd <- c(freeUnstd, rep(list(nu = rep(0, ny)), ngroups))
            TY <- mapply(FUN, x = rep(list(rep(NA, ny)), ngroups), y = rep(list(rep(0, 
                ny)), ngroups), z = TY, h = freeUnstd[names(freeUnstd) == "nu"], 
                SIMPLIFY = FALSE)
        } else {
            ne <- ncol(est[["psi"]])
            if (!is.list(AL)) 
                AL <- rep(list(AL), ngroups)
            if (!("alpha" %in% names(freeUnstd))) 
                freeUnstd <- c(freeUnstd, rep(list(alpha = rep(0, ne)), ngroups))
            AL <- mapply(FUN, x = rep(list(rep(NA, ne)), ngroups), y = rep(list(rep(0, 
                ne)), ngroups), z = AL, h = freeUnstd[names(freeUnstd) == "alpha"], 
                SIMPLIFY = FALSE)
            VE <- mapply(FUNV, x = rep(list(rep(NA, ne)), ngroups), y = rep(list(rep(1, 
                ne)), ngroups), h = freeUnstd[names(freeUnstd) == "psi"], SIMPLIFY = FALSE)
        }
        
        if ("beta" %in% names(est)) {
            if (!is.list(BE)) 
                BE <- rep(list(BE), ngroups)
            BE <- mapply(FUN, x = free[names(free) == "beta"], y = est[names(est) == 
                "beta"], z = BE, h = freeUnstd[names(freeUnstd) == "beta"], SIMPLIFY = FALSE)
        }
        
    } else {
        est <- inspect(object, "coef")
        free <- lapply(inspect(object, "free"), function(h) {
            apply(h, 2, function(x) {
                x[x != 0] <- paste0("sim", x[x != 0])
                return(x)
            })
        })
        if (modelType == "Path") {
            set1 <- lapply(free[names(free) == "beta"], function(x) findRecursiveSet(x)[[1]])
            pospsi <- match("psi", names(free))
            for (i in seq_along(pospsi)) {
                free[[pospsi[i]]][set1[[i]], set1[[i]]] <- NA
            }
        }
        if (!is.null(RPS)) 
            stop("Misspecification is not allowed in RPS if 'std' is FALSE.")
        if (!is.null(VPS)) 
            stop("Misspecification is not allowed in VPS if 'std' is FALSE.")
        if (!is.null(VE)) 
            stop("Misspecification is not allowed in VE if 'std' is FALSE.")
        if (!is.null(RTE)) 
            stop("Misspecification is not allowed in RTE if 'std' is FALSE.")
        if (!is.null(VTE)) 
            stop("Misspecification is not allowed in VTE if 'std' is FALSE.")
        if (!is.null(VY)) 
            stop("Misspecification is not allowed in VY if 'std' is FALSE.")
        if (!is.null(ME)) 
            stop("Misspecification is not allowed in ME if 'std' is FALSE.")
        if (!is.null(MY)) 
            stop("Misspecification is not allowed in MY if 'std' is FALSE.")
        if (!is.list(RPS)) 
            RPS <- rep(list(RPS), ngroups)
        FUN2 <- function(x, y = NULL, z = NULL) {
			if(!is.null(y)) {
				x[!is.free(x)] <- y[!is.free(x)]
				y[!is.free(x)] <- ""
			}
            bind(x, y, z)
        }
        FUNS2 <- function(x, y, z = NULL) {
			if(!is.null(y)) {
				x[!is.free(x)] <- y[!is.free(x)]
				y[!is.free(x)] <- ""
			}
            binds(x, y, z)
        }
        if (!is.list(PS)) 
            PS <- rep(list(PS), ngroups)
        PS <- mapply(FUNS2, x = free[names(free) == "psi"], y = est[names(est) == 
            "psi"], z = PS, SIMPLIFY = FALSE)
        
        if (modelType %in% c("CFA", "SEM")) {
            if (!is.list(LY)) 
                LY <- rep(list(LY), ngroups)
            LY <- mapply(FUN2, x = free[names(free) == "lambda"], y = est[names(est) == 
                "lambda"], z = LY, SIMPLIFY = FALSE)
            
            if (!is.list(TE)) 
                TE <- rep(list(TE), ngroups)
            TE <- mapply(FUNS2, x = free[names(free) == "theta"], y = est[names(est) == 
                "theta"], z = TE, SIMPLIFY = FALSE)
            
            if (!is.list(TY)) 
                TY <- rep(list(TY), ngroups)
            if ("nu" %in% names(est)) {
                TY <- mapply(FUN2, x = lapply(free[names(free) == "nu"], as.vector), 
                  y = lapply(est[names(est) == "nu"], as.vector), z = TY, SIMPLIFY = FALSE)
            } else {
                ny <- nrow(est[["lambda"]])
                TY <- mapply(FUN2, x = rep(list(rep(NA, ny)), ngroups), y = rep(list(rep(0, 
                  ny)), ngroups), z = TY, SIMPLIFY = FALSE)
            }
        }
        if (!is.list(AL)) 
            AL <- rep(list(AL), ngroups)
        if ("alpha" %in% names(est)) {
            AL <- mapply(FUN2, x = lapply(free[names(free) == "alpha"], as.vector), 
                y = lapply(est[names(est) == "alpha"], as.vector), z = AL, SIMPLIFY = FALSE)
        } else {
            p <- ncol(est[["psi"]])
            if (modelType == "Path") {
                AL <- mapply(FUN2, x = rep(list(rep(NA, p)), ngroups), y = rep(list(rep(0, 
                  p)), ngroups), z = AL, SIMPLIFY = FALSE)
            } else {
                AL <- mapply(FUN2, x = rep(list(rep(0, p)), ngroups), z = AL, SIMPLIFY = FALSE)
            }
        }
        
        if ("beta" %in% names(est)) {
            if (!is.list(BE)) 
                BE <- rep(list(BE), ngroups)
            BE <- mapply(FUN2, x = free[names(free) == "beta"], y = est[names(est) == 
                "beta"], z = BE, SIMPLIFY = FALSE)
        }
        
    }
    groupLab <- object@Options$group
    if (is.null(groupLab)) 
        groupLab <- "group"
    result <- model(LY = LY, PS = PS, RPS = RPS, TE = TE, RTE = RTE, BE = BE, VTE = VTE, 
        VY = VY, VPS = VPS, VE = VE, TY = TY, AL = AL, MY = MY, ME = ME, modelType = modelType, 
        indLab = indLab, facLab = facLab, groupLab = groupLab, ngroups = ngroups, 
        smartStart = smartStart)
    return(result)
}


test.estmodel <- function() {
    loading <- matrix(0, 6, 2)
    loading[1:3, 1] <- NA
    loading[4:6, 2] <- NA
    cfa1 <- estmodel(LY = loading, ngroups = 1, modelType = "CFA")
    cfa2 <- estmodel(LY = loading, ngroups = 2, modelType = "CFA")
    
    loading <- matrix(0, 6, 2)
    loading[1:3, 1] <- c(1, "con1", "con2")
    loading[4:6, 2] <- c(1, "con3", "con4")
    latent <- matrix(NA, 2, 2)
    intcept <- paste0("int", 1:6)
    facmean <- rep(0, 2)
    error <- diag(NA, 6)
    cfa3 <- estmodel(LY = loading, PS = list(latent, latent), TE = error, AL = facmean, 
        TY = intcept, ngroups = 2, modelType = "CFA")
    
    
    path <- matrix(0, 4, 4)
    path[3, 1:2] <- NA
    path[4, 3] <- NA
    path1 <- estmodel(BE = path, ngroups = 1, modelType = "Path")
    path2 <- estmodel(BE = path, ngroups = 2, modelType = "Path")
    
    path <- matrix(0, 4, 4)
    path[3, 1:2] <- c("con1", "con2")
    path[4, 3] <- "con3"
    faccov <- diag(NA, 4)
    faccov[2, 1] <- faccov[1, 2] <- NA
    facmean <- rep(NA, 4)
    path3 <- estmodel(BE = path, PS = list(faccov, faccov), AL = facmean, ngroups = 2, 
        modelType = "Path")
    
    path <- matrix(0, 4, 4)
    path[3, 1:2] <- NA
    path[4, 3] <- NA
    loading <- matrix(0, 12, 4)
    loading[1:3, 1] <- NA
    loading[4:6, 2] <- NA
    loading[7:9, 3] <- NA
    loading[10:12, 4] <- NA
    sem1 <- estmodel(LY = loading, BE = path, ngroups = 1, modelType = "SEM")
    sem2 <- estmodel(LY = loading, BE = path, ngroups = 2, modelType = "SEM")
    
    path <- matrix(0, 4, 4)
    path[3, 1:2] <- NA
    path[4, 3] <- NA
    loading <- matrix(0, 12, 4)
    loading[1:3, 1] <- paste0("con", 1:3)
    loading[4:6, 2] <- paste0("con", 4:6)
    loading[7:9, 3] <- paste0("con", 7:9)
    loading[10:12, 4] <- paste0("con", 10:12)
    faccov <- diag(1, 4)
    faccov[2, 1] <- faccov[1, 2] <- NA
    intcept <- paste0("int", 1:12)
    facmean <- rep(0, 4)
    error <- diag(NA, 12)
    sem3 <- estmodel(LY = loading, BE = path, PS = faccov, TY = intcept, AL = facmean, 
        TE = error, ngroups = 2, modelType = "SEM")
    
}

test.model.lavaan <- function() {
    
    HS.model <- " visual  =~ x1 + x2 + x3\ntextual =~ x4 + x5 + x6\nspeed   =~ x7 + x8 + x9 "
    
    dat <- data.frame(HolzingerSwineford1939, z = rnorm(nrow(HolzingerSwineford1939), 
        0, 1))
    
    fit <- cfa(HS.model, data = dat)
    dat2 <- generate(model.lavaan(fit), n = 200)
    dat2 <- generate(model.lavaan(fit, std = TRUE), n = 200)
    
    fitgroup <- cfa(HS.model, data = dat, group = "school")
    dat2 <- generate(model.lavaan(fitgroup), n = 200)
    dat2 <- generate(model.lavaan(fitgroup, std = TRUE), n = 200)
    
    mod <- " x5 ~ x4\nx4 ~ x3\nx3 ~ x1 + x2"
    
    fitpath <- sem(mod, data = dat)
    dat2 <- generate(model.lavaan(fitpath), n = 200)
    dat2 <- generate(model.lavaan(fitpath, std = TRUE), n = 200)
    
    
    dat2 <- data.frame(PoliticalDemocracy, z = rnorm(nrow(PoliticalDemocracy), 0, 
        1))
    model1 <- "\ninvisible(\".BeGiN_TiDy_IdEnTiFiEr_HaHaHa# latent variable definitions.HaHaHa_EnD_TiDy_IdEnTiFiEr\")\nind60 =~ x1 + x2 + x3\ndem60 =~ y1 + a*y2 + b*y3 + c*y4\ndem65 =~ y5 + a*y6 + b*y7 + c*y8\ninvisible(\".BeGiN_TiDy_IdEnTiFiEr_HaHaHa.HaHaHa_EnD_TiDy_IdEnTiFiEr\")\ninvisible(\".BeGiN_TiDy_IdEnTiFiEr_HaHaHa# regressions.HaHaHa_EnD_TiDy_IdEnTiFiEr\")\ndem60 ~ ind60\ndem65 ~ ind60 + dem60\ninvisible(\".BeGiN_TiDy_IdEnTiFiEr_HaHaHa.HaHaHa_EnD_TiDy_IdEnTiFiEr\")\ninvisible(\".BeGiN_TiDy_IdEnTiFiEr_HaHaHa# residual correlations.HaHaHa_EnD_TiDy_IdEnTiFiEr\")\ny1 ~~ y5\ny2 ~~ y4 + y6\ny3 ~~ y7\ny4 ~~ y8\ny6 ~~ y8\n"
    fitsem <- sem(model1, data = dat2, meanstructure = TRUE)
    dat3 <- generate(model.lavaan(fitsem), n = 200)
    dat3 <- generate(model.lavaan(fitsem, std = TRUE), n = 200)
    
} 
