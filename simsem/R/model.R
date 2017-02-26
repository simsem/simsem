## Takes model specification matrices of type SimMatrix (or lists of these
## matrices for multiple groups).  Returns a SimSem object that contains
## templates for data generation and analyis.
model <- function(LY = NULL, PS = NULL, RPS = NULL, TE = NULL, RTE = NULL, BE = NULL,
    VTE = NULL, VY = NULL, VPS = NULL, VE = NULL, TY = NULL, AL = NULL, MY = NULL,
    ME = NULL, KA = NULL, GA = NULL, modelType = NULL, indLab = NULL, facLab = NULL, covLab = NULL,
	groupLab = "group", ngroups = 1, con = NULL) {

	con <- parseSyntaxCon(con)
    paramSet <- list(LY = LY, PS = PS, RPS = RPS, TE = TE, RTE = RTE, BE = BE, VTE = VTE,
        VY = VY, VPS = VPS, VE = VE, TY = TY, AL = AL, MY = MY, ME = ME, KA = KA, GA = GA)
    if (!is.null(modelType)) {
        modelType <- tolower(modelType)

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
                  stop("You must specify the same number of matrices for all groups.")

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
                  pt <- buildPT(psl[[i]], pt = pt, group = i, facLab = facLab, indLab = indLab, covLab = covLab)
                } else {
                  pt <- mapply(pt, buildPT(psl[[i]], pt = pt, group = i, facLab = facLab,
                    indLab = indLab, covLab = covLab), FUN = c, SIMPLIFY = FALSE)
                }
            }

            # Adjust indices for between group constraints
            pt <- btwGroupCons(pt)

			addptcon <- addeqcon(pt, con)
			pt <- addptcon[[1]]

            # nullpt <- nullpt(psl[[1]], ngroups=n)
			pt <- attachConPt(pt, con)
			con <- addptcon[[2]]

            return(new("SimSem", pt = pt, dgen = psl, modelType = modelType, groupLab = groupLab, con=con))

        } else {
            # ngroups = 1, and no matrices are lists
            paramSet <- buildModel(paramSet, modelType)
            pt <- buildPT(paramSet, facLab = facLab, indLab = indLab, covLab = covLab)
            # nullpt <- nullpt(paramSet)
			addptcon <- addeqcon(pt, con)
			pt <- addptcon[[1]]

			pt <- attachConPt(pt, con)
			con <- addptcon[[2]]

            return(new("SimSem", pt = pt, dgen = paramSet, modelType = modelType,
                groupLab = groupLab, con=con))
        }
    } else {
        stop("modelType has not been specified. Options are: \"cfa\", \"sem\", or \"path\"")
    }
}



## Takes a list of simMatrix/simVector (sg) and a model type and completes
## necessary matrices and checks the validity of the model specification.
buildModel <- function(paramSet, modelType) {

    if (modelType == "cfa") {

        if (is.null(paramSet$LY))
            stop("A loading (LY) matrix must be specified in CFA models.")

        ni <- nrow(paramSet$LY@free)
        nk <- ncol(paramSet$LY@free)

        if (!is.null(paramSet$TE)) {
            if (!paramSet$TE@symmetric)
				stop("The error covariance (TE) matrix must be symmetric.")
            if (!is.null(paramSet$RTE))
                stop("Conflict: You cannot specify both error covariance (TE) and error correlation (RTE)!")
            if (!is.null(paramSet$VTE))
                stop("Conflict: You cannot specify both error covariance (TE) and error variance (RTE)!")
            if (!is.null(paramSet$VY))
                stop("Conflict: You cannot specify both error covariance (TE) and total indicator variance (VY)!")
        } else {
            if (is.null(paramSet$RTE))
                stop("Either error correlation (RTE) or error covariance (TE) must be specified in CFA models.")
            if (!paramSet$RTE@symmetric)
				stop("The error correlation (RTE) matrix must be symmetric.")
            if (is.null(paramSet$VTE) && is.null(paramSet$VY))
                {
                  paramSet$VY <- bind(rep(NA, ni), popParam = 1)
                }  ## Set variance of indicators to be free, pop value of 1
        }

        if (!is.null(paramSet$PS)) {
			if (!paramSet$PS@symmetric)
				stop("The factor covariance (PS) matrix must be symmetric.")
            if (!is.null(paramSet$RPS))
                stop("Conflict: You cannot specify both factor covariance (PS) and factor correlation (RPS)!")
            if (!is.null(paramSet$VE))
                stop("Conflict: You cannot specify both factor covariance (PS) and total factor variance (VE)!")
        } else {
            if (is.null(paramSet$RPS))
                stop("Either factor covariance (PS) or factor correlation (RPS) must be specified in CFA models!")
            if (!paramSet$RPS@symmetric)
				stop("The factor correlation (RPS) matrix must be symmetric!")
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

		# There is a covariate
        if (!is.null(paramSet$KA) | !is.null(paramSet$GA)) {
			if (is.null(paramSet$GA)) {
				paramSet$GA <- bind(matrix(0, ni, ncol(paramSet$KA@free)))
			}
			if (is.null(paramSet$KA)) {
				paramSet$KA <- bind(matrix(0, ni, ncol(paramSet$GA@free)))
			}
		}
    } else if (modelType == "path") {

        if (is.null(paramSet$BE))
            stop("The path coefficient (BE) matrix has not been specified.")
        ne <- ncol(paramSet$BE@free)
        if (!is.null(paramSet$PS)) {
			if (!paramSet$PS@symmetric)
				stop("The covariance (PS) matrix must be symmetric.")
            if (!is.null(paramSet$RPS))
                stop("Conflict: You cannot specify both covariance (PS) and correlation (RPS)!")
            if (!is.null(paramSet$VPS))
                stop("Conflict: You cannot specify both covariance (PS) and variance (VPS)!")
            if (!is.null(paramSet$VE))
                stop("Conflict: You cannot specify both covariance (PS) and total variance (VE)!")
        } else {
            if (is.null(paramSet$RPS))
                stop("The residual correlation (RPS) matrix has not been specified.")
            if (!paramSet$RPS@symmetric)
				stop("The correlation (RPS) matrix must be symmetric!")
            if (is.null(paramSet$VPS) && is.null(paramSet$VE))
                {
                  paramSet$VE <- bind(free = rep(NA, ne), popParam = 1)
                }  ## Set latent variance to be free, pop value = 1
        }
        if (is.null(paramSet$ME) && is.null(paramSet$AL))
            {
                paramSet$ME <- bind(rep(NA, ne), popParam = 0)
            }  ## Set factor intercepts to be free, pop value = 0
		if (!is.null(paramSet$GA) & !is.null(paramSet$KA)) stop("Conflict: You cannot specify both the covaraite effects on indicators (KA) and factors (GA simultaneously)")
        if (is.null(paramSet$GA)) {
			if (!is.null(paramSet$KA)) {
				paramSet$GA <- paramSet$KA
				paramSet$KA <- NULL
			}
		}

    } else if (modelType == "sem") {

        if (is.null(paramSet$LY))
            stop("A loading (LY) matrix must be specified in SEM models.")
        ny <- nrow(paramSet$LY@free)
        ne <- ncol(paramSet$LY@free)

        if (!is.null(paramSet$TE)) {
            if (!paramSet$TE@symmetric)
				stop("The error covariance (TE) matrix must be symmetric!")
            if (!is.null(paramSet$RTE))
                stop("Conflict: You cannot specify both error covariance (TE) and error correlation (RTE)!")
            if (!is.null(paramSet$VTE))
                stop("Conflict: You cannot specify both error covariance (TE) and error variance (VTE)!")
            if (!is.null(paramSet$VY))
                stop("Conflict: You cannot specify both error covariance (TE) and total indicator variance (VY)!")
        } else {
            if (is.null(paramSet$RTE))
                stop("Either measurement error correlation (RTE) or measurement error covariance (TE) must be specified in SEM models.")
            if(!paramSet$RTE@symmetric)
				stop("The error correlation (RTE) matrix must be symmetric!")
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
            stop("A path coefficient (BE) matrix must be specified in SEM models.")

        if (!is.null(paramSet$PS)) {
            if(!paramSet$PS@symmetric)
				stop("The residual covariance (PS) matrix must be symmetric.")
            if (!is.null(paramSet$RPS))
                stop("Conflict: You cannot specify both residual covariance (PS) and residual correlation (RPS)!")
            if (!is.null(paramSet$VPS))
                stop("Conflict: You cannot specify both residual covariance (PS) and residual variance (VPS)!")
            if (!is.null(paramSet$VE))
                stop("Conflict: You cannot specify both residual covariance (PS) and total indicator variance (VE)!")
        } else {
            if (is.null(paramSet$RPS))
                stop("Either error covariance (PS) or error correlation (RPS) must be specified in SEM models.")
            if(!paramSet$RPS@symmetric)
				stop("The error correlation (RPS) matrix must be symmetric.")
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

		# There is a covariate
        if (!is.null(paramSet$KA) | !is.null(paramSet$GA)) {
			if (is.null(paramSet$GA)) {
				paramSet$GA <- bind(matrix(0, ny, ncol(paramSet$KA@free)))
			}
			if (is.null(paramSet$KA)) {
				paramSet$KA <- bind(matrix(0, ny, ncol(paramSet$GA@free)))
			}
		}
    } else {
        stop("modelType not recognized. Options are: \"CFA\", \"SEM\", or \"Path\"")
    }

    return(paramSet)
}

## Takes a list of named simMatrix/simVector objects (paramSet) that are
## optionally also lists.  Returns a table of parameters to be used for
## analysis with lavaan.  This time, PT will only take a sg paramSet. And while
## I'm at it, I'm taking out the df stuff.

buildPT <- function(paramSet, pt = NULL, group = 1, facLab = NULL, indLab = NULL, covLab = NULL) {

    ## Convert a chunk at a time - starting with LY - factor loading. At least
    ## LY,PS/RPS must be specified.
	psLab <- indLab
	psLetter <- "y"

    if (!is.null(paramSet$LY)) {
		psLab <- facLab
		psLetter <- "f"
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
        pt <- parseFree(paramSet$LY, group = group, pt = pt, op = "=~", lhs, rhs)
    }

    ## PS - factor covariance: Symmetric
    if (!is.null(paramSet$PS)) {
        nf <- ncol(paramSet$PS@free)
        if (is.null(psLab)) {
            lhs <- paste0(psLetter, rep(1:nf, nf:1))
            rhs <- paste0(psLetter, unlist(lapply(1:nf, function(k) (1:nf)[k:nf])))
        } else {
            lhs <- rep(psLab, nf:1)
            rhs <- unlist(lapply(1:nf, function(k) psLab[k:nf]))
        }
        if (!is.null(pt)) {
            if (!is.null(paramSet$LY)) {
                pt <- mapply(pt, parseFree(paramSet$PS, group = group, pt = pt, op = "~~",
                  lhs, rhs), FUN = c, SIMPLIFY = FALSE)
            } else {
                pt <- parseFree(paramSet$PS, group = group, pt = pt, op = "~~", lhs,
                  rhs)
            }
        } else {
            pt <- parseFree(paramSet$PS, group = group, pt = pt, op = "~~", lhs,
                rhs)
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
        if (is.null(psLab)) {
            lhs <- paste0(psLetter, rep(1:nf, nf:1))
            rhs <- paste0(psLetter, unlist(lapply(1:nf, function(k) (1:nf)[k:nf])))
        } else {
            lhs <- rep(psLab, nf:1)
            rhs <- unlist(lapply(1:nf, function(k) psLab[k:nf]))
        }
        if (!is.null(pt)) {
            if (!is.null(paramSet$LY)) {
                pt <- mapply(pt, parseFree(paramSet$RPS, group = group, pt = pt,
                  op = "~~", lhs, rhs), FUN = c, SIMPLIFY = FALSE)
            } else {
                pt <- parseFree(paramSet$RPS, group = group, pt = pt, op = "~~",
                  lhs, rhs)
            }
        } else {
            pt <- parseFree(paramSet$RPS, group = group, pt = pt, op = "~~", lhs,
                rhs)
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
                lhs, rhs), FUN = c, SIMPLIFY = FALSE)
        } else {
            pt <- parseFree(paramSet$TE, group = group, pt = pt, op = "~~", lhs,
                rhs)
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
            lhs, rhs), FUN = c, SIMPLIFY = FALSE)
    }

    ## BE - Regressions among factors
    if (!is.null(paramSet$BE)) {
        nf <- ncol(paramSet$BE@free)
        if (is.null(psLab)) {
            lhs <- rep(paste(psLetter, 1:nf, sep = ""), each = nf)
            rhs <- rep(paste(psLetter, 1:nf, sep = ""), times = nf)
        } else {
            lhs <- rep(psLab, each = nf)
            rhs <- rep(psLab, times = nf)
        }
        pt <- mapply(pt, parseFree(paramSet$BE, group = group, pt = pt, op = "~",
            rhs, lhs), FUN = c, SIMPLIFY = FALSE)
    }


    ## AL - factor intercept

    # if ME is not null but AL is null
    if (!is.null(paramSet$ME) && is.null(paramSet$AL)) {
        paramSet$AL <- paramSet$ME
    }

    # Create pt
    if (!is.null(paramSet$AL)) {
        nf <- length(paramSet$AL@free)
        if (is.null(psLab)) {
            lhs <- paste(psLetter, 1:nf, sep = "")
            rhs <- rep("", times = nf)
        } else {
            lhs <- psLab
            rhs <- rep("", times = nf)
        }
        pt <- mapply(pt, parseFree(paramSet$AL, group = group, pt = pt, op = "~1",
            lhs, rhs), FUN = c, SIMPLIFY = FALSE)
    }

    ## TY - indicator intercept

    # if MY is not null but TY is null
    if (!is.null(paramSet$MY) && is.null(paramSet$TY)) {
        paramSet$TY <- paramSet$MY
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
            lhs, rhs), FUN = c, SIMPLIFY = FALSE)
    }

	nz <- NULL # Save for create covariances and means among covariates

    ## GA - Regressions of factors on covariates
    if (!is.null(paramSet$GA)) {
        nf <- nrow(paramSet$GA@free)
        nz <- ncol(paramSet$GA@free)
        if (is.null(psLab)) {
            lhs <- rep(paste(psLetter, 1:nf, sep = ""), each = nz)
        } else {
            lhs <- rep(psLab, each = nz)
        }
		if (is.null(covLab)) {
			covLab <- paste("z", 1:nz, sep = "") # Save for create covariances and means among covariates
		}
		rhs <- rep(covLab, times = nf)
        pt <- mapply(pt, parseFree(paramSet$GA, group = group, pt = pt, op = "~",
            lhs, rhs), FUN = c, SIMPLIFY = FALSE)
    }

    ## KA - Regressions of factors on indicators
    if (!is.null(paramSet$KA)) {
        ni <- nrow(paramSet$KA@free)
        nz <- ncol(paramSet$KA@free)

		if (is.null(indLab)) {
            lhs <- rep(paste0("y", 1:ni) , each = nz)
        } else {
            lhs <- rep(indLab, each = nz)
        }
		if (is.null(covLab)) {
			covLab <- paste("z", 1:nz, sep = "") # Save for create covariances and means among covariates
		}
		rhs <- rep(covLab, times = ni)
        pt <- mapply(pt, parseFree(paramSet$KA, group = group, pt = pt, op = "~",
            lhs, rhs), FUN = c, SIMPLIFY = FALSE)
    }

	# Create parameter table for covariates
	if(!is.null(covLab)) {
		lhs <- rep(covLab, nz:1)
		rhs <- unlist(lapply(1:nz, function(k) covLab[k:nz]))
		pt <- mapply(pt, parseFree(bind(matrix(0, nz, nz), symmetric=TRUE), group = group, pt = pt, op = "~~",
                lhs, rhs, exo = 1, forceUstart = NA), FUN = c, SIMPLIFY = FALSE)
		lhs2 <- covLab
		rhs2 <- rep("", times = nz)
		pt <- mapply(pt, parseFree(bind(rep(0, nz)), group = group, pt = pt, op = "~1",
            lhs2, rhs2, exo = 1, forceUstart = NA), FUN = c, SIMPLIFY = FALSE)
	}
    return(pt)
}

## Returns a pt (list) of parsed SimMatrix/SimVector
parseFree <- function(simDat, group, pt, op, lhs = NULL, rhs = NULL,
    swap = FALSE, exo = 0, forceUstart = NULL) {
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
    } else if (simDat@symmetric && op == "~~") {
        # Just get lower tri
        numElem <- nrow(freeDat) * (nrow(freeDat) + 1)/2
    } else {
        numElem <- nrow(freeDat) * ncol(freeDat)
    }

    id <- startId:(startId + (numElem) - 1)
    op <- rep(op, length(id))
    user <- rep(0, numElem)
    group <- rep(group, numElem)
    free <- freeIdx(freeDat, start = startFree, symm = (op == "~~"))
	if(is.null(forceUstart)) {
		ustart <- startingVal(freeDat, popParamDat, symm = (op == "~~"))
	} else {
		ustart <- rep(forceUstart, numElem)
	}
    exo <- rep(exo, length(id))
    eq.id <- eqIdx(freeDat, id, symm = (op == "~~"))
    label <- names(eq.id)
    eq.id <- as.vector(eq.id)
    unco <- uncoIdx(freeDat, start = startUnco, symm = (op == "~~"))
    return(list(id = id, lhs = as.character(lhs), op = as.character(op), rhs = as.character(rhs),
        user = user, group = as.integer(group), free = as.integer(free), ustart = ustart,
        exo = exo, eq.id = eq.id, label = as.character(label), unco = as.integer(unco)))
}

## Calculates the indices of free parameters by lavaan rules.  1. Each unique
## free parameter (NA) gets a unique index 2. The first constrained free
## parameter gets a unique index 3. Constrained parameters with identical
## labels get identical indices 4. Fixed parameters are 0
freeIdx <- function(mat, start = 1, symm = FALSE) {
    if (is.matrix(mat) && symm) {
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
uncoIdx <- function(mat, start = 1, symm = FALSE) {

    if (is.matrix(mat) && symm) {
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
eqIdx <- function(mat, id, symm = FALSE) {

    if (is.matrix(mat) && symm) {
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
startingVal <- function(free, popParam, smart = FALSE, symm = FALSE) {
	# smartStart & smart are depreciated from the model set of functions. Will be provided in the sim function instead.
    if (is.matrix(free) && symm) {
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

model.cfa <- function(LY = NULL, PS = NULL, RPS = NULL, TE = NULL, RTE = NULL, VTE = NULL,
    VY = NULL, VPS = NULL, VE = NULL, TY = NULL, AL = NULL, MY = NULL, ME = NULL, KA = NULL, GA = NULL,
    indLab = NULL, facLab = NULL, covLab = NULL, groupLab = "group", ngroups = 1, con = NULL) {
    model(LY = LY, PS = PS, RPS = RPS, TE = TE, RTE = RTE, BE = NULL, VTE = VTE,
        VY = VY, VPS = VPS, VE = VE, TY = TY, AL = AL, MY = MY, ME = ME, KA = KA, GA = GA, modelType = "cfa",
        indLab = indLab, facLab = facLab, covLab = covLab, groupLab = groupLab, ngroups = ngroups, con = con)
}

model.path <- function(PS = NULL, RPS = NULL, BE = NULL, VPS = NULL, VE = NULL, AL = NULL,
    ME = NULL, KA = NULL, GA = NULL, indLab = NULL, facLab = NULL, covLab = NULL, groupLab = "group", ngroups = 1, con = NULL) {
    model(LY = NULL, PS = PS, RPS = RPS, TE = NULL, RTE = NULL, BE = BE, VTE = NULL,
        VY = NULL, VPS = VPS, VE = VE, TY = NULL, AL = AL, MY = NULL, ME = ME, KA = KA, GA = GA, modelType = "path",
        indLab = indLab, facLab = facLab, groupLab = groupLab, covLab = covLab, ngroups = ngroups, con = con)
}

model.sem <- function(LY = NULL, PS = NULL, RPS = NULL, TE = NULL, RTE = NULL, BE = NULL,
    VTE = NULL, VY = NULL, VPS = NULL, VE = NULL, TY = NULL, AL = NULL, MY = NULL,
    ME = NULL, KA = NULL, GA = NULL, indLab = NULL, facLab = NULL, covLab = NULL, groupLab = "group", ngroups = 1, con = NULL) {
    model(LY = LY, PS = PS, RPS = RPS, TE = TE, RTE = RTE, BE = BE, VTE = VTE, VY = VY,
        VPS = VPS, VE = VE, TY = TY, AL = AL, MY = MY, ME = ME, KA = KA, GA = GA, modelType = "sem",
        indLab = indLab, facLab = facLab, groupLab = groupLab, covLab = covLab, ngroups = ngroups, con = con)
}

estmodel <- function(LY = NULL, PS = NULL, RPS = NULL, TE = NULL, RTE = NULL, BE = NULL,
    VTE = NULL, VY = NULL, VPS = NULL, VE = NULL, TY = NULL, AL = NULL, MY = NULL,
    ME = NULL, KA = NULL, GA = NULL, modelType, indLab = NULL, facLab = NULL, covLab = NULL, groupLab = "group", ngroups = 1, con = NULL) {
    if (is.null(modelType))
        stop("modelType has not been specified. Options are: \"cfa\", \"sem\", or \"path\"")
	modelType <- tolower(modelType)

	if (!is.null(GA)) {
		if (!is.list(GA)) GA <- list(GA)
		GA <- lapply(GA, bind)
	}
	if (modelType != "path") {
		if (!is.null(KA)) {
			if (!is.list(KA)) KA <- list(KA)
			KA <- lapply(KA, bind)
		}
	}
    if (modelType == "cfa") {
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
    } else if (modelType == "path") {
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
    } else if (modelType == "sem") {
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
    if (!is.null(GA) && (ngroups != length(GA)))
        GA <- rep(GA, ngroups)
    if (!is.null(KA) && (ngroups != length(KA)))
        KA <- rep(KA, ngroups)

    model(LY = LY, PS = PS, RPS = RPS, TE = TE, RTE = RTE, BE = BE, VTE = VTE, VY = VY,
        VPS = VPS, VE = VE, TY = TY, AL = AL, MY = MY, ME = ME, KA = KA, GA = GA, modelType = modelType,
        indLab = indLab, facLab = facLab, covLab = covLab, groupLab = groupLab, ngroups = ngroups, con = con)
}

estmodel.cfa <- function(LY = NULL, PS = NULL, RPS = NULL, TE = NULL, RTE = NULL,
    VTE = NULL, VY = NULL, VPS = NULL, VE = NULL, TY = NULL, AL = NULL, MY = NULL,
    ME = NULL, KA = NULL, GA = NULL, indLab = NULL, facLab = NULL, covLab = NULL, groupLab = "group", ngroups = 1, con = NULL) {
    estmodel(LY = LY, PS = PS, RPS = RPS, TE = TE, RTE = RTE, BE = NULL, VTE = VTE,
        VY = VY, VPS = VPS, VE = VE, TY = TY, AL = AL, MY = MY, ME = ME, KA = KA, GA = GA, modelType = "CFA",
        indLab = indLab, facLab = facLab, covLab = covLab, groupLab = groupLab, ngroups = ngroups, con = con)
}

estmodel.path <- function(PS = NULL, RPS = NULL, BE = NULL, VPS = NULL, VE = NULL,
    AL = NULL, ME = NULL, KA = NULL, GA = NULL, indLab = NULL, facLab = NULL, covLab = NULL, groupLab = "group", ngroups = 1, con = NULL) {
    estmodel(LY = NULL, PS = PS, RPS = RPS, TE = NULL, RTE = NULL, BE = BE, VTE = NULL,
        VY = NULL, VPS = VPS, VE = VE, TY = NULL, AL = AL, MY = NULL, ME = ME, KA = KA, GA = GA, modelType = "Path",
        indLab = indLab, facLab = facLab, covLab = covLab, groupLab = groupLab, ngroups = ngroups, con = con)
}

estmodel.sem <- function(LY = NULL, PS = NULL, RPS = NULL, TE = NULL, RTE = NULL,
    BE = NULL, VTE = NULL, VY = NULL, VPS = NULL, VE = NULL, TY = NULL, AL = NULL,
    MY = NULL, ME = NULL, KA = NULL, GA = NULL, indLab = NULL, facLab = NULL, covLab = NULL, groupLab = "group", ngroups = 1, con = NULL) {
    estmodel(LY = LY, PS = PS, RPS = RPS, TE = TE, RTE = RTE, BE = BE, VTE = VTE,
        VY = VY, VPS = VPS, VE = VE, TY = TY, AL = AL, MY = MY, ME = ME, KA = KA, GA = GA, modelType = "SEM",
        indLab = indLab, facLab = facLab, covLab = covLab, groupLab = groupLab, ngroups = ngroups, con = con)
}

model.lavaan <- function(object, std = FALSE, LY = NULL, PS = NULL, RPS = NULL, TE = NULL,
    RTE = NULL, BE = NULL, VTE = NULL, VY = NULL, VPS = NULL, VE = NULL, TY = NULL,
    AL = NULL, MY = NULL, ME = NULL, KA = NULL, GA = NULL) {
    ngroups <- lavInspect(object, "ngroups")
    if (ngroups > 1L) {
      name <- names(lavInspect(object, "coef")[[1]])
    } else {
      name <- names(lavInspect(object, "coef"))
    }
    modelType <- NULL
    indLab <- NULL
    facLab <- NULL
  PT <- lavaan::parTable(object)
	covLab <- unique(PT$lhs[PT$op == "~~" & PT$exo == 1])
	if(length(covLab) == 0) covLab <- NULL

    if (isTRUE(all.equal(lavaan::lavNames(object, "ov"), lavaan::lavNames(object, "lv")))) {
        indLab <- setdiff(lavaan::lavNames(object, "ov"), covLab)
		facLab <- indLab
        modelType <- "path"
    } else {
        indLab <- setdiff(lavaan::lavNames(object, "ov"), covLab)
        facLab <- setdiff(lavaan::lavNames(object, "lv"), c(covLab, indLab))
        if ("beta" %in% name) {
            modelType <- "sem"
        } else {
            modelType <- "cfa"
        }
    }

	# Handle the equality constraints
	pt <- lavaan::parTable(object)
	eqpos <- which(pt$op %in% ":=")
	iseqposfromlavaan <- pt$lhs[eqpos] %in% pt$plabel & pt$rhs[eqpos] %in% pt$plabel
	eqposfromlavaan <- eqpos[iseqposfromlavaan]
	for(i in seq_along(eqposfromlavaan)) {
		temppos <- eqposfromlavaan[i]
		templab <- paste0(".line", temppos, ".")
		if(pt$label[temppos] == "") pt$label[temppos] <- templab
	}
	ptg <- lapply(pt, "[", pt$group != 0)
	ptg <- split(data.frame(ptg), ptg$group)

	# Put labels in it

    if (std) {
        est <- standardize(object)
		if(!is.null(covLab)) est <- reshuffleParamGroup(est, covLab, indLab, facLab, ngroups)
        free <- lapply(est, function(x) {
			if(!is.null(x)) {
				x[!(round(x, 4) == 1 | round(x, 4) == 0)] <- NA
			}
            x
        })
        freeUnstd <- labelFree(lavInspect(object, "free"), object@Model@isSymmetric)
		if(!is.null(covLab)) freeUnstd <- reshuffleParamGroup(freeUnstd, covLab, indLab, facLab, ngroups)
        if (!is.null(PS))
            stop("Misspecification is not allowed in PS if 'std' is TRUE.")
        if (!is.null(TE))
            stop("Misspecification is not allowed in TE if 'std' is TRUE.")
        if (!is.null(VE))
            stop("Misspecification is not allowed in VE if 'std' is TRUE.")
        if (!is.null(VY))
            stop("Misspecification is not allowed in VY if 'std' is TRUE.")

        FUN <- function(x, y = NULL, z = NULL, h, pttemp = NULL, lhstemp = NULL, optemp = NULL, rhstemp = NULL) {
			if(!is.null(h)) {
				x[(is.na(x) & (h != 0)) & is.na(h)] <- NA
				x[(is.na(x) & (h != 0)) & !is.na(h)] <- h[(is.na(x) & (h != 0)) & !is.na(h)]
			}
			freex <- is.free(x)
			if(!is.null(pttemp)) {
				if(is.null(lhstemp)) lhstemp <- rownames(x)
				if(is.null(rhstemp)) rhstemp <- colnames(x)
				targetelem <- which(pttemp$op == optemp & pttemp$lhs %in% lhstemp & pttemp$rhs %in% rhstemp)
				for(i in seq_along(targetelem)) {
					varleft <- as.character(pttemp$lhs[targetelem[i]])
					varright <- as.character(pttemp$rhs[targetelem[i]])
					if(optemp == "=~") {
						if(freex[varright, varleft] && pttemp$label[targetelem[i]] != "") x[varright, varleft] <- as.character(pttemp$label[targetelem[i]])
					} else {
						if(freex[varleft, varright] && pttemp$label[targetelem[i]] != "") x[varleft, varright] <- as.character(pttemp$label[targetelem[i]])
					}
				}
			}
            y[!freex] <- ""
            bind(x, y, z)
        }
        FUNS <- function(x, y, z = NULL, h, pttemp = NULL, lhstemp = NULL, optemp = NULL, rhstemp = NULL) {
			if(!is.null(h)) {
				x[(is.na(x) & (h != 0)) & is.na(h)] <- NA
				x[(is.na(x) & (h != 0)) & !is.na(h)] <- h[(is.na(x) & (h != 0)) & !is.na(h)]
			}
            diag(x) <- 1
			freex <- is.free(x)
			if(!is.null(pttemp)) {
				if(is.null(lhstemp)) lhstemp <- rownames(x)
				if(is.null(rhstemp)) rhstemp <- colnames(x)
				targetelem <- which(pttemp$op == optemp & pttemp$lhs %in% lhstemp & pttemp$rhs %in% rhstemp)
				for(i in seq_along(targetelem)) {
					varleft <- as.character(pttemp$lhs[targetelem[i]])
					varright <- as.character(pttemp$rhs[targetelem[i]])
					if(freex[varright, varleft] && pttemp$label[targetelem[i]] != "") x[varright, varleft] <- x[varleft, varright] <- as.character(pttemp$label[targetelem[i]])
				}
			}
            if (is.matrix(y)) y[!freex] <- ""
            binds(x, y, z)
        }
        FUNV <- function(x, y = NULL, z = NULL, h, pttemp = NULL, lhstemp = NULL, optemp = NULL) {
			if(!is.null(h)) {
				h <- diag(h)
				x[(is.na(x) & (h != 0)) & is.na(h)] <- NA
				x[(is.na(x) & (h != 0)) & !is.na(h)] <- h[(is.na(x) & (h != 0)) & !is.na(h)]
			}
			freex <- is.free(x)
			if(!is.null(pttemp)) {
				if(optemp == "~~") {
					if(is.null(lhstemp)) lhstemp <- rownames(x)
					targetelem <- which(pttemp$op == optemp & pttemp$lhs %in% lhstemp & as.character(pttemp$lhs) == as.character(pttemp$rhs))
					for(i in seq_along(targetelem)) {
						var <- as.character(pttemp$lhs[targetelem[i]])
						if(freex[var] && pttemp$label[targetelem[i]] != "") x[var] <- as.character(pttemp$label[targetelem[i]])
					}
				} else {
					if(is.null(lhstemp)) lhstemp <- names(x)
					targetelem <- which(pttemp$op == optemp & pttemp$lhs %in% lhstemp)
					for(i in seq_along(targetelem)) {
						var <- as.character(pttemp$lhs[targetelem[i]])
						if(freex[var] && pttemp$label[targetelem[i]] != "") x[var] <- as.character(pttemp$label[targetelem[i]])
					}

				}
			}
            y[!freex] <- ""
            bind(x, y, z)
        }

        if (!is.list(RPS))
            RPS <- rep(list(RPS), ngroups)
        RPS <- mapply(FUNS, x = free[names(free) == "psi"], y = est[names(est) ==
            "psi"], z = RPS, h = freeUnstd[names(freeUnstd) == "psi"], pttemp = ptg, MoreArgs = list(lhstemp = rownames(free[names(free) == "psi"]), optemp = "~~", rhstemp = colnames(free[names(free) == "psi"])), SIMPLIFY = FALSE)

        if (modelType %in% c("cfa", "sem")) {
            ne <- ncol(est[["lambda"]])
            ny <- nrow(est[["lambda"]])
            if (!is.list(LY))
                LY <- rep(list(LY), ngroups)
            LY <- mapply(FUN, x = free[names(free) == "lambda"], y = est[names(est) ==
                "lambda"], z = LY, h = freeUnstd[names(freeUnstd) == "lambda"], pttemp = ptg, MoreArgs = list(lhstemp = facLab, optemp = "=~", rhstemp = indLab), SIMPLIFY = FALSE)

            if (!is.list(AL))
                AL <- rep(list(AL), ngroups)
            if (!("alpha" %in% names(freeUnstd)))
                freeUnstd <- c(freeUnstd, rep(list(alpha = rep(0, ne)), ngroups))
            AL <- mapply(FUNV, x = rep(list(rep(0, ne)), ngroups), z = AL, h = freeUnstd[names(freeUnstd) ==
                "alpha"], pttemp = ptg, MoreArgs = list(lhstemp = facLab, optemp = "~1"), SIMPLIFY = FALSE)
            VE <- mapply(FUNV, x = rep(list(rep(1, ne)), ngroups), h = freeUnstd[names(freeUnstd) ==
                "psi"], pttemp = ptg, MoreArgs = list(lhstemp = indLab, optemp = "~~"), SIMPLIFY = FALSE)
            if (!is.list(RTE))
                RTE <- rep(list(RTE), ngroups)
            RTE <- mapply(FUNS, x = free[names(free) == "theta"], y = est[names(est) ==
                "theta"], z = RTE, h = freeUnstd[names(freeUnstd) == "theta"], pttemp = ptg, MoreArgs = list(lhstemp = indLab, optemp = "~~", rhstemp = indLab), SIMPLIFY = FALSE)
            VY <- mapply(FUNV, x = rep(list(rep(NA, ny)), ngroups), y = rep(list(rep(1,
                ny)), ngroups), h = freeUnstd[names(freeUnstd) == "theta"], pttemp = ptg, MoreArgs = list(lhstemp = indLab, optemp = "~~"), SIMPLIFY = FALSE)
            if (!is.list(TY))
                TY <- rep(list(TY), ngroups)
            if (!("nu" %in% names(freeUnstd)))
                freeUnstd <- c(freeUnstd, rep(list(nu = rep(0, ny)), ngroups))
            TY <- mapply(FUNV, x = rep(list(rep(NA, ny)), ngroups), y = rep(list(rep(0,
                ny)), ngroups), z = TY, h = freeUnstd[names(freeUnstd) == "nu"], pttemp = ptg, MoreArgs = list(lhstemp = indLab, optemp = "~1"),
                SIMPLIFY = FALSE)
            if (!is.null(covLab)) {
				if (!is.list(KA)) KA <- rep(list(KA), ngroups)
				KA <- mapply(FUN, x = free[names(free) == "kappa"], y = est[names(est) ==
                "kappa"], z = KA, h = freeUnstd[names(freeUnstd) == "kappa"], pttemp = ptg, MoreArgs = list(lhstemp = NULL, optemp = "~", rhstemp = covLab), SIMPLIFY = FALSE)
			}
        } else {
            ne <- ncol(est[["psi"]])
            if (!is.list(AL))
                AL <- rep(list(AL), ngroups)
            if (!("alpha" %in% names(freeUnstd)))
                freeUnstd <- c(freeUnstd, rep(list(alpha = rep(0, ne)), ngroups))
            AL <- mapply(FUN, x = rep(list(rep(NA, ne)), ngroups), y = rep(list(rep(0,
                ne)), ngroups), z = AL, h = freeUnstd[names(freeUnstd) == "alpha"], pttemp = ptg, MoreArgs = list(lhstemp = indLab, optemp = "~1"),
                SIMPLIFY = FALSE)
            VE <- mapply(FUNV, x = rep(list(rep(NA, ne)), ngroups), y = rep(list(rep(1,
                ne)), ngroups), h = freeUnstd[names(freeUnstd) == "psi"], pttemp = ptg, MoreArgs = list(lhstemp = indLab, optemp = "~~"), SIMPLIFY = FALSE)
        }

        if ("beta" %in% names(est)) {
            if (!is.list(BE))
                BE <- rep(list(BE), ngroups)
            BE <- mapply(FUN, x = free[names(free) == "beta"], y = est[names(est) ==
                "beta"], z = BE, h = freeUnstd[names(freeUnstd) == "beta"], pttemp = ptg, MoreArgs = list(lhstemp = NULL, optemp = "~", rhstemp = NULL), SIMPLIFY = FALSE)
        }
		if (!is.null(covLab)) {
			if (!is.list(GA)) GA <- rep(list(GA), ngroups)
			GA <- mapply(FUN, x = free[names(free) == "gamma"], y = est[names(est) ==
			"gamma"], z = GA, h = freeUnstd[names(freeUnstd) == "gamma"], pttemp = ptg, MoreArgs = list(lhstemp = NULL, optemp = "~", rhstemp = covLab), SIMPLIFY = FALSE)
		}

    } else {
        est <- lavInspect(object, "coef")
		if(!is.null(covLab)) est <- reshuffleParamGroup(est, covLab, indLab, facLab, ngroups)
        free <- labelFree(lavInspect(object, "free"), object@Model@isSymmetric)
		if(!is.null(covLab)) free <- reshuffleParamGroup(free, covLab, indLab, facLab, ngroups)
        if (modelType == "path") {
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
        FUN2 <- function(x, y = NULL, z = NULL, pttemp = NULL, lhstemp = NULL, optemp = NULL, rhstemp = NULL) {
			freex <- is.free(x)
			if(!is.null(pttemp)) {
				if(is.null(lhstemp)) lhstemp <- rownames(lhstemp)
				if(is.null(rhstemp)) rhstemp <- colnames(rhstemp)
				targetelem <- which(pttemp$op == optemp & pttemp$lhs %in% lhstemp & pttemp$rhs %in% rhstemp)
				for(i in seq_along(targetelem)) {
					varleft <- as.character(pttemp$lhs[targetelem[i]])
					varright <- as.character(pttemp$rhs[targetelem[i]])
					if(optemp == "=~") {
						if(freex[varright, varleft] && pttemp$label[targetelem[i]] != "") x[varright, varleft] <- as.character(pttemp$label[targetelem[i]])
					} else {
						if(freex[varleft, varright] && pttemp$label[targetelem[i]] != "") x[varleft, varright] <- as.character(pttemp$label[targetelem[i]])
					}
				}
			}
			if(!is.null(y)) {
				x[!freex] <- y[!freex]
				y[!freex] <- ""
			}
            bind(x, y, z)
        }
        FUNS2 <- function(x, y, z = NULL, pttemp = NULL, lhstemp = NULL, optemp = NULL, rhstemp = NULL) {
			freex <- is.free(x)
			if(!is.null(pttemp)) {
				if(is.null(lhstemp)) lhstemp <- rownames(lhstemp)
				if(is.null(rhstemp)) rhstemp <- colnames(rhstemp)
				targetelem <- which(pttemp$op == optemp & pttemp$lhs %in% lhstemp & pttemp$rhs %in% rhstemp)
				for(i in seq_along(targetelem)) {
					varleft <- as.character(pttemp$lhs[targetelem[i]])
					varright <- as.character(pttemp$rhs[targetelem[i]])
					if(freex[varright, varleft] && pttemp$label[targetelem[i]] != "") x[varright, varleft] <- x[varleft, varright] <- as.character(pttemp$label[targetelem[i]])
				}
			}
			if(!is.null(y)) {
				x[!freex] <- y[!freex]
				y[!freex] <- ""
			}
            binds(x, y, z)
        }
		FUNV2 <- function(x, y = NULL, z = NULL, pttemp = NULL, lhstemp = NULL, optemp = NULL) {
			freex <- is.free(x)
			if(!is.null(pttemp)) {
				if(optemp == "~~") {
					if(is.null(lhstemp)) lhstemp <- rownames(x)
					targetelem <- which(pttemp$op == optemp & pttemp$lhs %in% lhstemp & as.character(pttemp$lhs) == as.character(pttemp$rhs))
					for(i in seq_along(targetelem)) {
						var <- as.character(pttemp$lhs[targetelem[i]])
						if(freex[var] && pttemp$label[targetelem[i]] != "") x[var] <- as.character(pttemp$label[targetelem[i]])
					}
				} else {
					if(is.null(lhstemp)) lhstemp <- names(x)
					targetelem <- which(pttemp$op == optemp & pttemp$lhs %in% lhstemp)
					for(i in seq_along(targetelem)) {
						var <- as.character(pttemp$lhs[targetelem[i]])
						if(freex[var] && pttemp$label[targetelem[i]] != "") x[var] <- as.character(pttemp$label[targetelem[i]])
					}

				}
			}
			if(!is.null(y)) {
				x[!freex] <- y[!freex]
				y[!freex] <- ""
			}
            bind(x, y, z)
		}
		if (!is.list(PS))
            PS <- rep(list(PS), ngroups)
		PS <- mapply(FUNS2, x = free[names(free) == "psi"], y = est[names(est) ==
			"psi"], z = PS, pttemp = ptg, MoreArgs = list(lhstemp = NULL, optemp = "~~", rhstemp = NULL), SIMPLIFY = FALSE)

        if (modelType %in% c("cfa", "sem")) {
            if (!is.list(LY))
                LY <- rep(list(LY), ngroups)
            LY <- mapply(FUN2, x = free[names(free) == "lambda"], y = est[names(est) ==
                "lambda"], z = LY, pttemp = ptg, MoreArgs = list(lhstemp = facLab, optemp = "=~", rhstemp = indLab), SIMPLIFY = FALSE)

            if (!is.list(TE))
                TE <- rep(list(TE), ngroups)
            TE <- mapply(FUNS2, x = free[names(free) == "theta"], y = est[names(est) ==
                "theta"], z = TE, pttemp = ptg, MoreArgs = list(lhstemp = indLab, optemp = "~~", rhstemp = indLab), SIMPLIFY = FALSE)

            if (!is.list(TY))
                TY <- rep(list(TY), ngroups)
            if ("nu" %in% names(est) && !is.null(est$nu)) {
                TY <- mapply(FUNV2, x = lapply(free[names(free) == "nu"], as.vector),
                  y = lapply(est[names(est) == "nu"], as.vector), z = TY, pttemp = ptg, MoreArgs = list(lhstemp = indLab, optemp = "~1"), SIMPLIFY = FALSE)
            } else {
                ny <- nrow(est[["lambda"]])
                TY <- mapply(FUNV2, x = rep(list(rep(NA, ny)), ngroups), y = rep(list(rep(0,
                  ny)), ngroups), z = TY, pttemp = ptg, MoreArgs = list(lhstemp = indLab, optemp = "~1"), SIMPLIFY = FALSE)
            }
			if ("kappa" %in% names(est) && !is.null(est$kappa)) {
				if (!is.list(KA))
					KA <- rep(list(KA), ngroups)
				KA <- mapply(FUN2, x = free[names(free) == "kappa"], y = est[names(est) ==
					"kappa"], z = KA, pttemp = ptg, MoreArgs = list(lhstemp = NULL, optemp = "~", rhstemp = covLab), SIMPLIFY = FALSE)
			}
        }
        if (!is.list(AL))
            AL <- rep(list(AL), ngroups)
        if ("alpha" %in% names(est) && !is.null(est$alpha)) {
            AL <- mapply(FUNV2, x = lapply(free[names(free) == "alpha"], as.vector),
                y = lapply(est[names(est) == "alpha"], as.vector), z = AL, pttemp = ptg, MoreArgs = list(lhstemp = NULL, optemp = "~1"), SIMPLIFY = FALSE)
        } else {
            p <- ncol(est[["psi"]])
            if (modelType == "path") {
                AL <- mapply(FUNV2, x = rep(list(rep(NA, p)), ngroups), y = rep(list(rep(0,
                  p)), ngroups), z = AL, pttemp = ptg, MoreArgs = list(lhstemp = indLab, optemp = "~1"), SIMPLIFY = FALSE)
            } else {
                AL <- mapply(FUNV2, x = rep(list(rep(0, p)), ngroups), z = AL, pttemp = ptg, MoreArgs = list(lhstemp = facLab, optemp = "~1"), SIMPLIFY = FALSE)
            }
        }

        if ("beta" %in% names(est) && !is.null(est$beta)) {
            if (!is.list(BE))
                BE <- rep(list(BE), ngroups)
            BE <- mapply(FUN2, x = free[names(free) == "beta"], y = est[names(est) ==
                "beta"], z = BE, pttemp = ptg, MoreArgs = list(lhstemp = NULL, optemp = "~", rhstemp = NULL), SIMPLIFY = FALSE)
        }
        if ("gamma" %in% names(est) && !is.null(est$gamma)) {
			if (!is.list(GA))
				GA <- rep(list(GA), ngroups)
			GA <- mapply(FUN2, x = free[names(free) == "gamma"], y = est[names(est) ==
				"gamma"], z = GA, pttemp = ptg, MoreArgs = list(lhstemp = NULL, optemp = "~", rhstemp = covLab), SIMPLIFY = FALSE)
		}
    }
    groupLab <- lavInspect(object, "group")
    if (is.null(groupLab)) groupLab <- "group"


	# Get the nonlinear constraints
	pos <- (pt$op %in% c(":=", "==", ">", "<"))
	conList <- NULL
	if(any(pos)) {
		posplabel <- pt$lhs[pos] %in% pt$plabel | pt$rhs[pos] %in% pt$plabel
		pos <- pos[!posplabel]
		if(any(pos)) conList <- list(lhs = pt$lhs[pos], op = pt$op[pos], con = pt$rhs[pos])
	}

    result <- model(LY = LY, PS = PS, RPS = RPS, TE = TE, RTE = RTE, BE = BE, VTE = VTE,
        VY = VY, VPS = VPS, VE = VE, TY = TY, AL = AL, MY = MY, ME = ME, GA = GA, KA = KA, modelType = modelType,
        indLab = indLab, facLab = facLab, groupLab = groupLab, covLab = covLab, ngroups = ngroups, con = conList)
	tempcov <- NULL
	if(!is.null(KA)) {
		tempcov <- matrix(rnorm(100), ncol = ncol(KA[[1]]@free))
		colnames(tempcov) <- covLab
	}
	tryCatch(draw(result, covData = tempcov), error = function(e) print("The regression matrix is not recursive. Simsem template does not support non-recursive matrix."))
    return(result)
}

labelFree <- function(free, symmetric) {
	free2 <- mapply(function(x, y) {
	  if (y) {
	    return(x[lower.tri(x, diag = TRUE)])
	  } else return(x)
	}, x = free, y = symmetric)
	ord <- do.call(c, free2)
	ord <- ord[ord != 0]
	target <- paste0("con", 1:length(unique(ord)))
	nocommon <- !(duplicated(ord) | duplicated(ord, fromLast = TRUE))
	target[ord[nocommon]] <- NA

	free <- lapply(free, function(h) {
            apply(h, 2, function(x) {
                x[x != 0] <- target[x[x != 0]]
                return(x)
            })
        })
	free
}

## taken shamelessly from param.value in lavaan.
standardize <- function(object) {

    GLIST <- object@Model@GLIST
    est.std <- lavaan::standardizedSolution(object)$est.std

    for (mm in 1:length(GLIST)) {
        ## labels
        dimnames(GLIST[[mm]]) <- object@Model@dimNames[[mm]]

        ## fill in starting values
        m.user.idx <- object@Model@m.user.idx[[mm]]
        x.user.idx <- object@Model@x.user.idx[[mm]]
        GLIST[[mm]][m.user.idx] <- est.std[x.user.idx]

        ## class
        class(GLIST[[mm]]) <- c("matrix")


    }
    GLIST
}

reshuffleParamGroup <- function(set, covLab, indLab, facLab, ngroups) {
	if(ngroups > 1) {
		groupvec <- rep(1:ngroups, each = length(set)/ngroups)
		setGroup <- split(set, groupvec)
		result <- lapply(setGroup, reshuffleParam, covLab=covLab, indLab=indLab, facLab=facLab)
		names(result) <- NULL
		result <- do.call(c, result)
	} else {
		result <- reshuffleParam(set, covLab, indLab, facLab)
	}
	return(result)
}

reshuffleParam <- function(set, covLab, indLab, facLab) {
	lambda <- NULL
	theta <- NULL
	psi <- NULL
	beta <- NULL
	nu <- NULL
	alpha <- NULL
	gamma <- NULL
	kappa <- NULL

	if(!is.null(set$lambda)) lambda <- set$lambda[indLab, facLab, drop=FALSE]
	if(!is.null(set$theta)) theta <- set$theta[indLab, indLab, drop=FALSE]
	if(!is.null(set$psi)) psi <- set$psi[facLab, facLab, drop=FALSE]
	if(!is.null(set$beta)) beta <- set$beta[facLab, facLab, drop=FALSE]
	if(!is.null(set$nu)) nu <- set$nu[indLab, , drop=FALSE]
	if(!is.null(set$alpha)) alpha <- set$alpha[facLab, , drop=FALSE]

	covariateFac <- covLab[covLab %in% colnames(set$beta)]
	if(nrow(lambda) > ncol(lambda)) {
		singleIndicator <- indLab[indLab %in% colnames(set$beta)]
		if(length(singleIndicator) > 0) {
			if(!is.null(set$lambda)) lambda[singleIndicator, ] <- set$beta[singleIndicator, facLab, drop=FALSE]
			if(!is.null(set$theta)) theta[singleIndicator, singleIndicator] <- set$psi[singleIndicator, singleIndicator, drop=FALSE]
			if(!is.null(set$nu)) nu[singleIndicator, ] <- set$alpha[singleIndicator, , drop=FALSE]
		}
		if(length(covariateFac) > 0) {
			kappa <- matrix(0, length(indLab), length(covLab), dimnames=list(indLab, covLab))
			kappa[singleIndicator, covLab] <- set$beta[singleIndicator, covLab, drop=FALSE]
		}
	}
	if(length(covariateFac) > 0) {
		gamma <- set$beta[facLab, covLab, drop=FALSE]
	}
	return(list(lambda=lambda, theta=theta, psi=psi, beta=beta, nu=nu, alpha=alpha, gamma=gamma, kappa=kappa))
}



parseSyntaxCon <- function(script) {
	if(is.null(script)) return(list(NULL))
	if(is(script, "list")) return(script)

# Most of the beginning of this codes are from lavaanify function in lavaan

    # break up in lines
    model <- unlist( strsplit(script, "\n") )

    # remove comments starting with '#' or '!'
    model <- gsub("#.*","", model); model <- gsub("!.*","", model)

    # replace semicolons by newlines and split in lines again
    model <- gsub(";","\n", model); model <- unlist( strsplit(model, "\n") )

    # strip all white space
    model <- gsub("[[:space:]]+", "", model)

    # keep non-empty lines only
    idx <- which(nzchar(model))
    model <- model[idx]

    # check for multi-line formulas: they contain no "~" or "=" character
    # but before we do that, we remove all modifiers
    # to avoid confusion with for example equal("f1=~x1") statements
    model.simple <- gsub("\\(.*\\)\\*", "MODIFIER*", model)

    start.idx <- grep("[~=<>:]", model.simple)
    end.idx <- c( start.idx[-1]-1, length(model) )
    model.orig    <- model
    model <- character( length(start.idx) )
    for(i in 1:length(start.idx)) {
        model[i] <- paste(model.orig[start.idx[i]:end.idx[i]], collapse="")
    }
    # ok, in all remaining lines, we should have a '~' operator
    # OR one of '=', '<' '>' outside the ""
    model.simple <- gsub("\\\".[^\\\"]*\\\"", "LABEL", model)
    idx.wrong <- which(!grepl("[=:<>]", model.simple))
    if(length(idx.wrong) > 0) {
        cat("Missing ~ operator in formula(s):\n")
        print(model[idx.wrong])
        stop("Syntax error in missing model syntax")
    }

	lhs <- NULL
	op <- NULL
	rhs <- NULL
    for(i in 1:length(model)) {
        x <- model[i]
		currentop <- NULL
        # 1. which operator is used?
        line.simple <- gsub("\\\".[^\\\"]*\\\"", "LABEL", x)
        # "=~" operator?
        if(grepl("==", line.simple, fixed=TRUE)) {
            currentop <- "=="
        # ":=" operator?
        } else if(grepl(":=", line.simple, fixed=TRUE)) {
            currentop <- ":="
        # "<" operator?
        } else if(grepl("<", line.simple, fixed=TRUE)) {
            currentop <- "<"
        # ">" operator?
        } else if(grepl(">", line.simple, fixed=TRUE)) {
            currentop <- ">"
		} else {
            stop("unknown operator in ", model[i])
        }
		op <- c(op, currentop)
        # 2. split by operator (only the *first* occurence!)
        # check first if equal/label modifier has been used on the LEFT!
        if(substr(x,1,5) == "label") stop("label modifier can not be used on the left-hand side of the operator")
        op.idx <- regexpr(currentop, x)
        lhs <- c(lhs, substr(x, 1L, op.idx-1L))
        rhs <- c(rhs, substr(x, op.idx+attr(op.idx, "match.length"), nchar(x)))
    }
	list(lhs=lhs, op=op, rhs=rhs)
}

attachConPt <- function(pt, con) {
	if(is.null(con) || is.null(con[[1]])) {
		return(pt)
	} else {
		return(patMerge(pt, con))
	}
	# len <- length(con[[1]])
	# pt$id <- c(pt$id, length(pt$id) + (1:len))
	# pt$lhs <- c(pt$lhs, con$lhs)
	# pt$op <- c(pt$op, con$op)
	# pt$rhs <- c(pt$rhs, con$rhs)
	# pt$user <- c(pt$user, rep(as.integer(1), len))
	# pt$group <- c(pt$group, rep(as.integer(0), len))
	# pt$free <- c(pt$free, rep(as.integer(0), len))
	# pt$ustart <- c(pt$ustart, rep(NA, len))
	# pt$exo <- c(pt$exo, rep(as.integer(0), len))
	# lab <- rep("", len)
	# lab[con$op == ":="] <- con$lhs[con$op == ":="]
	# pt$label <- c(pt$label, lab)
	# pt$eq.id <- c(pt$eq.id, rep(as.integer(0), len))
	# pt$unco <- c(pt$unco, rep(as.integer(0), len))
	# pt
}

patMerge <- function (pt1 = NULL, pt2 = NULL, remove.duplicated = FALSE,
    fromLast = FALSE, warn = TRUE)
{
    pt1 <- as.data.frame(pt1, stringsAsFactors = FALSE)
    pt2 <- as.data.frame(pt2, stringsAsFactors = FALSE)
    stopifnot(!is.null(pt1$lhs), !is.null(pt1$op), !is.null(pt1$rhs),
        !is.null(pt2$lhs), !is.null(pt2$op), !is.null(pt2$rhs))
    if (is.null(pt1$group) && is.null(pt2$group)) {
        TMP <- rbind(pt1[, c("lhs", "op", "rhs", "group")], pt2[,
            c("lhs", "op", "rhs", "group")])
    }
    else {
        if (is.null(pt1$group) && !is.null(pt2$group)) {
            pt1$group <- rep(1L, length(pt1$lhs))
        }
        else if (is.null(pt2$group) && !is.null(pt1$group)) {
            pt2$group <- rep(1L, length(pt2$lhs))
        }
        TMP <- rbind(pt1[, c("lhs", "op", "rhs", "group")], pt2[,
            c("lhs", "op", "rhs", "group")])
    }
    if (is.null(pt1$user) && !is.null(pt2$user)) {
        pt1$user <- rep(0L, length(pt1$lhs))
    }
    else if (is.null(pt2$user) && !is.null(pt1$user)) {
        pt2$user <- rep(0L, length(pt2$lhs))
    }
    if (is.null(pt1$free) && !is.null(pt2$free)) {
        pt1$free <- rep(0L, length(pt1$lhs))
    }
    else if (is.null(pt2$free) && !is.null(pt1$free)) {
        pt2$free <- rep(0L, length(pt2$lhs))
    }
    if (is.null(pt1$ustart) && !is.null(pt2$ustart)) {
        pt1$ustart <- rep(0, length(pt1$lhs))
    }
    else if (is.null(pt2$ustart) && !is.null(pt1$ustart)) {
        pt2$ustart <- rep(0, length(pt2$lhs))
    }
    if (is.null(pt1$exo) && !is.null(pt2$exo)) {
        pt1$exo <- rep(0L, length(pt1$lhs))
    }
    else if (is.null(pt2$exo) && !is.null(pt1$exo)) {
        pt2$exo <- rep(0L, length(pt2$lhs))
    }
    if (is.null(pt1$label) && !is.null(pt2$label)) {
        pt1$label <- rep("", length(pt1$lhs))
    }
    else if (is.null(pt2$label) && !is.null(pt1$label)) {
        pt2$label <- rep("", length(pt2$lhs))
    }
    if (is.null(pt1$plabel) && !is.null(pt2$plabel)) {
        pt1$plabel <- rep("", length(pt1$lhs))
    }
    else if (is.null(pt2$plabel) && !is.null(pt1$plabel)) {
        pt2$plabel <- rep("", length(pt2$lhs))
    }
    if (is.null(pt1$start) && !is.null(pt2$start)) {
        pt1$start <- rep(as.numeric(NA), length(pt1$lhs))
    }
    else if (is.null(pt2$start) && !is.null(pt1$start)) {
        pt2$start <- rep(as.numeric(NA), length(pt2$lhs))
    }
    if (is.null(pt1$est) && !is.null(pt2$est)) {
        pt1$est <- rep(0, length(pt1$lhs))
    }
    else if (is.null(pt2$est) && !is.null(pt1$est)) {
        pt2$est <- rep(0, length(pt2$lhs))
    }
    if (remove.duplicated) {
        idx <- which(duplicated(TMP, fromLast = fromLast))
        if (length(idx)) {
            if (warn) {
                warning("lavaan WARNING: duplicated parameters are ignored:\n",
                  paste(apply(pt1[idx, c("lhs", "op", "rhs")],
                    1, paste, collapse = " "), collapse = "\n"))
            }
            if (fromLast) {
                pt1 <- pt1[-idx, ]
            }
            else {
                idx <- idx - nrow(pt1)
                pt2 <- pt2[-idx, ]
            }
        }
    }
    else if (!is.null(pt1$start) && !is.null(pt2$start)) {
        for (i in 1:length(pt1$lhs)) {
            idx <- which(pt2$lhs == pt1$lhs[i] & pt2$op == pt1$op[i] &
                pt2$rhs == pt1$rhs[i] & pt2$group == pt1$group[i])
            pt2$start[idx] <- pt1$start[i]
        }
    }
    if (is.null(pt1$id) && !is.null(pt2$id)) {
        nid <- max(pt2$id)
        pt1$id <- (nid + 1L):(nid + nrow(pt1))
    }
    else if (is.null(pt2$id) && !is.null(pt1$id)) {
        nid <- max(pt1$id)
        pt2$id <- (nid + 1L):(nid + nrow(pt2))
    }
    NEW <- base::merge(pt1, pt2, all = TRUE, sort = FALSE)
    NEW
}

addeqcon <- function(pt, con) {
	pt$plabel <- paste0(".p", pt$id, ".")
	eqcon <- setdiff(pt$label[duplicated(pt$label)], "")
	newcon <- list(lhs = character(0), op = character(0), rhs = character(0))
	for(i in seq_along(eqcon)) {
		target <- eqcon[i]
		pos <- which(target == pt$label)
		lhs <- rep(pt$plabel[pos[1]], length(pos) - 1)
		op <- rep("==", length(pos) - 1)
		rhs <- pt$plabel[pos[-1]]
		pt <- patMerge(pt, list(lhs = lhs, op = op, rhs = rhs))
		newcon <- mapply(c, newcon, list(lhs = rep(target, length(pos) - 1), op = op, rhs = rep(target, length(pos) - 1)), SIMPLIFY =FALSE)
	}
	if(length(eqcon) > 0) con <- mapply(c, newcon, con, SIMPLIFY =FALSE)
	pt <- pt[-match(c("eq.id", "unco"), names(pt))]
	pt$free[pt$free != 0] <- 1:sum(pt$free != 0)
	list(pt, con)
}
