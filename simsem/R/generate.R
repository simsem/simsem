## Higher level wrapper function for createData and drawParameters. Takes a
## SimSem analysis/data generation template, and returns a raw data set,
## optionally with the drawn parameter values.

generate <- function(model, n, maxDraw = 50, misfitBounds = NULL, misfitType = "f0",
    averageNumMisspec = FALSE, optMisfit = NULL, optDraws = 50, createOrder = c(1, 2, 3), indDist = NULL, sequential = FALSE,
    facDist = NULL, errorDist = NULL, saveLatentVar = FALSE, indLab = NULL, modelBoot = FALSE, realData = NULL, covData = NULL,
    params = FALSE, group = NULL, empirical = FALSE, ...) {
	if(is(model, "SimSem")) {
		if(!is.null(group)) model@groupLab <- group
		data <- generateSimSem(model = model, n = n, maxDraw = maxDraw, misfitBounds = misfitBounds, misfitType = misfitType,
			averageNumMisspec = averageNumMisspec, optMisfit = optMisfit, optDraws = optDraws, createOrder = createOrder, indDist = indDist, sequential = sequential,
			facDist = facDist, errorDist = errorDist, saveLatentVar = saveLatentVar, indLab = indLab, modelBoot = modelBoot, realData = realData, covData = covData,
			params = params, empirical = empirical)
	} else if (is(model, "MxModel")) {
		data <- generateMx(object = model, n = n, indDist = indDist, groupLab = group, covData = covData, empirical = empirical)
	} else {
		if(is.character(model)) {
			model <- list(model = model)
		} else if (is.partable(model)) {
			model <- list(model = model)
		} else if (is.lavaancall(model)) {
			# Intentionally leave it blank
		} else if (is(model, "lavaan")) {
			temp <- lavaan::parTable(model)
			temp$ustart <- temp$est
			model <- list(model = temp)
		} else {
			stop("Please specify an appropriate object for the 'model' argument: simsem model template, lavaan script, lavaan parameter table, OpenMx object, or list of options for the 'simulateData' function.")
		}
		model$sample.nobs <- n
		if(!is.null(indDist)) {
			model$skewness <- indDist@skewness
			model$kurtosis <- indDist@kurtosis
		}
		model$empirical <- empirical
		model <- c(model, list(...))
		browser()
		data <- do.call(lavaan::simulateData, model)
	}
	return(data)
}

generateSimSem <- function(model, n, maxDraw = 50, misfitBounds = NULL, misfitType = "f0",
    averageNumMisspec = FALSE, optMisfit = NULL, optDraws = 50, createOrder = c(1, 2, 3), indDist = NULL, sequential = FALSE,
    facDist = NULL, errorDist = NULL, saveLatentVar = FALSE, indLab = NULL, modelBoot = FALSE, realData = NULL, covData = NULL,
    params = FALSE, empirical = FALSE) {
    if (is.null(indLab)) {
        if (model@modelType == "path") {
            indLab <- unique(model@pt$lhs[!(model@pt$op %in% c("==", ":=", ">", "<"))])
        } else {
            indLab <- unique(model@pt$rhs[model@pt$op == "=~"])
        }
    }
	facLab <- NULL
	if(model@modelType != "path") facLab <- unique(model@pt$lhs[model@pt$op == "=~"])
    free <- max(model@pt$free)
    ngroups <- max(model@pt$group)

    # Wrap distributions in lists for mg
    if (!class(indDist) == "list") {
        indDist <- rep(list(indDist), ngroups)
    }
    if (!class(facDist) == "list") {
        facDist <- rep(list(facDist), ngroups)
    }
    if (!class(errorDist) == "list") {
        errorDist <- rep(list(errorDist), ngroups)
    }

	if(ngroups > 1 && length(n) == 1) n <- rep(n, ngroups)

	covLab <- unique(model@pt$lhs[model@pt$op == "~1" & model@pt$exo == 1])
	if(!is.null(realData)) {
		if((ngroups > 1) && !(model@groupLab %in% colnames(realData))) stop(paste0("The ", model@groupLab, " varaible does not in the realData argument"))
		if(is.null(covData) && (length(covLab) > 0)) {
			usedCol <- covLab
			if(ngroups > 1) usedCol <- c(usedCol, model@groupLab)
			covData <- realData[,usedCol]
		}
		realData <- realData[,setdiff(colnames(realData), covLab)]
	}

	if(length(covLab) > 0) {
		if(is.null(covData)) stop("The covariate data must be specified.")
		if(ngroups > 1) covLab <- c(covLab, model@groupLab)
		covData <- covData[,covLab, drop=FALSE]
		if(ncol(covData) != length(covLab)) stop(paste0("The covariate data must contain the following variable names: ", paste(covLab, collapse = ", ")))
		if(any(is.na(covData))) stop("The covariate data must not have missing variables.")
		indLab <- setdiff(indLab, covLab)
	} else {
		if(!is.null(covData)) {
			warnings("CONFLICT: The model template does not have any covariates but the covariate data are specified. The covariate data are ignored.")
			covData <- NULL
		}
	}
    draws <- draw(model, maxDraw = maxDraw, misfitBounds = misfitBounds, misfitType = misfitType,
        averageNumMisspec = averageNumMisspec, optMisfit = optMisfit, optDraws = optDraws, createOrder = createOrder, covData = covData)

	# The data-generation model and analysis model may have different parameterization (residual factor varaince != 1) so the change of scale is needed
	if (model@modelType %in% c("cfa", "sem")) {
		draws <- changeScaleFactor(draws, model)
	}

	realDataGroup <- rep(list(NULL), ngroups)
	covDataGroup <- rep(list(NULL), ngroups)
	if (ngroups > 1) {
		if(!is.null(realData)) {
			realDataGroup <- split(realData, realData[,model@groupLab])
			nrealData <- sapply(realDataGroup, nrow)
			if(any(nrealData != n)) stop("CONFLICT: The group sizes specified in realData and the 'n' argument are not equal.")
		}
		if(!is.null(covData)) {
			covDataGroup <- split(covData[,setdiff(covLab, model@groupLab), drop=FALSE], covData[,model@groupLab])
			ncovData <- sapply(covDataGroup, nrow)
			if(any(ncovData != n)) stop("CONFLICT: The group sizes specified in covData and the 'n' argument are not equal.")
		}
	} else {
		if(!is.null(realData)) realDataGroup <- list(realData)
		if(!is.null(covData)) covDataGroup <- list(covData)
	}
	# realData must be separated into different groups
	# covariates must be separated into different groups
	# realData must not contain covariates
    datal <- mapply(FUN = createData, draws, indDist, facDist, errorDist, n = n, realData = realDataGroup,
		covData = covDataGroup, MoreArgs = list(sequential = sequential, saveLatentVar = saveLatentVar, modelBoot = modelBoot, indLab = indLab, facLab = facLab, empirical = empirical),
        SIMPLIFY = FALSE)
	data <- NULL
	extra <- NULL
	if(saveLatentVar) {
		data <- lapply(datal, "[[", 1)
		extra <- lapply(datal, "[[", 2)
		data <- do.call("rbind", data)
		extra <- do.call("rbind.fill", extra)
	} else {
		data <- do.call("rbind", datal)
	}
	if(ngroups > 1) {
		data <- cbind(data, group = rep(1:ngroups, n))
		colnames(data)[ncol(data)] <- model@groupLab
		if(saveLatentVar) {
			extra <- cbind(extra, group = rep(1:ngroups, n))
			colnames(extra)[ncol(extra)] <- model@groupLab
		}
    }
	if(saveLatentVar) attr(data, "latentVar") <- extra
    if (params) {
        return(list(data = data, psl = draws))
    } else {
        return(data)
    }

}

popMisfitParams <- function(psl, df = NULL, covData = NULL) {
    ngroups <- length(psl)
    real <- lapply(psl, "[[", 1)
    realmis <- lapply(psl, "[[", 2)
	covStat <- rep(list(NULL), ngroups)
	if (!is.null(covData)) {
		if(ngroups == 1) {
			covStat[[1]] <- list(MZ = as.matrix(colMeans(covData)), CZ = cov(covData))
		} else {
			groupCov <- covData[,ncol(covData)]
			targetDat <- covData[,-ncol(covData)]
			for(i in 1:ngroups) {
				covStat[[i]] <- list(MZ = as.matrix(colMeans(targetDat[groupCov == i,])), CZ = cov(targetDat[groupCov == i,]))
			}
		}
	}
    macsreal <- mapply(createImpliedMACS, real, covStat, SIMPLIFY = FALSE)
    macsmis <- mapply(createImpliedMACS, realmis, covStat, SIMPLIFY = FALSE)
    misfit <- popMisfitMACS(paramM = lapply(macsreal, "[[", 1), paramCM = lapply(macsreal,
        "[[", 2), misspecM = lapply(macsmis, "[[", 1), misspecCM = lapply(macsmis,
        "[[", 2), fit.measures = "all", dfParam = df)
    return(misfit)
}


changeScaleFactor <- function(drawResult, gen) {
	# Find the scales that are based on fixed factor
	dgen <- gen@dgen
	pt <- gen@pt
	addcon <- pt$op %in% c("==", "<", ">", ":=")
	pt <- lapply(pt, function(x) x[!addcon])
	ptgroup <- split(as.data.frame(pt), pt$group)
	indLab <- lapply(ptgroup, function(x) unique(x$rhs[x$op == "=~"]))
	facLab <- lapply(ptgroup, function(x) unique(x$lhs[x$op == "=~"]))
	nfac <- lapply(facLab, length)
	scale <- lapply(nfac, diag)
	# Find which fixed factor
	ptgroup <- split(as.data.frame(pt), pt$group)
	ngroup <- length(ptgroup)

	facPos <- mapply(function(temppt, templab) which((temppt$lhs %in% templab) & (as.character(temppt$rhs) == as.character(temppt$lhs)) & (temppt$op == "~~")), temppt = ptgroup, templab = facLab, SIMPLIFY=FALSE) # assume that the order is good
	fixedPos <- mapply(function(temppt, temppos) temppt$free[temppos] == 0, temppt=ptgroup, temppos=facPos, SIMPLIFY=FALSE)
	fixedValue <- mapply(function(temppt, temppos) temppt$ustart[temppos], temppt=ptgroup, temppos=facPos, SIMPLIFY=FALSE)

	param <- lapply(drawResult, "[[", "param")
	misspec <- lapply(drawResult, "[[", "misspec")

	for(g in 1:ngroup) {
		select <- fixedPos[[g]]
		if(any(select)) {
			supposedVal <- fixedValue[[g]][select]
			tempscale <- scale[[g]]
			if(length(supposedVal) > 1) {
				diag(tempscale)[select] <- diag(solve(sqrt(diag(diag(param[[g]]$PS[select, select, drop=FALSE])))) %*% sqrt(diag(supposedVal)))
			} else {
				temp <- as.matrix(supposedVal)
				temp2 <- as.matrix(diag(param[[g]]$PS[select, select, drop=FALSE]))
				diag(tempscale)[select] <- diag(solve(sqrt(temp2)) %*% sqrt(temp))
			}
			scale[[g]] <- tempscale
		}
	}

	# Find which manifest variable

	manifestPos <- vector("list", ngroup)
	manifestValue <- vector("list", ngroup)
	for(g in 1:ngroup) {
		for(i in 1:length(facLab[[g]])) {
			rowLoad <- (ptgroup[[g]]$lhs == facLab[[g]][i]) & (ptgroup[[g]]$op == "=~") & (ptgroup[[g]]$free == 0) & (ptgroup[[g]]$ustart != 0)
			if(any(rowLoad)) {
				targetrow <- which(rowLoad)[1]
				manifestPos[[g]] <- rbind(manifestPos[[g]], c(which(ptgroup[[g]]$rhs[targetrow] == indLab[[g]]), i))
				manifestValue[[g]] <- c(manifestValue[[g]], ptgroup[[g]]$ustart[which(rowLoad)[1]])
			}
		}
	}

	for(g in 1:ngroup) {
		if(!is.null(manifestPos[[g]])) {
			select <- manifestPos[[g]][,2]
			supposedVal <- manifestValue[[g]]
			tempscale <- scale[[g]]
			for(i in 1:length(select)) {
				# Note that the LY is multiplied by solve(scale) not scale
				tempscale[select[i], select[i]] <- param[[g]]$LY[manifestPos[[g]][i, 1], manifestPos[[g]][i, 2]] / manifestValue[[g]][i]
			}
			scale[[g]] <- tempscale
		}
	}

	# Find which equality --> Then borrow from fixed or manifest
	conLoad <- (duplicated(pt$free) | duplicated(pt$free, fromLast=TRUE)) & (pt$free != 0) & (pt$op == "=~")
	if(any(conLoad)) {
		conPos <- unique(pt$free[conLoad])
		for(i in length(conPos)) {
			eachPos <- conPos[i] == pt$free
			tempFacName <- pt$lhs[eachPos]
			tempGroup <- pt$group[eachPos]
			scaleVal <- NA
			for(j in 1:length(tempFacName)) {
				pos <- which(tempFacName[j] == facLab[[tempGroup[j]]])
				isFixed <- fixedPos[[tempGroup[j]]][pos]
				if(isFixed) scaleVal <- scale[[tempGroup[j]]][pos, pos]
			}
			# Impose scale only when the fixed factor method is used in some factors
			if(!is.na(scaleVal)) {
				for(j in length(tempFacName)) {
					pos <- which(tempFacName[j] == facLab[[tempGroup[j]]])
					scale[[tempGroup[j]]][pos, pos] <- scaleVal
				}
			}
		}
	}

	for(g in 1:ngroup) {
		param[[g]]$LY <- param[[g]]$LY %*% solve(scale[[g]])
		param[[g]]$PS <- scale[[g]] %*% param[[g]]$PS %*% scale[[g]]
		if(!is.null(param[[g]]$BE)) param[[g]]$BE <- scale[[g]] %*% param[[g]]$BE %*% solve(scale[[g]])
		param[[g]]$AL <- scale[[g]] %*% param[[g]]$AL
		if(!is.null(param[[g]]$GA)) param[[g]]$GA <- scale[[g]] %*% param[[g]]$GA
		if(!is.null(misspec[[g]])) {
			misspec[[g]]$LY <- misspec[[g]]$LY %*% solve(scale[[g]])
			misspec[[g]]$PS <- scale[[g]] %*% misspec[[g]]$PS %*% scale[[g]]
			if(!is.null(misspec[[g]]$BE)) misspec[[g]]$BE <- scale[[g]] %*% misspec[[g]]$BE %*% solve(scale[[g]])
			misspec[[g]]$AL <- scale[[g]] %*% misspec[[g]]$AL
			if(!is.null(misspec[[g]]$GA)) misspec[[g]]$GA <- scale[[g]] %*% misspec[[g]]$GA
		}
	}
	for(g in 1:ngroup) {
		drawResult[[g]]$param <- param[[g]]
		if(!is.null(misspec[[g]])) drawResult[[g]]$misspec <- misspec[[g]]
	}
	return(drawResult)
}

semMACS <- function(param) {
	ID <- matrix(0, nrow(param$PS), nrow(param$PS))
    diag(ID) <- 1
	implied.mean <- solve(ID - param$BE) %*% param$AL
        implied.covariance <- solve(ID - param$BE) %*% param$PS %*%
            t(solve(ID - param$BE))
        if (!is.null(param$LY)) {
            implied.mean <- param$TY + (param$LY %*% implied.mean)
            implied.covariance <- (param$LY %*% implied.covariance %*%
                t(param$LY)) + param$TE
        }
		return(list(implied.mean, implied.covariance))
}

# The script below is modified from lavaan. Not being used anymore.

fleishman1978_abcd <- function(skewness, kurtosis) {
	system.function <- function(x, skewness, kurtosis) {
		b.=x[1L]; c.=x[2L]; d.=x[3L]
		eq1 <- b.^2 + 6*b.*d. + 2*c.^2 + 15*d.^2 - 1
		eq2 <- 2*c.*(b.^2 + 24*b.*d. + 105*d.^2 + 2) - skewness
		eq3 <- 24*(b.*d. + c.^2*(1 + b.^2 + 28*b.*d.) +
				   d.^2*(12 + 48*b.*d. + 141*c.^2 + 225*d.^2)) - kurtosis
		eq <- c(eq1,eq2,eq3)
		sum(eq^2) ## SS
	}

	out <- nlminb(start=c(1,0,0), objective=system.function,
				  scale=10,
				  control=list(trace=0),
				  skewness=skewness, kurtosis=kurtosis)
	if(out$convergence != 0) warning("no convergence")
	b. <- out$par[1L]; c. <- out$par[2L]; d. <- out$par[3L]; a. <- -c.
	c(a.,b.,c.,d.)
}

lavaanValeMaurelli1983 <- function(n=100L, COR, skewness, kurtosis) {

    getICOV <- function(b1, c1, d1, b2, c2, d2, R) {
        objectiveFunction <- function(x, b1, c1, d1, b2, c2, d2, R) {
            rho=x[1L]
            eq <- rho*(b1*b2 + 3*b1*d2 + 3*d1*b2 + 9*d1*d2) +
                  rho^2*(2*c1*c2) + rho^3*(6*d1*d2) - R
            eq^2
        }

        out <- nlminb(start=R, objective=objectiveFunction,
                      scale=10, control=list(trace=0),
                      b1=b1, c1=c1, d1=d1, b2=b2, c2=c2, d2=d2, R=R)
        if(out$convergence != 0) warning("no convergence")
        rho <- out$par[1L]
        rho
    }

    # number of variables
    nvar <- ncol(COR)
    # check skewness
    if(length(skewness) == nvar) {
        SK <- skewness
    } else if(length(skewness == 1L)) {
        SK <- rep(skewness, nvar)
    } else {
        stop("skewness has wrong length")
    }

    if(length(kurtosis) == nvar) {
        KU <- kurtosis
    } else if(length(skewness == 1L)) {
        KU <- rep(kurtosis, nvar)
    } else {
        stop("kurtosis has wrong length")
    }

    # create Fleishman table
    FTable <- matrix(0, nvar, 4L)
    for(i in 1:nvar) {
        FTable[i,] <- fleishman1978_abcd(skewness=SK[i], kurtosis=KU[i])
    }

    # compute intermediate correlations between all pairs
    ICOR <- diag(nvar)
	if(nvar > 1) {
		for(j in 1:(nvar-1L)) {
			for(i in (j+1):nvar) {
				if(COR[i,j] == 0) next
				ICOR[i,j] <- ICOR[j,i] <-
					getICOV(FTable[i,2], FTable[i,3], FTable[i,4],
							FTable[j,2], FTable[j,3], FTable[j,4], R=COR[i,j])
			}
		}
	}
    # generate Z ## FIXME: replace by rmvnorm once we use that package
    X <- Z <- mvrnorm(n=n, mu=rep(0,nvar), Sigma=ICOR)

    # transform Z using Fleishman constants
    for(i in 1:nvar) {
        X[,i] <- FTable[i,1L] + FTable[i,2L]*Z[,i] + FTable[i,3L]*Z[,i]^2 +
                 FTable[i,4L]*Z[,i]^3
    }

    X
}

HeadrickSawilowsky1999 <- function(n=100L, COR, skewness, kurtosis) {
    # number of variables
    p <- ncol(COR)
    # check skewness
    if(length(skewness) == p) {
        SK <- skewness
    } else if(length(skewness == 1L)) {
        SK <- rep(skewness, p)
    } else {
        stop("skewness has wrong length")
    }

    if(length(kurtosis) == p) {
        KU <- kurtosis
    } else if(length(skewness == 1L)) {
        KU <- rep(kurtosis, p)
    } else {
        stop("kurtosis has wrong length")
    }

    FTable <- matrix(0, p, 4L)
    for(i in 1:p) {
        FTable[i,] <- fleishman1978_abcd(skewness=SK[i], kurtosis=KU[i])
    }
	if (p == 2) {
		targetR <- COR[lower.tri(COR)]
		objFUN2 <- function(x, va, vb, vd, COR) {
			r <- x^2
			eq <- r*(vb[1]*vb[2] + 3*vb[2]*vd[1] + 3*vb[1]*vd[2] + 9*vd[1]*vd[2] + 2*va[1]*va[2]*r + 6*vd[1]*vd[2]*(r^2)) - COR
			eq^2
		}
		out <- nlminb(start=targetR, objective=objFUN2,
                      scale=10, control=list(trace=0),
                      va=FTable[,1], vb=FTable[,2], vd=FTable[,4], COR=targetR)
        if(out$convergence != 0) warning("no convergence")
        vr <- out$par[1L]
		z1 <- rnorm(n, 0, 1)
		edata <- sapply(1:2, function(x) rnorm(n, 0, 1))
		tarvar <- z1 %*% matrix(vr, nrow=1, ncol=2)
		transformvr <- edata %*% diag(sqrt(1 - vr^2), 2)
		Z <- tarvar + transformvr
	} else if (p == 3) {
		rowindex <- row(diag(p))[lower.tri(diag(p))]
		colindex <- col(diag(p))[lower.tri(diag(p))]
		targetR <- COR[lower.tri(COR)]
		findStartA <- function(x, targetR, rowindex, colindex) {
			eq <- x[rowindex] * x[colindex] - targetR
			sum(eq^2)
		}
		start <- nlminb(start=runif(3, -1, 1), objective=findStartA,
                      scale=10, control=list(trace=0),
                      targetR=targetR, rowindex = rowindex, colindex = colindex)
		if(start$convergence != 0) warning("no convergence")
		objFUN3 <- function(x, va, vb, vd, targetR, rowindex, colindex) {
			vr <- x
			tempr1 <- vr[rowindex]
			tempr2 <- vr[colindex]
			tempb1 <- vb[rowindex]
			tempb2 <- vb[colindex]
			tempa1 <- va[rowindex]
			tempa2 <- va[colindex]
			tempd1 <- vd[rowindex]
			tempd2 <- vd[colindex]
			rprod <- tempr1 * tempr2
			eq <- rprod*(tempb1 * tempb2 + 3*tempb2*tempd1 + 3*tempb1*tempd2 + 9*tempd2*tempd2 + 2*tempa1*tempa2*(rprod^2) + 6*tempd1*tempd2*(rprod^4)) - targetR
			sum(eq^2)
		}
		out <- nlminb(start=start$par, objective=objFUN3,
                      scale=10, control=list(trace=0),
                      va=FTable[,1], vb=FTable[,2], vd=FTable[,4], targetR=targetR, rowindex = rowindex, colindex = colindex)

        if(out$convergence != 0) warning("no convergence")
		z1 <- rnorm(n, 0, 1)
		vr <- out$par
		edata <- sapply(1:p, function(x) rnorm(n, 0, 1))
		tarvar <- z1 %*% matrix(vr, nrow=1)
		transformvr <- edata %*% diag(sqrt(1 - vr^2))
		Z <- tarvar + transformvr
	} else if (p > 3) {
		rowindex <- row(diag(p))[lower.tri(diag(p))]
		colindex <- col(diag(p))[lower.tri(diag(p))]
		targetR <- COR[lower.tri(COR)]
		halfp <- ceiling(p/2)
		mat <- matrix(FALSE, p, p)
		# Arbitrarily pick first half from Z1 and second half from Z2
		mat[1:halfp, 1:halfp] <- TRUE
		mat[(halfp + 1):p, (halfp + 1):p] <- TRUE
		# This method does not make sense in this logic. In two or three variables, only one Z is needed. In four variables, two Zs are needed. However, in more than four variables, only two Zs are needed. If the number of Zs is doubled from two to four variables, isn't it doubled when from four to eight variables.
		ctrlvec <- mat[lower.tri(mat)]

		findStartB <- function(x, targetR, rowindex, colindex, p, ctrlvec) {
			r0 <- rep(x[1], length(targetR))
			r0[ctrlvec] <- 1
			vr <- x[2:(p+1)]
			eq <- r0 * vr[rowindex] * vr[colindex] - targetR
			sum(eq^2)
		}
		#r0 is modeled instead of the product of different t which does not make sense to use it.
		start <- nlminb(start=runif(p+1, -1, 1), objective=findStartB,
                      scale=10, control=list(trace=0),
                      targetR=targetR, rowindex = rowindex, colindex = colindex, p = p, ctrlvec = ctrlvec, lower=-1, upper=1)
		if(start$convergence != 0) warning("no convergence")

		objFUN4 <- function(x, va, vb, vd, targetR, rowindex, colindex, p, ctrlvec) {
			r0 <- rep(x[1], length(targetR))
			r0[ctrlvec] <- 1
			vr <- x[2:(p+1)]
			tempr1 <- vr[rowindex]
			tempr2 <- vr[colindex]
			tempb1 <- vb[rowindex]
			tempb2 <- vb[colindex]
			tempa1 <- va[rowindex]
			tempa2 <- va[colindex]
			tempd1 <- vd[rowindex]
			tempd2 <- vd[colindex]
			rprod <- tempr1 * tempr2 * r0
			eq <- rprod*(tempb1 * tempb2 + 3*tempb2*tempd1 + 3*tempb1*tempd2 + 9*tempd2*tempd2 + 2*tempa1*tempa2*(rprod^2) + 6*tempd1*tempd2*(rprod^4)) - targetR
			sum(eq^2)
		}
		# optimize COR (y) directly
		out <- nlminb(start=start$par, objective=objFUN4,
                      scale=10, control=list(trace=0),
                      va=FTable[,1], vb=FTable[,2], vd=FTable[,4], targetR=targetR, rowindex = rowindex, colindex = colindex, p = p, ctrlvec = ctrlvec, lower=-1, upper=1)

        if(out$convergence != 0) warning("no convergence")
		z1 <- rnorm(n, 0, 1)
		v <- rnorm(n, 0, 1)
		r0 <- out$par[1]
		vr <- out$par[2:(p+1)]
		edata <- sapply(1:p, function(x) rnorm(n, 0, 1))
		z2 <- r0 * z1 + sqrt(1 - r0^2) * v
		classify <- c(rep(TRUE, halfp), rep(FALSE, p - halfp))
		classify <- rbind(classify, !classify) * rbind(vr, vr)
		tarvar <- cbind(z1, z2) %*% classify
		transformvr <- edata %*% diag(sqrt(1 - vr^2))
		Z <- tarvar + transformvr
	}
	X <- Z
    # transform Z using Fleishman constants
    for(i in 1:p) {
        X[,i] <- FTable[i,1L] + FTable[i,2L]*Z[,i] + FTable[i,3L]*Z[,i]^2 +
                 FTable[i,4L]*Z[,i]^3
    }

    X
}

####### The following functions are copied from the plyr package.

rbind.fill <- function (...)
{
    dfs <- list(...)
    if (length(dfs) == 0)
        return()
    if (is.list(dfs[[1]]) && !is.data.frame(dfs[[1]])) {
        dfs <- dfs[[1]]
    }
    dfs <- compact(dfs)
    if (length(dfs) == 0)
        return()
    if (length(dfs) == 1)
        return(dfs[[1]])
    is_df <- vapply(dfs, is.data.frame, logical(1))
    if (any(!is_df)) {
        stop("All inputs to rbind.fill must be data.frames",
            call. = FALSE)
    }
    rows <- unlist(lapply(dfs, .row_names_info, 2L))
    nrows <- sum(rows)
    ot <- output_template(dfs, nrows)
    setters <- ot$setters
    getters <- ot$getters
    if (length(setters) == 0) {
        return(as.data.frame(matrix(nrow = nrows, ncol = 0)))
    }
    pos <- matrix(c(cumsum(rows) - rows + 1, rows), ncol = 2)
    for (i in seq_along(rows)) {
        rng <- seq(pos[i, 1], length = pos[i, 2])
        df <- dfs[[i]]
        for (var in names(df)) {
            setters[[var]](rng, df[[var]])
        }
    }
    quickdf(lapply(getters, function(x) x()))
}

quickdf <- function (list)
{
    rows <- unique(unlist(lapply(list, NROW)))
    stopifnot(length(rows) == 1)
    names(list) <- make_names(list, "X")
    class(list) <- "data.frame"
    attr(list, "row.names") <- c(NA_integer_, -rows)
    list
}

compact <- function (l) Filter(Negate(is.null), l)

output_template <- function (dfs, nrows)
{
    vars <- unique(unlist(lapply(dfs, base::names)))
    output <- vector("list", length(vars))
    names(output) <- vars
    seen <- rep(FALSE, length(output))
    names(seen) <- vars
    for (df in dfs) {
        matching <- intersect(names(df), vars[!seen])
        for (var in matching) {
            output[[var]] <- allocate_column(df[[var]], nrows,
                dfs, var)
        }
        seen[matching] <- TRUE
        if (all(seen))
            break
    }
    list(setters = lapply(output, `[[`, "set"), getters = lapply(output,
        `[[`, "get"))
}

allocate_column <- function (example, nrows, dfs, var)
{
    a <- attributes(example)
    type <- typeof(example)
    class <- a$class
    isList <- is.recursive(example)
    a$names <- NULL
    a$class <- NULL
    if (is.data.frame(example)) {
        stop("Data frame column '", var, "' not supported by rbind.fill")
    }
    if (is.array(example)) {
        if (length(dim(example)) > 1) {
            if ("dimnames" %in% names(a)) {
                a$dimnames[1] <- list(NULL)
                if (!is.null(names(a$dimnames)))
                  names(a$dimnames)[1] <- ""
            }
            df_has <- vapply(dfs, function(df) var %in% names(df),
                FALSE)
            dims <- unique(lapply(dfs[df_has], function(df) dim(df[[var]])[-1]))
            if (length(dims) > 1)
                stop("Array variable ", var, " has inconsistent dims")
            a$dim <- c(nrows, dim(example)[-1])
            length <- prod(a$dim)
        }
        else {
            a$dim <- NULL
            a$dimnames <- NULL
            length <- nrows
        }
    }
    else {
        length <- nrows
    }
    if (is.factor(example)) {
        df_has <- vapply(dfs, function(df) var %in% names(df),
            FALSE)
        isfactor <- vapply(dfs[df_has], function(df) is.factor(df[[var]]),
            FALSE)
        if (all(isfactor)) {
            levels <- unique(unlist(lapply(dfs[df_has], function(df) levels(df[[var]]))))
            a$levels <- levels
            handler <- "factor"
        }
        else {
            type <- "character"
            handler <- "character"
            class <- NULL
            a$levels <- NULL
        }
    }
    else if (inherits(example, "POSIXt")) {
        tzone <- attr(example, "tzone")
        class <- c("POSIXct", "POSIXt")
        type <- "double"
        handler <- "time"
    }
    else {
        handler <- type
    }
    column <- vector(type, length)
    if (!isList) {
        column[] <- NA
    }
    attributes(column) <- a
    assignment <- make_assignment_call(length(a$dim))
    setter <- switch(handler, character = function(rows, what) {
        what <- as.character(what)
        eval(assignment)
    }, factor = function(rows, what) {
        what <- match(what, levels)
        eval(assignment)
    }, time = function(rows, what) {
        what <- as.POSIXct(what, tz = tzone)
        eval(assignment)
    }, function(rows, what) {
        eval(assignment)
    })
    getter <- function() {
        class(column) <<- class
        column
    }
    list(set = setter, get = getter)
}

make_assignment_call <- function (ndims)
{
    assignment <- quote(column[rows] <<- what)
    if (ndims >= 2) {
        assignment[[2]] <- as.call(c(as.list(assignment[[2]]),
            rep(list(quote(expr = )), ndims - 1)))
    }
    assignment
}

make_names <- function (x, prefix = "X")
{
    nm <- names(x)
    if (is.null(nm)) {
        nm <- rep.int("", length(x))
    }
    n <- sum(nm == "", na.rm = TRUE)
    nm[nm == ""] <- paste(prefix, seq_len(n), sep = "")
    nm
}

# Copy from lavaan to avoid using hidden function in lavaan

temp.unstandardize.est.ov <- function(partable, ov.var=NULL, cov.std=TRUE) {

    # check if ustart is missing; if so, look for est
    if(is.null(partable$ustart))
        partable$ustart <- partable$est

    # check if group is missing
    if(is.null(partable$group))
        partable$group <- rep(1L, length(partable$ustart))

    stopifnot(!any(is.na(partable$ustart)))
    est <- out <- partable$ustart
    N <- length(est)
    ngroups <- max(partable$group)

    # if ov.var is NOT a list, make a list
    if(!is.list(ov.var)) {
        tmp <- ov.var
        ov.var <- vector("list", length=ngroups)
        ov.var[1:ngroups] <- list(tmp)
    }

    for(g in 1:ngroups) {

        ov.names <- lavaan::lavNames(partable, "ov", group=g) # not user
        lv.names <- lavaan::lavNames(partable, "lv", group=g)

        OV  <- sqrt(ov.var[[g]])

        # 1a. "=~" regular indicators
        idx <- which(partable$op == "=~" & !(partable$rhs %in% lv.names) &
                     partable$group == g)
        out[idx] <- out[idx] * OV[ match(partable$rhs[idx], ov.names) ]

        # 1b. "=~" regular higher-order lv indicators

        # 1c. "=~" indicators that are both in ov and lv
        #idx <- which(partable$op == "=~" & partable$rhs %in% ov.names
        #                             & partable$rhs %in% lv.names &
        #             partable$group == g)

        # 2. "~" regressions (and "<~")
        idx <- which((partable$op == "~" | partable$op == "<~") &
                     partable$lhs %in% ov.names &
                     partable$group == g)
        out[idx] <- out[idx] * OV[ match(partable$lhs[idx], ov.names) ]

        idx <- which((partable$op == "~" | partable$op == "<~") &
                     partable$rhs %in% ov.names &
                     partable$group == g)
        out[idx] <- out[idx] / OV[ match(partable$rhs[idx], ov.names) ]

        # 3a. "~~" ov
        # ATTENTION: in Mplus 4.1, the off-diagonal residual covariances
        #            were computed by the formula cov(i,j) / sqrt(i.var*j.var)
        #            were i.var and j.var where diagonal elements of OV
        #
        #            in Mplus 6.1 (but also AMOS and EQS), the i.var and j.var
        #            elements are the 'THETA' diagonal elements!!

        # variances
        rv.idx <- which(partable$op == "~~" & !(partable$lhs %in% lv.names) &
                        partable$lhs == partable$rhs &
                        partable$group == g)
        out[rv.idx] <- ( out[rv.idx] * OV[ match(partable$lhs[rv.idx], ov.names) ]
                                     * OV[ match(partable$rhs[rv.idx], ov.names) ] )

        # covariances
        idx <- which(partable$op == "~~" & !(partable$lhs %in% lv.names) &
                     partable$lhs != partable$rhs &
                     partable$group == g)
        if(length(idx) > 0L) {
            if(cov.std == FALSE) {
                out[idx] <- ( out[idx] * OV[ match(partable$lhs[idx], ov.names) ]
                                       * OV[ match(partable$rhs[idx], ov.names) ] )
            } else {
                # RV   <- sqrt(est[rv.idx])
                RV   <- sqrt(out[rv.idx])
                rv.names <- partable$lhs[rv.idx]
                out[idx] <- ( out[idx] * RV[ match(partable$lhs[idx], rv.names) ]
                                       * RV[ match(partable$rhs[idx], rv.names) ] )
            }
        }

        # 3b. "~~" lv
        #idx <- which(partable$op == "~~" & partable$rhs %in% lv.names &
        #             partable$group == g)

        # 4a. "~1" ov
        idx <- which(partable$op == "~1" & !(partable$lhs %in% lv.names) &
                     partable$group == g)
        out[idx] <- out[idx] * OV[ match(partable$lhs[idx], ov.names) ]

        # 4b. "~1" lv
        #idx <- which(partable$op == "~1" & partable$lhs %in% lv.names &
        #             partable$group == g)

    }

    # 5a ":="
    # 5b "=="
    # 5c. "<" or ">"

    out
}
