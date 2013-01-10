## Higher level wrapper function for createData and drawParameters. Takes a
## SimSem analysis/data generation template, and returns a raw data set,
## optionally with the drawn parameter values.

generate <- function(model, n, maxDraw = 50, misfitBounds = NULL, misfitType = "f0", 
    averageNumMisspec = FALSE, optMisfit = NULL, optDraws = 50, createOrder = c(1, 2, 3), indDist = NULL, sequential = FALSE, 
    facDist = NULL, errorDist = NULL, indLab = NULL, modelBoot = FALSE, realData = NULL, covData = NULL, 
    params = FALSE) {
    if (is.null(indLab)) {
        if (model@modelType == "path") {
            indLab <- unique(model@pt$lhs)
        } else {
            indLab <- unique(model@pt$rhs[model@pt$op == "=~"])
        }
    }
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
		if(!is.null(covData) && (length(covLab) > 0)) {
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
			warnings("CONFLICT: The model template does not have any covariates but the covaraite data are specified. The covaraite data are ignored.")
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
		covData = covDataGroup, MoreArgs = list(sequential = sequential, modelBoot = modelBoot, indLab = indLab), 
        SIMPLIFY = FALSE)
    data <- do.call("rbind", datal)
    data <- cbind(data, group = rep(1:ngroups, n))
    colnames(data)[ncol(data)] <- model@groupLab
    
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

test.changeScaleSEM <- function() {
	loading <- matrix(0, 8, 3)
	loading[1:3, 1] <- NA
	loading[4:6, 2] <- NA
	loading[7:8, 3] <- "con1"
	loading.start <- matrix("", 8, 3)
	loading.start[1:3, 1] <- 0.7
	loading.start[4:6, 2] <- 0.7
	loading.start[7:8, 3] <- "rnorm(1,0.6,0.05)"
	LY <- bind(loading, loading.start)

	RTE <- binds(diag(8))

	factor.cor <- diag(3)
	factor.cor[1, 2] <- factor.cor[2, 1] <- NA
	RPS <- binds(factor.cor, 0.5)

	path <- matrix(0, 3, 3)
	path[3, 1:2] <- NA
	path.start <- matrix(0, 3, 3)
	path.start[3, 1] <- "rnorm(1,0.6,0.05)"
	path.start[3, 2] <- "runif(1,0.3,0.5)"
	BE <- bind(path, path.start)

	SEM.model <- model(BE=BE, LY=LY, RPS=RPS, RTE=RTE, modelType="SEM")

	dat <- generate(SEM.model, n=300)

	# Manifest variable approach



	loading <- matrix(0, 8, 3)
	loading[1:3, 1] <- c(1, "con1", "con1")
	loading[4:6, 2] <- c(1, "con2", "con2")
	loading[7:8, 3] <- c(1, 1)
	loading.start <- matrix("", 8, 3)
	LY <- bind(loading, 0.9)

	RTE <- binds(diag(8))

	factor.cor <- diag(3)
	factor.cor[1, 2] <- factor.cor[2, 1] <- NA
	RPS <- binds(factor.cor, 0.5)

	VPS <- bind(rep(NA, 3), c(1.3, 1.6, 0.9))

	path <- matrix(0, 3, 3)
	path[3, 1:2] <- NA
	path.start <- matrix(0, 3, 3)
	path.start[3, 1] <- "rnorm(1,0.6,0.05)"
	path.start[3, 2] <- "runif(1,0.3,0.5)"
	BE <- bind(path, path.start)

	VTE <- bind(rep(NA, 8), 1)
	SEM.model <- model(BE=BE, LY=LY, RPS=RPS, RTE=RTE, VPS=VPS, VTE=VTE, modelType="SEM")

	dat <- generate(SEM.model, n=300)

	# Mixed approach

	loading <- matrix(0, 8, 3)
	loading[1:3, 1] <- c(1, "con1", "con1")
	loading[4:6, 2] <- c(1, "con2", "con2")
	loading[7:8, 3] <- c("con3", "con3")
	loading.start <- matrix("", 8, 3)
	LY <- bind(loading, 0.7)

	RTE <- binds(diag(8))

	factor.cor <- diag(3)
	factor.cor[1, 2] <- factor.cor[2, 1] <- NA
	RPS <- binds(factor.cor, 0.5)

	VPS <- bind(c(NA, NA, 0.9), c(1.3, 0.7, ""))

	path <- matrix(0, 3, 3)
	path[3, 1:2] <- NA
	path.start <- matrix(0, 3, 3)
	path.start[3, 1] <- "rnorm(1,0.6,0.05)"
	path.start[3, 2] <- "runif(1,0.3,0.5)"
	BE <- bind(path, path.start)

	VTE <- bind(rep(NA, 8), 1)
	SEM.model <- model(BE=BE, LY=LY, RPS=RPS, RTE=RTE, VPS=VPS, VTE=VTE, modelType="SEM")

	dat <- generate(SEM.model, n=300)
	
	
	# Constrain


	loading <- matrix(0, 9, 3)
	loading[1:3, 1] <- c("con1", "con2", "con3")
	loading[4:6, 2] <- c("con1", "con2", "con3")
	loading[7:9, 3] <- c("con1", "con2", "con3")
	LY <- bind(loading, 0.7)

	RTE <- binds(diag(9))

	RPS <- binds(diag(3))

	VPS <- bind(c(1, NA, NA), c("", 0.75, 0.75))

	path <- matrix(0, 3, 3)
	path[2, 1] <- NA
	path[3, 2] <- NA
	BE <- bind(path, 0.5)

	VTE <- bind(rep(NA, 9), 1)
	SEM.model <- model(BE=BE, LY=LY, RPS=RPS, RTE=RTE, VPS=VPS, VTE=VTE, modelType="SEM")

	dat <- generate(SEM.model, n=300)

	# Constrain Group

	loading.in <- matrix(0, 9, 3)
	loading.in[1:3, 1] <- paste0("load", 1:3)
	loading.in[4:6, 2] <- paste0("load", 4:6)
	loading.in[7:9, 3] <- paste0("load", 7:9)
	LY.in <- bind(loading.in, 0.7)

	RPS <- binds(diag(3))

	RTE <- binds(diag(9))

	VTE <- bind(rep(NA, 9), 0.51)

	TY.in <- bind(paste0("int", 1:9), 0)

	VPS1 <- bind(rep(1, 3))
	VPS2 <- bind(rep(NA, 3), c(1.1, 1.2, 1.3))

	AL1 <- bind(rep(0, 3))
	AL2 <- bind(rep(NA, 3), c(-0.5, 0.2, 0.3))

	path <- matrix(0, 3, 3)
	path[2, 1] <- NA
	path[3, 2] <- NA
	BE <- bind(path, 0.5)

	strong <- model(LY = LY.in, RPS = RPS, VPS=list(VPS1, VPS2), RTE = RTE, VTE=VTE, TY=TY.in, AL=list(AL1, AL2), BE=list(BE,BE), ngroups=2, modelType = "SEM")

	dat <- generate(strong,200)
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
