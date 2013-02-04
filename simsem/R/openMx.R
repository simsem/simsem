# Only work for single-group for now.

generateMx <- function(object, n, indDist = NULL, groupLab = NULL, covData = NULL) {
	library(OpenMx)
	if(length(object@submodels) > 1) {
		ngroups <- length(object@submodels)
		if(!is.list(n)) n <- as.list(n)
		if(!is.list(indDist)) indDist <- list(indDist)
		if(length(n) == 1) n <- rep(n, ngroups)
		if(length(indDist) == 1) indDist <- rep(indDist, ngroups)

		# Separate covData
		if(is.null(covData)) {
			covData.l <- rep(list(NULL), ngroups)
		} else {
			if(is.null(groupLab)) groupLab <- "group"
			covData.l <- split(covData, covData[,groupLab])
			covData.l <- lapply(covData.l, function(x) x[-ncol(x)])
		}
		upperLevelMatrices <- getInnerObjects(object)
		if(length(upperLevelMatrices) > 0) {
			names(upperLevelMatrices) <- paste0(object@name, ".", names(upperLevelMatrices))
		}
		data.l <- mapply(generateMxSingleGroup, object=object@submodels, n=n, indDist=indDist, covData=covData.l, MoreArgs=list(extraMatrices = upperLevelMatrices), SIMPLIFY=FALSE)
		if(!is.null(covData)) {
			data.l <- mapply(data.frame, data.l, covData.l, SIMPLIFY = FALSE)
		}
		data.l <- mapply(data.frame, data.l, group = 1:ngroups, SIMPLIFY = FALSE)
		
		data <- do.call(rbind, data.l)
		rownames(data) <- NULL
		if(!is.null(groupLab)) colnames(data)[ncol(data)] <- groupLab
	} else {
		data <- generateMxSingleGroup(object, n, indDist, covData)
	}
	data
}

getInnerObjects <- function(xxxobjectxxx) {
	xxxmatxxx <- xxxobjectxxx@matrices
	xxxmatnamexxx <- names(xxxmatxxx)
	xxxmatvalxxx <- lapply(xxxmatxxx, slot, "values")
	for(i in seq_along(xxxmatnamexxx)) {
		assign(xxxmatnamexxx[i], xxxmatvalxxx[[i]])
	}
	xxxalgebraxxx <- xxxobjectxxx@algebras
	xxxalgebranamexxx <- names(xxxalgebraxxx)
	xxxalgebraformulaxxx <- lapply(xxxalgebraxxx, slot, "formula")
	xxxalgebraassignedxxx <- NULL
	for(i in seq_along(xxxalgebranamexxx)) {
		temp <- NULL
		try(temp <- eval(xxxalgebraformulaxxx[[i]]), silent = TRUE)
		if(!is.null(temp)) {
			assign(xxxalgebranamexxx[i], temp)
			xxxalgebraassignedxxx <- c(xxxalgebraassignedxxx, xxxalgebranamexxx[i])
		}
	}
	xxxusednamexxx <- c(xxxmatnamexxx, xxxalgebraassignedxxx)
	xxxresultxxx <- list()
	for(i in seq_along(xxxusednamexxx)) {
		xxxresultxxx[[i]] <- get(xxxusednamexxx[i])
	}
	names(xxxresultxxx) <- xxxusednamexxx
	xxxresultxxx	
}

generateMxSingleGroup <- function(object, n, indDist = NULL, covData = NULL, extraMatrices = NULL) {

	if(is(object@objective, "MxRAMObjective")) {
		# Create F, S, and M to suppress warnings when compiling the package.
		F <- NULL
		S <- NULL
		M <- NULL
		nfac <- nrow(object@matrices$A@values)
		I <- mxMatrix(type = "Iden", nrow=nfac, ncol=nfac, free=FALSE, name="I")
		Z <- mxAlgebra(expression=solve(I-A), name="Z")
		impliedCov <- mxAlgebra(expression=F %*% Z %*% S %*% t(Z) %*% t(F), name="impliedCov")
		allMatrices <- c(object@matrices, I = I)
		allAlgebras <- c(object@algebras, Z = Z, impliedCov = impliedCov)
		if(!is.null(object@matrices$M)) {
			impliedMean <- mxAlgebra(expression=t(F %*% Z %*% t(M)), name="impliedMean")
			allAlgebras <- c(allAlgebras, impliedMean = impliedMean)
			newObjective <- mxFIMLObjective(
				covariance="impliedCov", 
				means="impliedMean", 
				dimnames=object@objective@dims, 
				thresholds=object@objective@thresholds
			)
		} else {
			newObjective <- mxMLObjective(
				covariance="impliedCov", 
				dimnames=object@objective@dims, 
				thresholds=object@objective@thresholds
			)
		}
		
		object@matrices <- allMatrices
		object@algebras <- allAlgebras
		object@objective <- newObjective
	}
	if (!is(object@objective, "MxMLObjective") & !is(object@objective, "MxFIMLObjective")) {
		stop("This function supports only MxRAMObjective, MxMLObjective, or MxFIMLObjective.")
	}
	defVars <- findDefVars(object)	
	if(length(defVars) > 0) {
		if(is.null(covData)) stop("Please specify the covData argument because the specified model has a definition variable.")
		covData.l <- as.list(data.frame(t(covData)))
		covData.l <- lapply(covData.l, function(x, name) {names(x) <- name; x}, name=colnames(covData))
		macs <- lapply(covData.l, getImpliedStatML, xxxobjectxxx = object, xxxextraxxx = extraMatrices)
		impliedMean <- lapply(macs, "[[", 1)
		impliedCov <- lapply(macs, "[[", 2)
		impliedThreshold <- lapply(macs, "[[", 3)
		Data.l <- mapply(dataGen, m=impliedMean, cm=impliedCov, MoreArgs=list(n=1, dataDist=indDist), SIMPLIFY=FALSE)
		if(!all(is.na(object@objective@thresholds))) {
			FUN <- function(x, thres) {
				for(i in colnames(thres)) {
						thresholdVal <- c(-Inf, setdiff(thres[,i], NA), Inf)
						temp <- cut(as.vector(x[,i]),thresholdVal)
						lev <- 1:(length(setdiff(thres[,i], NA))+1)
						x[,i] <- factor(as.numeric(temp), levels= lev, labels=lev, exclude=NA, ordered=TRUE)
					}
				x
			}
			Data.l <- mapply(FUN, x=Data.l, thres=impliedThreshold, SIMPLIFY=FALSE)
		}
		Data <- data.frame(do.call(rbind, Data.l))
		rownames(Data) <- NULL
		varnames <- object@objective@dims
		if(any(is.na(varnames))) varnames <- object@manifestVars

		colnames(Data) <- varnames[1:ncol(Data)]
		if(length(varnames) == 0) varnames <- paste0("y", 1:ncol(Data))
		if(!(length(object@objective@thresholds) == 1 && is.na(object@objective@thresholds))) {
			for(i in colnames(impliedThreshold[[1]])) {
				lev <- 1:length(unique(Data[,i]))
				Data[,i] <- factor(as.numeric(Data[,i]), levels= lev, labels=lev, exclude=NA, ordered=TRUE)
			}
		}
		Data <- cbind(Data, covData)
	} else {
		implied <- getImpliedStatML(object, xxxextraxxx = extraMatrices)
		impliedCov <- implied[[2]]
		impliedMean <- implied[[1]]
		impliedThreshold <- implied[[3]]
		varnames <- object@objective@dims
		if(any(is.na(varnames))) varnames <- object@manifestVars

		Data <- dataGen(indDist, n, impliedMean, impliedCov)
		if(length(varnames) == 0) varnames <- paste0("y", 1:ncol(Data))
		colnames(Data) <- varnames[1:ncol(Data)]
		Data <- data.frame(Data)
		if(!(length(impliedThreshold) == 1 && is.na(impliedThreshold))) {
			name <- colnames(impliedThreshold)
			if(is.null(name)) {
				name <- intersect(object@objective@threshnames, colnames(Data))
				colnames(impliedThreshold) <- name
			}
			for(i in name) {
				thresholdVal <- c(-Inf, setdiff(impliedThreshold[,i], NA), Inf)
				temp <- cut(as.vector(Data[,i]),thresholdVal)
				lev <- 1:(length(setdiff(impliedThreshold[,i], NA))+1)
				Data[,i] <- factor(as.numeric(temp), levels= lev, labels=lev, exclude=NA, ordered=TRUE)
			}
		}
	}
	return(Data)
}

getImpliedStatML <- function(xxxobjectxxx, xxxcovdatatxxx = NULL, xxxextraxxx = NULL) {
	if(!is.null(xxxextraxxx)) {
		xxxmatnamexxx2 <- names(xxxextraxxx)
		for(i in seq_along(xxxmatnamexxx2)) {
			assign(xxxmatnamexxx2[i], xxxextraxxx[[i]])
		}
	}
	xxxmatxxx <- xxxobjectxxx@matrices
	xxxmatnamexxx <- names(xxxmatxxx)
	xxxmatvalxxx <- lapply(xxxmatxxx, slot, "values")
	for(i in seq_along(xxxmatnamexxx)) {
		assign(xxxmatnamexxx[i], xxxmatvalxxx[[i]])
	}
	if(!is.null(xxxcovdatatxxx)) {
		xxxmatlabxxx <- lapply(xxxmatxxx, slot, "labels")
		xxxdefvarsxxx <- lapply(xxxmatlabxxx, function(x) apply(x, c(1,2), OpenMx::imxIsDefinitionVariable))
		for(i in seq_along(xxxmatnamexxx)) {
			if(any(xxxdefvarsxxx[[i]])) {
				xxxtempxxx <- get(xxxmatnamexxx[i])
				for(j in seq_len(length(xxxdefvarsxxx[[i]]))) {
					if(xxxdefvarsxxx[[i]][j]) {
						xxxtempnamexxx <- gsub("data.", "", xxxmatlabxxx[[i]][j])
						xxxtempxxx[j] <- xxxcovdatatxxx[xxxtempnamexxx]
					}
				}
				assign(xxxmatnamexxx[i], xxxtempxxx)
			}
		}
	}
	xxxalgebraxxx <- xxxobjectxxx@algebras
	xxxalgebranamexxx <- names(xxxalgebraxxx)
	xxxalgebraformulaxxx <- lapply(xxxalgebraxxx, slot, "formula")
	for(i in seq_along(xxxalgebranamexxx)) {
		assign(xxxalgebranamexxx[i], eval(xxxalgebraformulaxxx[[i]]))
	}
	
	xxximpliedCovxxx <- get(xxxobjectxxx@objective@covariance)
	
	if(is.na(xxxobjectxxx@objective@means)) {
		xxximpliedMeanxxx <- rep(0, nrow(xxximpliedCovxxx))
	} else {
		xxximpliedMeanxxx <- get(xxxobjectxxx@objective@means)
	}
	
	if(is.na(xxxobjectxxx@objective@thresholds)) {
		xxximpliedThresholdxxx <- NA
	} else {
		xxximpliedThresholdxxx <- get(xxxobjectxxx@objective@thresholds)
	}
	list(xxximpliedMeanxxx, xxximpliedCovxxx, xxximpliedThresholdxxx)
}

analyzeMx <- function(object, data, groupLab = NULL, mxMixture = FALSE, ...) {
	if(length(object@submodels) > 1 & !mxMixture) {
		temp <- object@submodels
		if(is.null(groupLab)) groupLab <- "group"
		data.l <- split(data, data[,groupLab])
		data.l <- lapply(data.l, function(x) x[-ncol(x)])
		temp <- mapply(function(x, y) { x@data <- mxData(observed=y, type="raw");x}, x=temp, y=data.l, SIMPLIFY=FALSE)
		object@submodels <- temp
	} else {
		object@data <- mxData(observed=data,type="raw")
	}
	capture.output(fit <- mxRun(object, ...))
	return(fit)
}

findDefVars <- function(object) {
	mat <- lapply(object@matrices, slot, "labels")
	defvars <- sapply(mat, function(x) x[apply(x, c(1,2), OpenMx::imxIsDefinitionVariable)])
	Reduce("c", defvars)
}

vectorizeMx <- function(object) {
	multigroup <- length(object@submodels) > 0
	if(multigroup) {
		object <- c(list(object), object@submodels)
	} else {
		object <- list(object)	
	}
	result <- NULL
	for(i in seq_along(object)) {
		name <- ""
		if(multigroup) name <- paste0(object[[i]]@name, ".")
		mat <- object[[i]]@matrices
		for(j in seq_along(mat)) {
			tempname <- paste0(name, mat[[j]]@name)
			lab <- mat[[j]]@labels
			free <- as.vector(mat[[j]]@free)
			madeLab <- paste0(tempname, "[", row(lab), ",", col(lab), "]")
			lab <- as.vector(lab)
			madeLab[!is.na(lab)] <- lab[!is.na(lab)]
			temp <- mat[[j]]@values[free]
			names(temp) <- madeLab[free]
			result <- c(result, temp)
		}
	}
	result[!duplicated(names(result))]
}

easyFitMx <- function(object, mxMixture = FALSE) {
	library(OpenMx)
	
	if(length(object@submodels) > 1 & !mxMixture) {
		dat <- lapply(object@submodels, slot, "data")
	} else {
		dat <- object@data
	}
	
	if(length(object@submodels) > 1 & !mxMixture) {
		N <- sum(sapply(dat, slot, "numObs"))
	} else {
		N <- dat@numObs
	}

    npar <- length(object@output$estimate)

    multigroup <- length(object@submodels) > 1
    G <- length(object@submodels) # number of groups
	if(G == 0) G <- 1 # Correct when there is no submodel

    # main container
    indices <- list()
		
	logl.H0 <- object@output$Minus2LogLikelihood * (-1/2)
	indices["logl"] <- logl.H0
	indices["npar"] <- npar

	AIC <-  -2*logl.H0 + 2*npar
	indices["aic"] <- AIC

	BIC <- -2*logl.H0 + npar*log(N)
	indices["bic"] <- BIC

	# add sample-size adjusted bic
	N.star <- (N + 2) / 24
	BIC2 <- -2*logl.H0 + npar*log(N.star)
	indices["bic2"] <- BIC2
	out <- unlist(indices)
    return(out)
}
