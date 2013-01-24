# Only work for single-group for now.

generateOpenMx <- function(object, n, indDist = NULL, groupLab = NULL) {
	library(OpenMx)
	if(length(object@submodels) > 1) {
		ngroups <- length(object@submodels)
		if(!is.list(n)) n <- list(n)
		if(!is.list(indDist)) indDist <- list(indDist)
		if(length(n) == 1) n <- rep(n, ngroups)
		if(length(indDist) == 1) indDist <- rep(indDist, ngroups)
		data.l <- mapply(generateOpenMxSingleGroup, object=object@submodels, n=n, indDist=indDist, SIMPLIFY=FALSE)
		data.l <- mapply(data.frame, data.l, group = 1:ngroups, SIMPLIFY = FALSE)
		data <- do.call(rbind, data.l)
		rownames(data) <- NULL
		if(!is.null(groupLab)) colnames(data)[ncol(data)] <- groupLab
	} else {
		data <- generateOpenMxSingleGroup(object, n, indDist)
	}
	data
}

generateOpenMxSingleGroup <- function(object, n, indDist = NULL) {
	if(is(object@objective, "MxRAMObjective")) {
		A <- object@matrices$A@values
		I <- diag(nrow(A))
		S <- object@matrices$S@values
		F <- object@matrices$F@values
		Z <- solve(I - A)
		impliedCov <- F %*% Z %*% S %*% t(Z) %*% t(F)
		if(!is.null(object@matrices$M)) {
			M <- object@matrices$M@values
			impliedMean <- t(F %*% Z %*% t(M))
		} else {
			impliedMean <- rep(0, nrow(impliedCov))
		}
	} else if (is(object@objective, "MxMLObjective") | is(object@objective, "MxFIMLObjective")) {
		implied <- getImpliedStatML(object)
		impliedCov <- implied[[2]]
		impliedMean <- implied[[1]]
	} else {
		stop("This function supports only MxRAMObjective, MxMLObjective, or MxFIMLObjective.")
	}
	varnames <- object@objective@dims
	if(any(is.na(varnames))) varnames <- object@manifestVars

	Data <- dataGen(indDist, n, impliedMean, impliedCov)
	colnames(Data) <- varnames[1:ncol(Data)]
	return(data.frame(Data))
}

getImpliedStatML <- function(xxxobjectxxx) {
	xxxmatxxx <- xxxobjectxxx@matrices
	xxxmatnamexxx <- names(xxxmatxxx)
	xxxmatvalxxx <- lapply(xxxmatxxx, slot, "values")
	for(i in 1:length(xxxmatnamexxx)) {
		assign(xxxmatnamexxx[i], xxxmatvalxxx[[i]])
	}
	xxxalgebraxxx <- xxxobjectxxx@algebras
	xxxalgebranamexxx <- names(xxxalgebraxxx)
	xxxalgebraformulaxxx <- lapply(xxxalgebraxxx, slot, "formula")
	for(i in 1:length(xxxalgebranamexxx)) {
		assign(xxxalgebranamexxx[i], eval(xxxalgebraformulaxxx[[i]]))
	}
	xxximpliedCovxxx <- get(xxxobjectxxx@objective@covariance)
	
	if(is.na(xxxobjectxxx@objective@means)) {
		xxximpliedMeanxxx <- rep(0, nrow(xxximpliedCovxxx))
	} else {
		xxximpliedMeanxxx <- get(xxxobjectxxx@objective@means)
	}
	list(xxximpliedMeanxxx, xxximpliedCovxxx)
}


analyzeOpenMx <- function(object, data, groupLab = NULL) {
	if(length(object@submodels) > 1) {
		temp <- object@submodels
		if(is.null(groupLab)) groupLab <- "group"
		data.l <- split(data, data[,groupLab])
		data.l <- lapply(data.l, function(x) x[-ncol(x)])
		temp <- mapply(function(x, y) { x@data <- mxData(observed=y, type="raw");x}, x=temp, y=data.l, SIMPLIFY=FALSE)
		object@submodels <- temp
	} else {
		object@data <- mxData(observed=data,type="raw")
	}
	capture.output(fit <- mxRun(object))
	return(fit)
}

analyzeSaturateOpenMx <- function(data, groupLab = NULL) {
	if(!is.null(groupLab) && groupLab %in% colnames(data)) {
		data.l <- split(data, data[,groupLab])
		data.l <- lapply(data.l, function(x) x[-ncol(x)])
		ngroups <- length(data.l)
		temp <- mapply(analyzeSaturateOpenMxSingleGroup, data = data.l, title = paste0("group", 1:ngroups), groupnum = 1:ngroups, SIMPLIFY=FALSE)
		title <- "Multiple group Saturate Model"
		algebra <- mxAlgebra("", name="allobjective")
		groupnames <- paste0("group", 1:ngroups)
		groupnames <- paste0(groupnames, ".objective")
		groupnames <- lapply(groupnames, as.name)
		algebra@formula <- as.call(c(list(as.name("sum")), groupnames))
		objective <- mxAlgebraObjective("allobjective")
		Saturate <- mxModel(title, unlist(temp), algebra, objective)
	} else {
		Saturate <- analyzeSaturateOpenMxSingleGroup(data, title = "Saturate Model")
	}
	capture.output(fit <- mxRun(Saturate))
	fit
}

analyzeSaturateOpenMxSingleGroup <- function(data, title = "Saturate Model", groupnum = NULL) {
	p <- ncol(data)
	startMeans <- colMeans(data, na.rm=TRUE)
	startCov <- cov(data, use="pairwise.complete.obs")
	lab <- outer(1:p, 1:p, function(x, y) paste0("cov", x, y, "_", groupnum))
	lab2 <- outer(1:p, 1:p, function(x, y) paste0("cov", y, x, "_", groupnum))
	lab[upper.tri(lab)] <- lab2[upper.tri(lab2)]
	Saturate <- mxModel(title,
		mxData(
			observed=data,
			type="raw"
		),
		# means
		mxMatrix(
			type="Full",
			nrow=1,
			ncol=p,
			values=startMeans,
			free=rep(TRUE, p),
			labels=paste0("mean", 1:p, "_", groupnum),
			name="M"
		),
		# symmetric paths
		mxMatrix(
			type="Symm",
			nrow=p,
			ncol=p,
			values=startCov,
			free=matrix(TRUE, p, p),
			labels=lab,
			byrow=TRUE,
			name="S"
		),
		mxFIMLObjective(
			covariance="S",
			means="M",
			dimnames=colnames(data)
		)
	)
	Saturate
}

analyzeNullOpenMx <- function(data, groupLab = NULL) {
	if(!is.null(groupLab) && groupLab %in% colnames(data)) {
		data.l <- split(data, data[,groupLab])
		data.l <- lapply(data.l, function(x) x[-ncol(x)])
		ngroups <- length(data.l)
		temp <- mapply(analyzeNullOpenMxSingleGroup, data = data.l, title = paste0("group", 1:ngroups), groupnum = 1:ngroups, SIMPLIFY=FALSE)
		title <- "Multiple group Null Model"
		algebra <- mxAlgebra("", name="allobjective")
		groupnames <- paste0("group", 1:ngroups)
		groupnames <- paste0(groupnames, ".objective")
		groupnames <- lapply(groupnames, as.name)
		algebra@formula <- as.call(c(list(as.name("sum")), groupnames))
		objective <- mxAlgebraObjective("allobjective")
		Null <- mxModel(title, unlist(temp), algebra, objective)
	} else {
		Null <- analyzeNullOpenMxSingleGroup(data, title = "Null Model")
	}
	capture.output(fit <- mxRun(Null))
	fit
}

analyzeNullOpenMxSingleGroup <- function(data, title = "Null Model", groupnum = NULL) {
	p <- ncol(data)
	startMeans <- colMeans(data, na.rm=TRUE)
	startVar <- apply(data, 2, var, na.rm=TRUE)
	lab <- paste0("var", 1:p, "_", groupnum)
	NullModel <- mxModel(title,
		mxData(
			observed=data,
			type="raw"
		),
		# means
		mxMatrix(
			type="Full",
			nrow=1,
			ncol=p,
			values=startMeans,
			free=rep(TRUE, p),
			labels=paste0("mean", 1:p, "_", groupnum),
			name="M"
		),
		# symmetric paths
		mxMatrix(
			type="Diag",
			nrow=p,
			ncol=p,
			values=startVar,
			free=rep(TRUE, p),
			labels=lab,
			byrow=TRUE,
			name="S"
		),
		mxFIMLObjective(
			covariance="S",
			means="M",
			dimnames=colnames(data)
		)
	)
	capture.output(fit <- mxRun(NullModel))
	fit
}





# Need to update for categorical variables and multiple groups
fitMeasuresCalc <- function(X2, df, p = NULL, X2.null, df.null, p.null = NULL, N, fit.measures = "all") {
	if(is.null(p)) p <- pchisq(X2, df=df, lower.tail=FALSE)
	if(is.null(p.null)) p.null <- pchisq(X2.null, df=df.null, lower.tail=FALSE)
    
    fit.measures <- tolower(fit.measures)
    if ("all" %in% fit.measures) {
        chisq <- c("chisq", "df", "pvalue")
        baseline <- c("baseline.chisq", "baseline.df", "baseline.pvalue")
        cfi.tli <- c("cfi", "tli")
        logl <- c("logl", "unrestricted.logl", "npar", "aic", "bic", "ntotal")
        logl <- c(logl, "bic2")
        rmsea.ci <- c("rmsea", "rmsea.ci.lower", "rmsea.ci.upper")
        srmr <- c("srmr")
        fit.measures <- c(chisq, baseline, cfi.tli, logl, rmsea.ci, srmr)
    }
    indices <- list()
    
    # Chi-square value estimated model (H0)
    if (any(c("chisq") %in% fit.measures)) {
        indices["Chi"] <- X2
    }
    if (any(c("df", "df.scaled") %in% fit.measures)) {
        indices["df"] <- df
    }
    if (any(c("pvalue", "pvalue.scaled") %in% fit.measures)) {
        indices["pvalue"] <- p
    }
    
    
    if (any(c("cfi", "tli", "baseline.chisq", "baseline.pvalue") %in% fit.measures)) {
        if ("baseline.chisq" %in% fit.measures) {
            indices["baseline.Chi"] <- X2.null
        }
        if ("baseline.df" %in% fit.measures) {
            indices["baseline.df"] <- df.null
        }
        if ("baseline.pvalue" %in% fit.measures) {
            indices["baseline.pvalue"] <- p.null
        }
        # CFI
        if ("cfi" %in% fit.measures) {
            indices["CFI"] <- (1 - max(c(X2 - df, 0))/max(c(X2 - df, X2.null - df.null, 0)))
        }
        # TLI
        if ("tli" %in% fit.measures) {
            if (df > 0) {
                TLI <- (X2.null/df.null - X2/df)/(X2.null/df.null - 1)
                # if(TLI < 0) TLI <- NaN
            } else {
                TLI <- 1
            }
            indices["TLI"] <- TLI
        }
    }
    
    if (any(c("rmsea") %in% fit.measures)) {
        # RMSEA
        if (df > 0) {
            RMSEA <- sqrt(max(c((X2/N)/df - 1/N, 0)))
        } else {
            RMSEA <- 0
        }
        indices["RMSEA"] <- RMSEA
    }
    
    if ("rmsea.ci.lower" %in% fit.measures) {
        lower.lambda <- function(lambda) {
            (pchisq(X2, df = df, ncp = lambda) - 0.95)
        }
        
        if (df < 1 || lower.lambda(0) < 0) {
            indices["RMSEA.ci.lower"] <- 0
        } else {
            lambda.l <- try(uniroot(f = lower.lambda, lower = 0, upper = X2)$root)
            if (inherits(lambda.l, "try-error")) {
                lambda.l <- NA
            }
            indices["RMSEA.ci.lower"] <- sqrt(lambda.l/(N * df))
        }
    }
    
    N.RMSEA <- max(N, X2 * 2)
    if ("rmsea.ci.upper" %in% fit.measures) {
        upper.lambda <- function(lambda) {
            (pchisq(X2, df = df, ncp = lambda) - 0.05)
        }
        if (df < 1 || upper.lambda(N.RMSEA) > 0 || upper.lambda(0) < 0) {
            indices["RMSEA.ci.upper"] <- 0
        } else {
            lambda.u <- try(uniroot(f = upper.lambda, lower = 0, upper = N.RMSEA)$root)
            if (inherits(lambda.u, "try-error")) {
                lambda.u <- NA
            }
            indices["RMSEA.ci.upper"] <- sqrt(lambda.u/(N * df))
        }
    }
    
    out <- unlist(indices)
    
    if (length(out) > 0L) {
        class(out) <- c("lavaan.vector", "numeric")
    } else {
        return(invisible(numeric(0)))
    }
    
    out
}
