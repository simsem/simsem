# plotPowerFit: This function will plot sampling distributions of fit indices that visualize power in detecting misspecified models

plotPowerFit <- function(altObject, nullObject = NULL, cutoff = NULL, usedFit = NULL, alpha = 0.05, contN = TRUE, contMCAR = TRUE, contMAR = TRUE, useContour = TRUE, logistic = TRUE) {
	if(is.null(nullObject)) {
		altObject <- clean(altObject)
    } else {
		mod <- clean(altObject, nullObject)
		altObject <- mod[[1]]
		nullObject <- mod[[2]]
		if(!isTRUE(all.equal(unique(altObject@n), unique(nullObject@n)))) stop("Models are based on different values of sample sizes")
		if(!isTRUE(all.equal(unique(altObject@pmMCAR), unique(nullObject@pmMCAR)))) stop("Models are based on different values of the percent completely missing at random")
		if(!isTRUE(all.equal(unique(altObject@pmMAR), unique(nullObject@pmMAR)))) stop("Models are based on different values of the percent missing at random")
	}
	nrep <- dim(altObject@fit)[[1]]
	if (is.null(usedFit)) 
        usedFit <- getKeywords()$usedFit
	if (!is.null(cutoff)) {
		usedFit <- intersect(usedFit, names(cutoff))
		cutoff <- cutoff[usedFit]
    }
    # Create matrix of predictors (randomly varying params)
    x <- NULL
    pred <- NULL
    
    if ((length(unique(altObject@n)) > 1) && contN) {
        if (!length(altObject@n) == nrep) {
            stop("Number of random sample sizes is not the same as the number of replications, check to see if N varied across replications")
        }
        x <- cbind(x, N = altObject@n)
        pred$N <- min(altObject@n):max(altObject@n)
    }
    if ((length(unique(altObject@pmMCAR)) > 1) && contMCAR) {
        if (!length(altObject@pmMCAR) == nrep) {
            stop("Number of random pmMCARs is not the same as the number of replications, check to see if pmMCAR varied across replications")
        }
        x <- cbind(x, pmMCAR = altObject@pmMCAR)
        pred$MCAR <- seq(min(altObject@pmMCAR), max(altObject@pmMCAR), by = 0.01)
        
    }
    if ((length(unique(altObject@pmMAR)) > 1) && contMAR) {
        if (!length(altObject@pmMAR) == nrep) {
            stop("Number of random pmMARs is not the same as the number of replications, check to see if pmMAR varied across replications")
        }
        x <- cbind(x, pmMAR = altObject@pmMAR)
        pred$MAR <- seq(min(altObject@pmMAR), max(altObject@pmMAR), by = 0.01)
        
    }
	if(!is.null(nullObject)) nullObject <- nullObject@fit
	plotPowerFitDf(altObject@fit, nullObject = nullObject, cutoff = cutoff, usedFit = usedFit, alpha = alpha, x = x, xval = pred, useContour = useContour, logistic = logistic)
}

# plotPowerFitDf: This function will plot sampling distributions of fit indices that visualize power in detecting misspecified models where the inputs are data.frame

plotPowerFitDf <- function(altObject, nullObject = NULL, cutoff = NULL, usedFit = NULL, alpha = 0.05, x = NULL, xval = NULL, useContour = TRUE, logistic = TRUE) {
	if(is.null(x)) {
		if(is.null(nullObject)) {
			plotCutoff(altObject, cutoff, usedFit = usedFit)
		} else {
			plotOverHist(altObject, nullObject, cutoff=cutoff, usedFit=usedFit, alpha=alpha)
			# Plot overlapping histogram; Optional for specifying cutoff or derived cutoff
		}
	} else if(ncol(x) == 1) {
		if(logistic & (!is.null(nullObject) | !is.null(cutoff))) {
			plotLogisticFit(altObject, nullObject=nullObject, cutoff=cutoff, usedFit=usedFit, x=x, xval=xval, alpha=alpha, useContour=useContour)
		} else {
			plotScatter(altObject, nullObject=nullObject, cutoff=cutoff, usedFit = usedFit, x=x, alpha=alpha)
		# Plot scatterplot if only one continuous; Optional for putting horizontal cutoff
		# If the cutoff exists, the power plot can be used.
		}
	} else if(ncol(x) == 2) {
		if(logistic & (!is.null(nullObject) | !is.null(cutoff))) {
			plotLogisticFit(altObject, nullObject=nullObject, cutoff=cutoff, usedFit=usedFit, x=x, xval=xval, alpha=alpha, useContour=useContour)
		} else {
			stop("Cannot make scatter plot with two or more varying variables")
		}
		# If the cutoff exists, the power 2/3D plot can be used.
	} else {
		stop("The varying parameter used cannot be over two dimensions.")
	}
}

# plotOverHist: Plot overlapping distributions with specified cutoffs

plotOverHist <- function(altObject, nullObject, cutoff=NULL, usedFit=NULL, alpha=0.05, cutoff2=NULL, cutoff3=NULL, cutoff4=NULL) {
	percentile <- 1 - alpha
	if (is.null(usedFit)) 
        usedFit <- getKeywords()$usedFit
	if(is.null(cutoff)) { 
		cutoff <- getCutoff(nullObject, alpha, usedFit = usedFit)
		names(cutoff) <- usedFit
	}
	usedFit <- intersect(usedFit, names(cutoff))
	cutoff <- cutoff[usedFit]
    altObject <- as.data.frame(altObject[, usedFit])
    nullObject <- as.data.frame(nullObject[, usedFit])
    colnames(altObject) <- usedFit
    colnames(nullObject) <- usedFit
    no.NA.altObject <- !apply(altObject, 2, function(vec) all(is.na(vec)))
    no.NA.nullObject <- !apply(nullObject, 2, function(vec) all(is.na(vec)))
    temp.name.alt <- colnames(altObject)[no.NA.altObject]
    temp.name.null <- colnames(nullObject)[no.NA.nullObject]
    altObject <- as.data.frame(altObject[, no.NA.altObject])
    nullObject <- as.data.frame(nullObject[, no.NA.nullObject])
    colnames(altObject) <- temp.name.alt
    colnames(nullObject) <- temp.name.null
    common.name <- intersect(colnames(altObject), colnames(nullObject))
    altObject <- as.data.frame(altObject[, common.name])
    nullObject <- as.data.frame(nullObject[, common.name])
    colnames(altObject) <- colnames(nullObject) <- common.name
    cutoff <- cutoff[common.name]
	if(!is.null(cutoff2)) cutoff2 <- cutoff2[common.name]
	if(!is.null(cutoff3)) cutoff3 <- cutoff3[common.name]
	if(!is.null(cutoff4)) cutoff4 <- cutoff4[common.name]
    if (length(common.name) == 2) {
        obj <- par(mfrow = c(1, 2))
    } else if (length(common.name) == 3) {
        obj <- par(mfrow = c(1, 3))
    } else if (length(common.name) > 3) {
        obj <- par(mfrow = c(2, ceiling(length(common.name)/2)))
    } else if (length(common.name) == 1) {
        # Intentionally leaving as blank
    } else {
        stop("Some errors occur")
    }
    for (i in 1:length(common.name)) {
        swap <- sum(common.name[i] == c("CFI", "TLI")) > 0
        overlapHist(nullObject[, i], altObject[, i], main = common.name[i], xlab = "Value", colors = c("yellow", "skyblue", "lightgreen"), 
            swap = swap)
        abline(v = cutoff[i], lty = 1, lwd = 3, col="red")
		if(!is.null(cutoff2)) abline(v = cutoff2[i], lty = 1, lwd = 3, col="red")
		if(!is.null(cutoff3)) abline(v = cutoff3[i], lty = 1, lwd = 3, col="blue")
		if(!is.null(cutoff4)) abline(v = cutoff4[i], lty = 1, lwd = 3, col="blue")
        position <- "topright"
        if (swap) 
            position <- "topleft"
        legend(position, c("Null", "Alternative"), cex = 1, bty = "n", fill = c("yellow", "skyblue"))
    }
    if (length(common.name) > 1) 
        par(obj)
}

# plotLogisticFit: Plot the logistic curves of predicting the rejection/retention of a hypothesized model

plotLogisticFit <- function(altObject, nullObject=NULL, cutoff=NULL, usedFit=NULL, x, xval, alpha=0.05, useContour=TRUE, df=0) {
    warnT <- as.numeric(options("warn"))
    options(warn = -1)
	if(is.null(nullObject) & is.null(cutoff)) stop("Please specify the nullObject or cutoff argument")
	if (is.null(usedFit)) 
        usedFit <- getKeywords()$usedFit
	if (!is.null(cutoff)) {
		usedFit <- intersect(usedFit, names(cutoff))
		cutoff <- cutoff[usedFit]
    }
	if(is.null(cutoff)) {
		nullFit <- as.data.frame(nullObject[,usedFit])
		colnames(nullFit) <- usedFit
		temp <- rep(NA, length(usedFit))
		cutoff <- getCutoff(object=nullFit, alpha = alpha, revDirec = FALSE, usedFit = usedFit, predictor = x, df = df, predictorVal="all")
		if(!is.matrix(cutoff)) {
			cutoff <- as.matrix(cutoff)
		}
	}
	if(is.matrix(cutoff)) {
		sig <- as.matrix(altObject[,usedFit] > cutoff)
		colnames(sig) <- usedFit
	} else {
		sig <- mapply(function(dat, x) dat > x, dat=altObject[,usedFit], x=as.list(cutoff))
	}
	reverse <- colnames(sig) %in% c("TLI", "CFI")
	if(any(reverse)) {
		sig[,reverse] <- !sig[,reverse]
	}
	plotPowerSig(sig, x = x, xval=xval, mainName = usedFit, useContour = useContour)
	options(warn = warnT)
}

# plotScatter: Plot the overlapping scatterplots showing the distribution of fit indices given the values of varying parameters

plotScatter <- function(altObject, nullObject=NULL, cutoff=NULL, usedFit = NULL, x, alpha=0.05, df=5) {
	if (is.null(usedFit)) 
        usedFit <- getKeywords()$usedFit
	if (!is.null(cutoff)) {
		usedFit <- intersect(usedFit, names(cutoff))
		cutoff <- cutoff[usedFit]
	}
	if(is.null(cutoff) && !is.null(nullObject)) {
		nullFit <- as.data.frame(nullObject[,usedFit])
		colnames(nullFit) <- usedFit
		temp <- rep(NA, length(usedFit))
		cutoff <- getCutoff(object=nullFit, alpha = alpha, revDirec = FALSE, usedFit = usedFit, predictor = x, df = df, predictorVal="all")
		if(!is.matrix(cutoff)) {
			cutoff <- as.matrix(cutoff)
		}
	}
	if (length(usedFit) == 2) {
        obj <- par(mfrow = c(1, 2))
    } else if (length(usedFit) == 3) {
        obj <- par(mfrow = c(1, 3))
    } else if (length(usedFit) > 3) {
        obj <- par(mfrow = c(2, ceiling(length(usedFit)/2)))
    } else if (length(usedFit) == 1) {
        # Intentionally leaving as blank
    } else {
        stop("Some errors occur")
    }
	for (i in 1:length(usedFit)) {
		temp <- NULL
		if(!is.null(cutoff)) {
			if(is.matrix(cutoff)) {
				temp <- cutoff[,i]
			} else {
				temp <- cutoff[i]
			}
		}
		nullVec <- NULL
		if(!is.null(nullObject)) nullVec <- nullObject[,usedFit[i]]
        plotIndividualScatter(altObject[,usedFit[i]], nullVec=nullVec, cutoff=temp, x, main = usedFit[i])
    }
	if (length(usedFit) > 1) 
        par(obj)
}

# plotIndividualScatter: Plot each overlapping scatterplot showing the distribution of fit indices given the values of varying parameters

plotIndividualScatter <- function(altVec, nullVec=NULL, cutoff=NULL, x, main = NULL) {
	maxAll <- max(c(altVec, nullVec), na.rm=TRUE)
	minAll <- min(c(altVec, nullVec), na.rm=TRUE)
	plot(c(min(x),max(x)), c(minAll, maxAll), type="n", main=main, xlab=colnames(x), ylab="Value")
	points(x, altVec, col="skyblue")
	if(!is.null(nullVec)) points(x, nullVec, col="black")
	if(is.null(cutoff)) {
		# Intetionally leave as blank
	} else if(length(cutoff) == 1) {
		abline(h=cutoff, col="red", lwd=2)
	} else {
		lines(x, cutoff, col="red", lwd=2)
	}
}
