# pValue: Find a p-value from an object

setMethod("pValue", signature(target = "numeric", dist = "vector"), definition = function(target, dist, revDirec = FALSE, x = NULL, xval = NULL, condCutoff=TRUE, df = 0) {
	if(length(target) == 1) {
		if(is.null(x)) {
			if (revDirec) {
				return(mean(target >= dist, na.rm = TRUE)) # Appropriate for pValue of CFI
			} else {
				return(mean(target <= dist, na.rm = TRUE)) # Appropriate for pValue of RMSEA
			}
		} else {
			# Assume that the target value is appropriate for the specific 'xval' only. Thus, the conditional quantile method is used.
			if(condCutoff) {
				return(pValueCondCutoff(target, dist, revDirec = revDirec, x = x, xval = xval, df = df))
			} else {
				return(pValueVariedCutoff(rep(target, length(dist)), dist, revDirec = revDirec, x = x, xval = xval))
			}
		}
	} else if (length(target) > 1) {
		return(pValueVariedCutoff(target, dist, revDirec = revDirec, x = x, xval = xval))
	} else {
		stop("Something is wrong!")
	}
})

setMethod("pValue", signature(target = "numeric", dist = "data.frame"), definition = function(target, dist, revDirec = NULL, x = NULL, xval = NULL, df = 0, asLogical = FALSE) {
    if (length(target) != ncol(dist)) 
        stop("The length of target and the number of columns of dist are not equal")
    numVar <- length(target)
    if (is.null(revDirec)) {
        revDirec <- rep(FALSE, numVar)
    } else {
        if (length(revDirec) != numVar) 
            stop("The length of revDirec and the number of columns of dist are not equal")
    }
    if (asLogical) {
        result <- NULL
        for (i in 1:numVar) {
            if (revDirec[i]) {
                result <- cbind(result, target[i] >= dist[, i])
            } else {
                result <- cbind(result, target[i] <= dist[, i])
            }
        }
        return(result)
    } else {
        result <- rep(NA, numVar)
        for (i in 1:numVar) {
            result[i] <- pValue(target[i], dist[, i], revDirec[i], x = x, xval = xval, df = df)
        }
        return(result)
    }
})

setMethod("pValue", signature(target = "SimModelOut", dist = "SimResult"), definition = function(target, dist, usedFit = NULL, nVal = NULL, pmMCARval = NULL, pmMARval = NULL, df = 0) {
    dist <- clean(dist)
    if (is.null(usedFit)) 
        usedFit <- getKeywords()$usedFit
    revDirec <- (usedFit %in% c("CFI", "TLI")) # CFA --> FALSE, RMSEA --> TRUE
	
	if (is.null(nVal) || is.na(nVal)) 
        nVal <- NULL
    if (is.null(pmMCARval) || is.na(pmMCARval)) 
        pmMCARval <- NULL
    if (is.null(pmMARval) || is.na(pmMARval)) 
        pmMARval <- NULL
    Data <- as.data.frame(dist@fit[, usedFit])
    condition <- c(length(dist@pmMCAR) > 1, length(dist@pmMAR) > 1, length(dist@n) > 1)
    condValue <- cbind(dist@pmMCAR, dist@pmMAR, dist@n)
    colnames(condValue) <- c("Percent MCAR", "Percent MAR", "N")
    condValue <- condValue[, condition]
    if (is.null(condValue) || length(condValue) == 0) 
        condValue <- NULL
    predictorVal <- rep(NA, 3)
    if (condition[3]) {
        ifelse(is.null(nVal), predictorVal[3] <- target@n, 
            predictorVal[3] <- nVal)
    }
    if (condition[1]) {
        ifelse(is.null(pmMCARval), predictorVal[1] <- mean(target@pMiss), 
            predictorVal[1] <- pmMCARval)
    }
    if (condition[2]) {
        ifelse(is.null(pmMARval), stop("Please specify the percent of missing at random, 'pmMARval', because the percent of missing at random in the result object is varying"), 
            predictorVal[2] <- pmMARval)
    }
    predictorVal <- predictorVal[condition]
    cutoff <- target@fit[usedFit]
	if(any(condition)) {
		result <- pValue(cutoff, Data, revDirec, x = condValue, xval = predictorVal, df = df, asLogical = FALSE)
		names(result) <- usedFit
		return(result)
	} else {
		logicalMat <- pValue(cutoff, Data, revDirec, asLogical = TRUE)
		result <- apply(logicalMat, 2, mean, na.rm = TRUE)
		names(result) <- usedFit
		andRule <- mean(apply(logicalMat, 1, all), na.rm = TRUE)
		orRule <- mean(apply(logicalMat, 1, any), na.rm = TRUE)
		return(c(result, andRule = andRule, orRule = orRule))
	}
}) 

# pValueCondCutoff: Find the p-value comparing between a cutoff and a distribution when a cutoff is conditional on a predictor value

pValueCondCutoff <- function(target, dist, revDirec = FALSE, x = NULL, xval = NULL, df = 0) {
	if (!is.matrix(x)) 
		x <- as.matrix(x)
	p <- ncol(x)
	name <- paste("x", 1:p, sep = "")
	colnames(x) <- name
	names(xval) <- name
	if (df == 0) {
		name2 <- name
	} else {
		library(splines)
		name2 <- paste("ns(", name, ",", df, ")", sep = "")
	}
	firstord <- paste(name2, collapse = " + ")
	FUN <- function(x, y) paste(x, " * ", y, sep = "")
	secondord <- outer(name2, name2, FUN)[lower.tri(diag(length(name2)))]
	secondord <- paste(secondord, collapse = " + ")
	if (secondord == "") {
		express <- paste("y ~ ", firstord, sep = "")
	} else {
		express <- paste("y ~ ", firstord, " + ", secondord, sep = "")
	}
	dat <- data.frame(y = dist, x)
	library(quantreg)
	percVal <- 1:49/50
	mod <- rq(express, data = dat, tau = percVal)
	xval <- data.frame(t(as.matrix(xval)))
	colnames(xval) <- name2
	perc <- predict(mod, xval, interval = "none")
	perc <- whichMonotonic(perc, percVal)
	result <- interpolate(perc, target)
	if(!revDirec) {
		result <- revText(result)
	}
	return(result)
}

# revText: Reverse the direction of p value such as "> .98" to "< .02"

revText <- function(val) {
	if(suppressWarnings(is.na(as.numeric(val)))) {
		ntext <- nchar(val)
		comp <- substr(val, 1, 1)
		val <- as.numeric(substr(val, 3, nchar(val)))
		if(comp == ">") {
			comp <- "<"
		} else if (comp == "<") {
			comp <- ">"
		} else {
			stop("Something is wrong")
		}
		result <- paste(comp, 1 - val)
	} else {
		result <- 1 - as.numeric(val)
	}
	return(result)
}

# whichMonotonic: Find the center of a vector that is monotonically increasing or decreasing

whichMonotonic <- function(vec, ord=NULL, anchor=NULL) {
	vec <- as.vector(vec)
	p <- length(vec)
	if(is.null(anchor)) anchor <- round((p/2) + 0.001) # Add a small decimal so that 0.5 will be rounded as 1 instead of 0
	testDirection <- vec[2:p] > vec[1:(p-1)]
	directionCenter <- vec[anchor + 1] > vec[anchor - 1]
	if(!directionCenter) testDirection <- !testDirection
	notMonotone <- which(!testDirection)
	minchange <- 1
	maxchange <- p
	if(length(notMonotone) > 0) {
		lhs <- notMonotone < anchor
		if(length(which(lhs)) > 0) minchange <- notMonotone[max(which(lhs))] + 1
		rhs <- notMonotone > anchor
		if(length(which(rhs)) > 0) maxchange <- notMonotone[min(which(rhs))]
	}
	result <- vec[minchange:maxchange]
	if(is.null(ord)) {
		names(result) <- NULL
	} else {
		names(result) <- ord[minchange:maxchange]
	}
	return(result)
}

# interpolate: Find a specific percentile value by a linear interpolation of a closed value

interpolate <- function(baselineVec, val, resultVec=NULL) {
	p <- length(baselineVec)
	if(is.null(resultVec)) resultVec <- names(baselineVec)
	lessThanVal <- which(baselineVec < val)
	if(length(lessThanVal) == 0) {
		return(paste("<", resultVec[1]))
	} else if (length(lessThanVal) == p) {
		return(paste(">", resultVec[p]))
	} else {
		floorIndex <- max(which(baselineVec < val))
		val1 <- baselineVec[floorIndex]
		val2 <- baselineVec[floorIndex + 1]
		perc1 <- as.numeric(resultVec[floorIndex])
		perc2 <- as.numeric(resultVec[floorIndex + 1])
		prop <- (val - val1) / (val2 - val1)
		targetPerc <- prop * (perc2 - perc1) + perc1
		names(targetPerc) <- NULL
		return(targetPerc)
	}
}

# pValueVariedCutoff: Find a value when the cutoffs are specified as a vector

pValueVariedCutoff <- function(cutoff, obtainedValue, revDirec = FALSE, x = NULL, xval = NULL) {
	# Change warning option to supress warnings
    warnT <- as.numeric(options("warn"))
    options(warn = -1)
	
	sig <- NULL
	if (revDirec) {
		sig <- cutoff >= obtainedValue # sig for CFI
	} else {
		sig <- cutoff <= obtainedValue # sig for RMSEA
	}
	if(is.null(x)) {
		result <- mean(sig, na.rm=TRUE)
	} else {
		x <- as.matrix(x)
		mod <- invisible(try(glm(sig ~ x, family = binomial(link = "logit")), silent = TRUE))
		result <- predProb(c(1, xval), mod)
	}
	## Return warnings setting to user's settings
    options(warn = warnT)
	
	return(result)
}

