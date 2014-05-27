# A collection of summary*, get*, and set* functions for the result object


# summaryParam: This function will summarize the obtained parameter estimates
# and standard error.

summaryParam <- function(object, alpha = 0.05, detail = FALSE, improper = FALSE, digits = NULL, matchParam = FALSE) {
    object <- clean(object, improper = improper)
    coef <- colMeans(object@coef, na.rm = TRUE)
    real.se <- sapply(object@coef, sd, na.rm = TRUE)
    estimated.se <- colMeans(object@se, na.rm = TRUE)
    estimated.se[estimated.se == 0] <- NA
    z <- object@coef/object@se
    crit.value <- qnorm(1 - alpha/2)
    sig <- abs(z) > crit.value
    pow <- apply(sig, 2, mean, na.rm = TRUE)
	result <- cbind(coef, real.se, estimated.se, pow)
	colnames(result) <- c("Estimate Average", "Estimate SD", "Average SE", "Power (Not equal 0)")
	
	if (length(object@stdCoef) != 0) {
		stdCoef <- colMeans(object@stdCoef, na.rm = TRUE)
		stdRealSE <- sapply(object@stdCoef, sd, na.rm = TRUE)
		leftover <- setdiff(rownames(result), names(stdCoef))
		if(length(leftover) > 0) {
			temp <- rep(NA, length(leftover))
			names(temp) <- leftover
			stdCoef <- c(stdCoef, temp)
			stdRealSE <- c(stdRealSE, temp)
		}
		resultStd <- cbind(stdCoef, stdRealSE)
        colnames(resultStd) <- c("Std Est", "Std Est SD")
        result <- data.frame(result, resultStd[rownames(result),])
	}

    if (!is.null(object@paramValue)) {
        targetVar <- match(colnames(object@coef), colnames(object@paramValue))
        targetVar <- targetVar[!is.na(targetVar)]
        paramValue <- object@paramValue[, targetVar]
		if(matchParam) result <- result[colnames(paramValue),]
        if ((nrow(result) == ncol(paramValue)) && all(rownames(result) == 
            colnames(paramValue))) {
            nRep <- object@nRep
            nParam <- nrow(result)
            if (nrow(paramValue) == 1) 
                paramValue <- matrix(unlist(rep(paramValue, nRep)), nRep, nParam, 
                  byrow = T)
			biasParam <- object@coef[,rownames(result)] - paramValue
			lowerBound <- object@cilower
			upperBound <- object@ciupper
			selectci <- colnames(lowerBound) %in% rownames(result)
			lowerBound <- lowerBound[,selectci]
			upperBound <- upperBound[,selectci]
			noci <- setdiff(rownames(result), colnames(lowerBound))
			if(length(noci) > 0) {
				if(length(selectci) > 0) warning("Some CIs are Wald CI and others are calculated inside the simulation.")
				selectCoef <- object@coef[,noci]
				selectSE <- object@se[,noci]
				
				crit <- qnorm(1 - alpha/2)
				
				lowerBound <- cbind(lowerBound, selectCoef - crit * selectSE)
				upperBound <- cbind(upperBound, selectCoef + crit * selectSE)
            }
			cover <- (paramValue > lowerBound) & (paramValue < upperBound)
            average.param <- apply(paramValue, 2, mean, na.rm = TRUE)
            sd.param <- apply(paramValue, 2, sd, na.rm = TRUE)
            average.bias <- apply(biasParam, 2, mean, na.rm = TRUE)
            perc.cover <- apply(cover, 2, mean, na.rm = TRUE)
            sd.bias <- apply(biasParam, 2, sd, na.rm = TRUE)
            perc.cover[estimated.se[rownames(result)] == 0] <- NA
            result2 <- cbind(average.param, sd.param, average.bias, sd.bias, perc.cover)
            colnames(result2) <- c("Average Param", "SD Param", "Average Bias", "SD Bias", 
                "Coverage")
            if (nrow(object@paramValue) == 1) 
                result2 <- result2[, c(1, 3, 5)]
            result <- data.frame(result, result2)
            if (detail) {
                relative.bias <- biasParam/paramValue
                relBias <- apply(relative.bias, 2, mean, na.rm = TRUE)
                relBias[is.nan(relBias)] <- NA
                std.bias <- NULL
                relative.bias.se <- NULL
                if (nrow(object@paramValue) == 1) {
                  std.bias <- average.bias/real.se
                  relative.bias.se <- (estimated.se - real.se)/real.se
                } else {
                  std.bias <- average.bias/sd.bias
                  relative.bias.se <- (estimated.se - sd.bias)/sd.bias
                }
				width <- upperBound - lowerBound
				average.width <- apply(width, 2, mean, na.rm = TRUE)
				sd.width <- apply(width, 2, sd, na.rm = TRUE)				
				belowLowerBound <- paramValue < lowerBound
				aboveUpperBound <- paramValue > upperBound
				perc.lower <- apply(belowLowerBound, 2, mean, na.rm = TRUE)
				perc.upper <- apply(aboveUpperBound, 2, mean, na.rm = TRUE)
				result3 <- cbind(relBias, std.bias, relative.bias.se, perc.lower, perc.upper, average.width, sd.width)
                colnames(result3) <- c("Rel Bias", "Std Bias", "Rel SE Bias", "Not Cover Below", "Not Cover Above", "Average CI Width", "SD CI Width")
                result <- data.frame(result, result3)
            }
        }
    }

    if (nrow(object@FMI1) > 1 & ncol(object@FMI1) >= 1) {
        FMI1 <- object@FMI1
        average.FMI1 <- apply(FMI1, 2, mean, na.rm = TRUE)
        sd.FMI1 <- apply(FMI1, 2, sd, na.rm = TRUE)
        resultFMI <- cbind(average.FMI1, sd.FMI1)
        colnames(resultFMI) <- c("Average FMI1", "SD FMI1")
		targetParam <- matchLavaanName(rownames(result), rownames(resultFMI))
		targetParam <- targetParam[!is.na(targetParam)]
        result <- data.frame(result, resultFMI[targetParam,])
    }
	
    if (nrow(object@FMI2) > 1 & ncol(object@FMI2) >= 1) {
        FMI2 <- object@FMI2
        average.FMI2 <- apply(FMI2, 2, mean, na.rm = TRUE)
        sd.FMI2 <- apply(FMI2, 2, sd, na.rm = TRUE)
        
        resultFMI <- cbind(average.FMI2, sd.FMI2)
        colnames(resultFMI) <- c("Average FMI2", "SD FMI2")
		targetParam <- matchLavaanName(rownames(result), rownames(resultFMI))
		targetParam <- targetParam[!is.na(targetParam)]
        result <- data.frame(result, resultFMI[targetParam,])
    }	
	
    if (length(unique(object@n)) > 1) {
        corCoefN <- cor(cbind(object@coef, object@n), use = "pairwise.complete.obs")[colnames(object@coef), 
            "object@n"]
        corSeN <- cor(cbind(object@se, object@n), use = "pairwise.complete.obs")[colnames(object@se), 
            "object@n"]
        result <- data.frame(result, r_coef.n = corCoefN, r_se.n = corSeN)
    }
    if (length(unique(object@pmMCAR)) > 1) {
        corCoefMCAR <- cor(cbind(object@coef, object@pmMCAR), use = "pairwise.complete.obs")[colnames(object@coef), 
            "object@pmMCAR"]
        corSeMCAR <- cor(cbind(object@se, object@pmMCAR), use = "pairwise.complete.obs")[colnames(object@se), 
            "object@pmMCAR"]
        result <- data.frame(result, r_coef.pmMCAR = corCoefMCAR, r_se.pmMCAR = corSeMCAR)
    }
    if (length(unique(object@pmMAR)) > 1) {
        corCoefMAR <- cor(cbind(object@coef, object@pmMAR), use = "pairwise.complete.obs")[colnames(object@coef), 
            "object@pmMAR"]
        corSeMAR <- cor(cbind(object@se, object@pmMAR), use = "pairwise.complete.obs")[colnames(object@se), 
            "object@pmMAR"]
        result <- data.frame(result, r_coef.pmMAR = corCoefMAR, r_se.pmMAR = corSeMAR)
    }
	if(!is.null(digits)) {
		result <- round(result, digits)
	}
    return(as.data.frame(result))
} 

matchLavaanName <- function(name1, name2) {
	result1 <- match(name1, name2)
	result2 <- match(name1, switchLavaanName(name2))
	result1[is.na(result1)] <- result2[is.na(result1)]
	result1
}

switchLavaanName <- function(name) {
	nameX <- strsplit(name, "\\.")
	nameGroup <- sapply(nameX, "[", 1)
	nameParam <- sapply(nameX, "[", 2)
	target <- grep("~~", nameParam)
	varTarget <- strsplit(nameParam[target], "~~")
	nameParam[target] <- paste0(sapply(varTarget, "[", 2), "~~", sapply(varTarget, "[", 1))
	nameX <- paste0(nameGroup, ".", nameParam)
	nameX
}

# summaryMisspec: This function will summarize the obtained fit indices and
# generate a data frame.

summaryMisspec <- function(object, improper = FALSE) {
    object <- clean(object, improper = improper)
    if (all(dim(object@misspecValue) == 0)) {
        stop("This object does not have any model misspecification.")
    } else {
        misspecAverage <- colMeans(object@misspecValue, na.rm = TRUE)
        misspecSE <- sapply(object@misspecValue, sd, na.rm = TRUE)
        popAverage <- colMeans(object@popFit, na.rm = TRUE)
        popSE <- sapply(object@popFit, sd, na.rm = TRUE)
        mis <- data.frame(mean = misspecAverage, sd = misspecSE)
        pop <- data.frame(mean = popAverage, sd = popSE)
        return(rbind(pop, mis))
    }
} 

# summaryPopulation: Summarize population values behind data generation model

summaryPopulation <- function(object, improper = FALSE) {
    object <- clean(object, improper = improper)
    paramValue <- object@paramValue
    nRep <- nrow(paramValue)
    nParam <- ncol(paramValue)
    result <- NULL
    if (nrow(paramValue) == 1) {
        result <- matrix(paramValue, nrow = 1)
        rownames(result) <- "Population Value"
		colnames(result) <- colnames(paramValue)
    } else {
        average.param <- apply(paramValue, 2, mean, na.rm = TRUE)
        sd.param <- apply(paramValue, 2, sd, na.rm = TRUE)
        result <- rbind(average.param, sd.param)
        rownames(result) <- c("Average", "SD")
    }
    return(result)
} 

# summaryFit: This function will summarize the obtained fit indices and
# generate a data frame.

summaryFit <- function(object, alpha = NULL, improper = FALSE, usedFit = NULL) {
    cleanObj <- clean(object, improper = improper)
	usedFit <- cleanUsedFit(usedFit, colnames(object@fit))
    condition <- c(length(unique(object@pmMCAR)) > 1, length(unique(object@pmMAR)) > 
        1, length(unique(object@n)) > 1)
    if (any(condition)) {
        if (is.null(alpha)) 
            alpha <- 0.05
        values <- list()
        ifelse(condition[3], values[[3]] <- round(seq(min(object@n), max(object@n), 
            length.out = 5)), values[[3]] <- NA)
        ifelse(condition[1], values[[1]] <- seq(min(object@pmMCAR), max(object@pmMCAR), 
            length.out = 5), values[[1]] <- NA)
        ifelse(condition[2], values[[2]] <- seq(min(object@pmMAR), max(object@pmMAR), 
            length.out = 5), values[[2]] <- NA)
        m <- do.call(expand.grid, values)
        FUN <- function(vec, obj, alpha, usedFit) as.numeric(getCutoff(obj, alpha, revDirec = FALSE, 
            usedFit = usedFit, nVal = vec[3], pmMCARval = vec[1], pmMARval = vec[2]))
        cutoffs <- sapply(as.data.frame(t(m)), FUN, obj = object, alpha = alpha, 
            usedFit = usedFit)
        mSelect <- as.matrix(m[, condition])
        colnames(mSelect) <- c("%MCAR", "%MAR", "N")[condition]
		rownames(cutoffs) <- usedFit
		colnames(cutoffs) <- NULL
        result <- data.frame(mSelect, t(cutoffs))
        rownames(result) <- NULL
    } else {
        if (is.null(alpha)) 
            alpha <- c(0.1, 0.05, 0.01, 0.001)
        cutoffs <- sapply(alpha, getCutoff, object = cleanObj, usedFit = usedFit)
        cutoffs <- matrix(as.numeric(cutoffs), length(usedFit), length(alpha))
        if (ncol(as.matrix(cutoffs)) == 1) {
            cutoffs <- t(cutoffs)
            # rownames(cutoffs) <- usedFit
        }
        
        fit <- as.data.frame(cleanObj@fit[, usedFit])
        meanfit <- apply(fit, 2, mean, na.rm = TRUE)
        sdfit <- apply(fit, 2, sd, na.rm = TRUE)
		if(length(alpha) == 1) cutoffs <- t(cutoffs)
        result <- cbind(cutoffs, meanfit, sdfit)
        colnames(result) <- c(alpha, "Mean", "SD")
        rownames(result) <- usedFit
        names(dimnames(result)) <- c("Fit Indices", "Alpha")
        # print(as.data.frame(cutoffs))
    }
    
    return(result)
} 

# summaryMisspec: This function will summarize the obtained fit indices and
# generate a data frame.

summaryConverge <- function(object, improper = FALSE) {
    result <- list()
    converged <- object@converged == 0
    numnonconverged <- sum(!converged)
    if (numnonconverged == 0) 
        stop("You are good! All replications were converged!")
    result <- c(result, list(Converged = c(num.converged = sum(converged), num.nonconverged = numnonconverged)))
	reasons <- c("Nonconvergent" = sum(object@converged %in% 1:2), "Improper SE" = sum(object@converged == 3), "Improper Variance" = sum(object@converged == 4), "Improper Correlation" = sum(object@converged == 5), "Optimal estimates were not guaranteed" = sum(object@converged == 6))
	reasons <- as.matrix(reasons)
	colnames(reasons) <- "count"
	result <- c(result, list("Nonconvergent Reasons" = reasons))
    n <- object@n
    pmMCAR <- object@pmMCAR
    pmMAR <- object@pmMAR
    paramValue <- object@paramValue
    misspecValue <- object@misspecValue
    popFit <- object@popFit
	nonconverged <- !converged
	improprep <- rep(FALSE, length(converged))
	if(improper) {
		nonconverged <- object@converged %in% 1:2
		improprep <- object@converged %in% 3:6
	}
    if (length(unique(n)) > 1) {
        temp1 <- n[converged]
        temp2 <- n[nonconverged]
		temp3 <- n[improprep]
		resultTemp <- c(mean.convergence = mean(temp1), sd.convergence = sd(temp1))
		resultDiff <- NULL
		if(length(temp2) > 0) {
			resultTemp <- c(resultTemp, mean.nonconvergence = mean(temp2), sd.nonconvergence = sd(temp2))
			resultDiff <- c(resultDiff, diff.non.mean = mean(temp2) - mean(temp1), diff.non.sd = sd(temp2) - sd(temp1))
		}
		if(length(temp3) > 0) {
			resultTemp <- c(resultTemp, mean.improper = mean(temp3), sd.improper = sd(temp3))
			resultDiff <- c(resultDiff, diff.improper.mean = mean(temp3) - mean(temp1), diff.improper.sd = sd(temp3) - sd(temp1))
		}
        result <- c(result, list(n = c(resultTemp, resultDiff)))
    }
    if (length(unique(pmMCAR)) > 1) {
        temp1 <- pmMCAR[converged]
        temp2 <- pmMCAR[nonconverged]
		temp3 <- pmMCAR[improprep]
		resultTemp <- c(mean.convergence = mean(temp1), sd.convergence = sd(temp1))
		resultDiff <- NULL
		if(length(temp2) > 0) {
			resultTemp <- c(resultTemp, mean.nonconvergence = mean(temp2), sd.nonconvergence = sd(temp2))
			resultDiff <- c(resultDiff, diff.non.mean = mean(temp2) - mean(temp1), diff.non.sd = sd(temp2) - sd(temp1))
		}
		if(length(temp3) > 0) {
			resultTemp <- c(resultTemp, mean.improper = mean(temp3), sd.improper = sd(temp3))
			resultDiff <- c(resultDiff, diff.improper.mean = mean(temp3) - mean(temp1), diff.improper.sd = sd(temp3) - sd(temp1))
		}
        result <- c(result, list(pmMCAR = c(resultTemp, resultDiff)))
    }
    if (length(unique(pmMAR)) > 1) {
        temp1 <- pmMAR[converged]
        temp2 <- pmMAR[nonconverged]
		temp3 <- pmMAR[improprep]
		resultTemp <- c(mean.convergence = mean(temp1), sd.convergence = sd(temp1))
		resultDiff <- NULL
		if(length(temp2) > 0) {
			resultTemp <- c(resultTemp, mean.nonconvergence = mean(temp2), sd.nonconvergence = sd(temp2))
			resultDiff <- c(resultDiff, diff.non.mean = mean(temp2) - mean(temp1), diff.non.sd = sd(temp2) - sd(temp1))
		}
		if(length(temp3) > 0) {
			resultTemp <- c(resultTemp, mean.improper = mean(temp3), sd.improper = sd(temp3))
			resultDiff <- c(resultDiff, diff.improper.mean = mean(temp3) - mean(temp1), diff.improper.sd = sd(temp3) - sd(temp1))
		}
        result <- c(result, list(pmMAR = c(resultTemp, resultDiff)))
    }
	if(nrow(paramValue) > 1) {
		temp1 <- paramValue[converged, , drop = FALSE]
		temp2 <- paramValue[nonconverged, , drop = FALSE]
		temp3 <- paramValue[improprep, , drop = FALSE]
		resultTemp <- cbind(mean.convergence = apply(temp1, 2, mean), sd.convergence = apply(temp1, 
			2, sd))
		resultDiff <- NULL
		if(nrow(temp2) > 0) {
			resultTemp <- cbind(resultTemp, mean.nonconvergence = apply(temp2, 2, mean), sd.nonconvergence = apply(temp2, 2, sd))
			resultDiff <- cbind(resultDiff, diff.non.mean = apply(temp2, 2, mean) - apply(temp1, 2, mean), 
			diff.non.sd = apply(temp2, 2, sd) - apply(temp1, 2, sd))
		}
		if(nrow(temp3) > 0) {
			resultTemp <- cbind(resultTemp, mean.improper = apply(temp3, 2, mean), sd.improper = apply(temp3, 2, sd))
			resultDiff <- cbind(resultDiff, diff.improper.mean = apply(temp3, 2, mean) - apply(temp1, 2, mean), 
			diff.improper.sd = apply(temp3, 2, sd) - apply(temp1, 2, sd))
		}
		result <- c(result, list(paramValue = cbind(resultTemp, resultDiff)))
	}
    if (!all(dim(misspecValue) == 0) && nrow(misspecValue) > 1) {
		temp1 <- misspecValue[converged, ]
		temp2 <- misspecValue[nonconverged, ]
		temp3 <- misspecValue[improprep, ]
		resultTemp <- cbind(mean.convergence = apply(temp1, 2, mean), sd.convergence = apply(temp1, 
			2, sd))
		resultDiff <- NULL
		if(nrow(temp2) > 0) {
			resultTemp <- cbind(resultTemp, mean.nonconvergence = apply(temp2, 2, mean), sd.nonconvergence = apply(temp2, 2, sd))
			resultDiff <- cbind(resultDiff, diff.non.mean = apply(temp2, 2, mean) - apply(temp1, 2, mean), 
			diff.non.sd = apply(temp2, 2, sd) - apply(temp1, 2, sd))
		}
		if(nrow(temp3) > 0) {
			resultTemp <- cbind(resultTemp, mean.improper = apply(temp3, 2, mean), sd.improper = apply(temp3, 2, sd))
			resultDiff <- cbind(resultDiff, diff.improper.mean = apply(temp3, 2, mean) - apply(temp1, 2, mean), 
			diff.improper.sd = apply(temp3, 2, sd) - apply(temp1, 2, sd))
		}
		result <- c(result, list(misspecValue = cbind(resultTemp, resultDiff)))
    }
    if (!all(dim(popFit) == 0) && nrow(popFit) > 1) {
		temp1 <- popFit[converged, ]
		temp2 <- popFit[nonconverged, ]
		temp3 <- popFit[improprep, ]
		resultTemp <- cbind(mean.convergence = apply(temp1, 2, mean), sd.convergence = apply(temp1, 
			2, sd))
		resultDiff <- NULL
		if(nrow(temp2) > 0) {
			resultTemp <- cbind(resultTemp, mean.nonconvergence = apply(temp2, 2, mean), sd.nonconvergence = apply(temp2, 2, sd))
			resultDiff <- cbind(resultDiff, diff.non.mean = apply(temp2, 2, mean) - apply(temp1, 2, mean), 
			diff.non.sd = apply(temp2, 2, sd) - apply(temp1, 2, sd))
		}
		if(nrow(temp3) > 0) {
			resultTemp <- cbind(resultTemp, mean.improper = apply(temp3, 2, mean), sd.improper = apply(temp3, 2, sd))
			resultDiff <- cbind(resultDiff, diff.improper.mean = apply(temp3, 2, mean) - apply(temp1, 2, mean), 
			diff.improper.sd = apply(temp3, 2, sd) - apply(temp1, 2, sd))
		}
		result <- c(result, list(popFit = cbind(resultTemp, resultDiff)))
    }
    return(result)
} 

# setPopulation: Set population parameter values

setPopulation <- function(target, population) {
    psl <- generate(population, n = 20, params = TRUE)$psl
    paramSet <- lapply(psl, "[[", 1)
	generatedgen <- population@dgen
	popParam <- NULL
	if (!is.list(generatedgen[[1]])) {
		generatedgen <- list(generatedgen)
	}
	for (i in seq_along(paramSet)) {
		popParam <- c(popParam, parsePopulation(generatedgen[[i]], paramSet[[i]], group = i))
	}
	extraParamIndex <- population@pt$op %in% c(">", "<", "==", ":=")
	extraParamName <- NULL
	if(any(extraParamIndex)) {
		extraparam <- collapseExtraParam(paramSet, population@dgen, fill=TRUE, con=population@con)
		extraParamName <- renameExtraParam(population@con$lhs, population@con$op, population@con$rhs)
		popParam[extraParamIndex] <- extraparam
	}
	index <- ((population@pt$free != 0)& !(duplicated(population@pt$free))) | extraParamIndex
	popParam <- popParam[index]
	names(popParam) <- c(names(coef(lavaan(population@pt, sample.nobs=rep(200, max(population@pt$group))))), extraParamName)
    return(popParam)
} 

# getPopulation: Description: Extract the population value from an object

getPopulation <- function(object, improper = FALSE, nonconverged = FALSE) {
    inspect(object, improper = improper, nonconverged = nonconverged)
} 

# getExtraOutput: Extract the extra output that users set in the 'outfun' argument

getExtraOutput <- function(object, improper = FALSE, nonconverged = FALSE) {
	targetRep <- 0
	if(improper) targetRep <- c(targetRep, 3:6)
	if(nonconverged) targetRep <- c(targetRep, 1:2)
    if (length(object@extraOut) == 0) {
        stop("This simulation result does not contain any extra results")
    } else {
        return(object@extraOut[object@converged %in% targetRep])
    }
} 

setMethod("coef", "SimResult",
function(object, improper = FALSE, nonconverged = FALSE) {
	inspect(object, "coef", improper = improper, nonconverged = nonconverged)
})

setMethod("inspect", "SimResult",
function(object, what="coef", improper = FALSE, nonconverged = FALSE) {

	targetRep <- 0
	if(improper) targetRep <- c(targetRep, 3:6)
	if(nonconverged) targetRep <- c(targetRep, 1:2)
	targetRep <- object@converged %in% targetRep
	
    if(length(what) > 1) {
        stop("`what' arguments contains multiple arguments; only one is allowed")
    }

    # be case insensitive
    what <- tolower(what)

    if(what == "type" ||
              what == "model.type" ||
              what == "modeltype") {
        return(object@modelType)
    } else if(what == "nrep" ||
              what == "numrep") {
        return(object@nRep)
    } else if(what == "param" ||
              what == "paramvalue" ||
              what == "parameter" ||
              what == "parameter.value" ||
              what == "paramvalues" ||
              what == "parameter.values" ||
              what == "parameters") {
		target <- object@paramValue
		if(nrow(target) > 1) target <- target[targetRep, , drop=FALSE]
        return(target)
    } else if(what == "se" ||
              what == "std.err" ||
              what == "standard.errors") {
        return(object@se[targetRep, , drop=FALSE])
    } else if(what == "misspec" ||
              what == "misspecification" ||
              what == "misspecified" ||
              what == "misspecified.param") {
		target <- object@misspecValue
		if(nrow(target) > 1) target <- target[targetRep, , drop=FALSE]
        return(target)
    } else if(what == "coef" ||
              what == "coefficients" ||
              what == "parameter.estimates" ||
              what == "estimates" ||
              what == "x" ||
              what == "est") {
        return(object@coef[targetRep, , drop=FALSE])
    } else if(what == "popfit" ||
              what == "popmisfit" ||
              what == "popmis" ||
              what == "misfit") {
		target <- object@popFit
		if(nrow(target) > 1) target <- target[targetRep, , drop=FALSE]
        return(target)
    } else if(what == "fmi" ||
              what == "fmi1") {
		target <- object@FMI1
		if(nrow(target) > 1) target <- target[targetRep, , drop=FALSE]
        return(target)
    } else if(what == "fmi2") {
        target <- object@FMI2
		if(nrow(target) > 1) target <- target[targetRep, , drop=FALSE]
        return(target)
    } else if(what == "fit" ||
              what == "fitmeasures" ||
              what == "fit.measures" ||
              what == "fit.indices") {
        return(object@fit[targetRep, , drop=FALSE])
    } else if(what == "std" ||
              what == "std.coef" ||
              what == "standardized" ||
              what == "standardizedsolution" ||
              what == "standardized.solution") {
        return(object@stdCoef[targetRep, , drop=FALSE])
    } else if(what == "cilower" ||
              what == "ci.lower" ||
              what == "lowerci" ||
              what == "lower.ci") {
         return(object@cilower[targetRep, , drop=FALSE])
    } else if(what == "ciupper" ||
              what == "ci.upper" ||
              what == "upperci" ||
              what == "upper.ci") {
         return(object@ciupper[targetRep, , drop=FALSE])
    } else if(what == "ciwidth" ||
              what == "ci.width" ||
              what == "widthci" ||
              what == "width.ci" ||
			  what == "width") {
		 lower <- as.matrix(object@cilower)
		 upper <- as.matrix(object@ciupper)
		 if(nrow(lower) <= 1) stop("There are no lower and upper bounds of confidence intervals saved in this simulation.")
         return(upper[targetRep, , drop=FALSE] - lower[targetRep, , drop=FALSE])
    } else if(what == "seed") {
        return(summarySeed(object))
    } else if(what == "n" || 
              what == "samplesize" ||
              what == "groupsize" ||
              what == "groupn" ||
              what == "ngroup" ||
			  what == "sample.size") {
        return(object@nobs[targetRep, , drop=FALSE])
    } else if(what == "ntotal" || 
              what == "totaln" ||
              what == "totalsamplesize" ||
			  what == "total.sample.size") {
        return(object@n[targetRep])
    } else if(what == "mcar" || 
              what == "pmmcar" ||
			  what == "pmcar") {
		target <- object@pmMCAR
		if(length(target) > 1) target <- target[targetRep]
        return(target)
    } else if(what == "mar" || 
              what == "pmmar" ||
			  what == "pmar") {
		target <- object@pmMAR
		if(length(target) > 1) target <- target[targetRep]
        return(target)
    } else if(what == "extra" || 
              what == "extraout" ||
			  what == "misc") {
        return(getExtraOutput(object, improper = improper, nonconverged = nonconverged))
    } else if(what == "time"  ||
              what == "timing") {
        return(summaryTime(object))
    } else if(what == "converged") {
		lab <- c("converged", "nonconverged", "nonconvergedMI", "improperSE", "improperVariance", "improperCorrelation", "nonOptimal")
		lab <- lab[sort(unique(object@converged)) + 1]
		out <- factor(object@converged, labels = lab)
        return(out)
    } else {
        stop("unknown `what' argument in inspect function: `", what, "'")
    }

})

# summaryPopulation: Summarize population values behind data generation model

summaryTime <- function(object, units = "seconds") {
	timing <- object@timing
	timing1 <- timing[-which(names(timing) %in% c("StartTime", "EndTime"))]
	units <- tolower(units)
	if(units %in% c("secs", "seconds", "second", "sec", "s")) {
		# Do nothing
	} else if (units %in% c("min", "mins", "minute", "minutes", "m")) {
		timing1 <- lapply(timing1, "/", 60)
	} else if (units %in% c("hours", "hour", "h")) {
		timing1 <- lapply(timing1, "/", 3600)	
	} else if (units %in% c("days", "day", "d")) {
		timing1 <- lapply(timing1, "/", 86400)
	} else {
		stop("unknown `units' argument in summaryTime function: `", units, "'")
	}
	
	#format(.POSIXct(dt,tz="GMT"), "%H:%M:%S")
	cat("============ Wall Time ============\n")
	t0.txt <- sprintf("  %-72s", "1. Error Checking and setting up data-generation and analysis template:")
    t1.txt <- sprintf("  %10.3f", timing1$SimulationParams)
	cat(t0.txt, t1.txt, "\n", sep="")
	t0.txt <- sprintf("  %-72s", "2. Set combinations of n, pmMCAR, and pmMAR:")
    t1.txt <- sprintf("  %10.3f", timing1$RandomSimParams)
	cat(t0.txt, t1.txt, "\n", sep="")
	t0.txt <- sprintf("  %-72s", "3. Setting up simulation conditions for each replication:")
    t1.txt <- sprintf("  %10.3f", timing1$SimConditions)
	cat(t0.txt, t1.txt, "\n", sep="")
	t0.txt <- sprintf("  %-72s", "4. Total time elapsed running all replications:")
    t1.txt <- sprintf("  %10.3f", timing1$RunReplications)
	cat(t0.txt, t1.txt, "\n", sep="")
	t0.txt <- sprintf("  %-72s", "5. Combining outputs from different replications:")
    t1.txt <- sprintf("  %10.3f", timing1$CombineResults)
	cat(t0.txt, t1.txt, "\n", "\n", sep="")
	
	cat("============ Average Time in Each Replication ============\n")
	t0.txt <- sprintf("  %-46s", "1. Data Generation:")
    t1.txt <- sprintf("  %10.3f", timing1$InReps[1])
	cat(t0.txt, t1.txt, "\n", sep="")
	t0.txt <- sprintf("  %-46s", "2. Impose Missing Values:")
    t1.txt <- sprintf("  %10.3f", timing1$InReps[2])
	cat(t0.txt, t1.txt, "\n", sep="")
	t0.txt <- sprintf("  %-46s", "3. User-defined Data-Transformation Function:")
    t1.txt <- sprintf("  %10.3f", timing1$InReps[3])
	cat(t0.txt, t1.txt, "\n", sep="")
	t0.txt <- sprintf("  %-46s", "4. Main Data Analysis:")
    t1.txt <- sprintf("  %10.3f", timing1$InReps[4])
	cat(t0.txt, t1.txt, "\n", sep="")
	t0.txt <- sprintf("  %-46s", "5. Extracting Outputs:")
    t1.txt <- sprintf("  %10.3f", timing1$InReps[5])
	cat(t0.txt, t1.txt, "\n", "\n", sep="")
	
	cat("============ Summary ============\n")
	t0.txt <- sprintf("  %-28s", "Start Time:")
    t1.txt <- format(timing$StartTime, "  %Y-%m-%d %H:%M:%S")
	cat(t0.txt, t1.txt, "\n", sep="")
	t0.txt <- sprintf("  %-28s", "End Time:")
    t1.txt <- format(timing$EndTime, "  %Y-%m-%d %H:%M:%S")
	cat(t0.txt, t1.txt, "\n", sep="")
	totaltime <- Reduce("+", timing1[-which("InReps" == names(timing1))])
	t0.txt <- sprintf("  %-28s", "Wall (Actual) Time:")
    t1.txt <- sprintf("  %10.3f", totaltime)
	cat(t0.txt, t1.txt, "\n", sep="")
	inreptime <- max(sum(timing1$InReps * object@nRep), timing1$RunReplications)
	systemtime <- totaltime - timing1$RunReplications + inreptime
	t0.txt <- sprintf("  %-28s", "System (Processors) Time:")
    t1.txt <- sprintf("  %10.3f", systemtime)
	cat(t0.txt, t1.txt, "\n", sep="")
	t0.txt <- sprintf("  %-28s", "Units:")
	cat(t0.txt, "  ", units, "\n", sep="")
} 

summarySeed <- function(object) {
	seed <- object@seed
	list("Seed number" = object@seed[1], "L'Ecuyer seed of the last replication" = object@seed[-1])
}