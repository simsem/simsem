SimSemParentClass <- setRefClass("SimSemParentClass")

SimMatrix <- setRefClass("SimMatrix",
      fields = list( free = "matrix", popParam = "matrix", misspec = "matrix", symmetric = "logical"),
	  contain = "SimSemParentClass",
      methods = list(
     summary = function() {
		print("Random Full Matrix Object.")
		print("Free/Fixed Parameters:")
		print(free)
		print("Population Parameters/Starting Values:")
		print(popParam)
		print("Model misspecification")
		print(misspec)
     }, 
	 summaryShort = function() {
		Labels <- popParam
		if(!(all(dim(Labels) == 1) && is.nan(Labels))) {
			Labels[!is.free(free)] <- as.character(free[!is.free(free)])
			Labels[is.free(free)] <- paste(free[is.free(free)], Labels[is.free(free)], sep = ":")
		} else {
			Labels <- free
		}
		if (!(all(dim(misspec) == 1) && is.nan(misspec))) {
			Labels <- matrix(paste0(Labels, "+", misspec), nrow(Labels), ncol(Labels))
		}
		print(Labels)
	 },
	 show = function() {
		summaryShort()
	 }
     ))
# Waiting to put prior

SimVector <- setRefClass("SimVector",
    fields = list(free = "vector", popParam = "vector", misspec = "vector"),
	contain = "SimSemParentClass",
    methods = list(
     summary = function() {
		print("Random Vector Object.")
		print("Free/Fixed Parameters:")
		print(free)
		print("Population Parameters/Starting Values:")
		print(popParam)
		print("Model misspecification")
		print(misspec)
     }, 
	 summaryShort = function() {
		Labels <- popParam
		if(!(length(Labels) == 1 && is.nan(Labels))) {
			Labels[!is.free(free)] <- as.character(free[!is.free(free)])
			Labels[is.free(free)] <- paste(free[is.free(free)], Labels[is.free(free)], sep = ":")
		} else {
			Labels <- free
		}
		if (length(misspec) != 0 && !(length(misspec) == 1 && is.nan(misspec))) {
			Labels <- paste0(Labels, "+", misspec)
		}
		print(Labels)
	 },
	 show = function() {
		summaryShort()
	 }
     ))
# Waiting to put prior

SimSem <- setRefClass("SimSem",
      fields = list(pt = "list", dgen = "list", modelType = "character", groupLab = "character", con = "list"),
	  contain = "SimSemParentClass",
      methods = list(
     summary = function() {
		cat("Model Type\n")
		print(modelType)
		cat("========================Lavaan Analysis Model========================\n")
		print(as.data.frame(pt))
		cat("========================Data Generation Template========================\n")
		temp <- dgen
		if("PS" %in% names(temp)) temp <- list(temp)
		for(i in 1:length(temp)) {
			cat(paste0("-------- Group ", i, " --------\n"))
			dgengroup <- temp[[i]]
			printIfNotNull(dgengroup$LY, "\nLY: Loading of Indicator.Y on Factor.ETA")
			printIfNotNull(dgengroup$TE, "\nTE: Covariance of Measurement.Error.EPSILON")
			printIfNotNull(dgengroup$VTE, "\nVTE: Variance of Measurement.Error.EPSILON")
			printIfNotNull(dgengroup$RTE, "\nRTE: Correlation of Measurement.Error.EPSILON")
			printIfNotNull(dgengroup$VY, "\nVY: Variance of Indicator.Y")
			printIfNotNull(dgengroup$TY, "\nTY: Measurement Intercept of Indicator.Y")
			printIfNotNull(dgengroup$MY, "\nMY: mean of Indicator.Y")
			printIfNotNull(dgengroup$BE, "\nBE: Regression Coefficient among Factor.ETA")
			printIfNotNull(dgengroup$PS, "\nPS: Covariance of Regression.Residual.PSI")
			printIfNotNull(dgengroup$VPS, "\nVPS: Variance of Regression.Residual.PSI")
			printIfNotNull(dgengroup$RPS, "\nRPS: Correlation of Regression.Residual.PSI")
			printIfNotNull(dgengroup$VE, "\nVE: Variance of Factor.ETA")
			printIfNotNull(dgengroup$AL, "\nAL: Regression Intercept of Factor.ETA")
			printIfNotNull(dgengroup$ME, "\nME: mean of Factor.ETA")
			cat("--------------------------", "\n")
		}
		if(any(!(dim(con) == 0))) {
			cat("========================Model Constraints========================\n")
			print(as.data.frame(con))
		}
     }, 
	 summaryShort = function() {
		summary()
	 },
	 show = function() {
		summaryShort()
	 }
     ))

# Set a null class to make sure that the default setting is used in data distribution object
setClass("NullCopula", contains="copula")

SimDataDist <- setRefClass("SimDataDist",
    fields = list(p = "numeric", margins = "character", paramMargins = "list", keepScale = "logical", reverse = "vector", copula = "copula"),
	contain = "SimSemParentClass",
    methods = list(
     summary = function() {
		cat("DATA DISTRIBUTION OBJECT\n")
		cat(paste("The number of variables is", p, "\n"))
		cat(paste("Keep means and variances of the original scales:", paste(keepScale, 
			collapse = " / "), "\n"))
		cat("The list of distributions:\n")
		attr <- sapply(paramMargins, function(x) paste0(names(x), " = ", x, collapse = ", "))
		out <- paste0(margins, ": ", attr)
		for (i in 1:p) {
			cat(i, ". ", out[i], "\n", sep = "")
		}
		cat(paste("Reverse (mirror) distribution:", paste(reverse, collapse = " / "), 
			"\n"))
		if(!is(copula, "NullCopula")) {
			cat(paste("Multivariate Copula:\n"))
			show(copula)
		}
     }, 
	 summaryShort = function() {
		summary()
	 },
	 show = function() {
		summaryShort()
	 }
     ))
#prototype(keepScale = TRUE, reverse = FALSE, copula = new("NullCopula")))

SimResult <- setRefClass("SimResult",
    fields = list(modelType = "character", nRep = "numeric", coef = "data.frame", 
		se = "data.frame", fit = "data.frame", converged = "vector", paramValue = "data.frame", 
		misspecValue = "data.frame", popFit = "data.frame", FMI1 = "data.frame", FMI2 = "data.frame", 
		stdCoef = "data.frame", seed = "numeric", n = "vector", nobs="data.frame", pmMCAR = "vector", pmMAR = "vector", 
		extraOut = "list", paramOnly = "logical", labelParam = "vector", timing = "list"),
	contain = "SimSemParentClass",
    methods = list(
	 clean = function(convergedNew = NULL, improper = FALSE) {
		if (is.null(convergedNew)) {
			targetRep <- 0
			if(improper) targetRep <- c(0, 3:5)
			convergedNew <- converged %in% targetRep
		}
		nRepNew <- sum(convergedNew)
		coefNew <- coef[convergedNew, , drop=FALSE]
		seNew <- se[convergedNew, , drop=FALSE]
		fitNew <- fit[convergedNew, , drop=FALSE]
		if (!is.null(paramValue) && (nrow(paramValue) > 1)) {
			paramValueNew <- paramValue[convergedNew, , drop=FALSE]
		} else {
			paramValueNew <- paramValue
		}
		if (!is.null(misspecValue) && (nrow(misspecValue) > 1)) { 
			misspecValueNew <- misspecValue[convergedNew, , drop=FALSE]
		} else { 
			misspecValueNew <- misspecValue
		}
		if (!is.null(popFit) && (nrow(popFit) > 1)) {
			popFitNew <- popFit[convergedNew, , drop=FALSE]
		} else {
			popFitNew <- popFit
		}
		if (!is.null(extraOut) && (length(extraOut) > 1)) {
			extraOutNew <- extraOut[convergedNew]
		} else {
			extraOutNew <- extraOut
		}
		if (!is.null(FMI1)) {
			FMI1New <- FMI1[convergedNew, , drop=FALSE]
		} else {
			FMI1New <- FMI1
		}
		if (!is.null(FMI2)) {
			FMI2New <- FMI2[convergedNew, , drop=FALSE]
		} else {
			FMI2New <- FMI2
		}
		stdCoefNew <- stdCoef[convergedNew, , drop=FALSE]
		if (length(n) > 1) {
			nNew <- n[convergedNew]
		} else {
			nNew <- n
		}
		if (length(pmMCAR) > 1) {
			pmMCARNew <- pmMCAR[convergedNew]
		} else {
			pmMCARNew <- pmMCAR
		}
		if (length(pmMAR) > 1) {
			pmMARNew <- pmMAR[convergedNew]
		} else {
			pmMARNew <- pmMAR
		}
		convergedNew <- converged[convergedNew]
		SimResult$new(modelType = modelType, nRep = nRepNew, coef = coefNew, 
		se = seNew, fit = fitNew, converged = convergedNew, paramValue = paramValueNew, 
		misspecValue = misspecValueNew, popFit = popFitNew, FMI1 = FMI1New, FMI2 = FMI2New, 
		stdCoef = stdCoefNew, seed = seed, n = nNew, nobs=nobs, pmMCAR = pmMCARNew, pmMAR = pmMARNew, 
		extraOut = extraOutNew, paramOnly = paramOnly, labelParam = labelParam, timing = timing)
	 },
     summary = function(digits = 3, usedFit = NULL, alpha = NULL) {
		if (is.null(usedFit)) 
			usedFit <- getKeywords()$usedFit
		cat("RESULT OBJECT\n")
		cat("Model Type\n")
		print(modelType)
		cleanObj <- clean()
		if(!paramOnly) {
			cat("========= Fit Indices Cutoffs ============\n")
			print(round(summaryFit(cleanObj, alpha = alpha), digits))
			cat("========= Parameter Estimates and Standard Errors ============\n")
			print(summaryParam(cleanObj, digits=digits))
			cat("========= Correlation between Fit Indices ============\n")
			fitNew <- cleanObj$fit[, usedFit]
			if (length(unique(n)) > 1) 
				fitNew <- data.frame(fitNew, n = cleanObj$n)
			if (length(unique(pmMCAR)) > 1) 
				fitNew <- data.frame(fitNew, pmMCAR = cleanObj$pmMCAR)
			if (length(unique(pmMAR)) > 1) 
				fitNew <- data.frame(fitNew, pmMAR = cleanObj$pmMAR)
			print(round(cor(fitNew), digits))
			cat("================== Replications =====================\n")
			cat(paste("Number of replications", "=", nRep, "\n"))
			cat(paste("Number of converged replications", "=", sum(converged == 0), "\n"))
			cat("Number of nonconverged replications: \n")
			cat(paste("   1.", "Nonconvergent Results", "=", sum(converged == 1), "\n"))
			cat(paste("   2.", "Nonconvergent results from multiple imputation", "=", sum(converged == 2), "\n"))
			cat(paste("   3.", "At least one SE were negative or NA", "=", sum(converged == 3), "\n"))
			cat(paste("   4.", "At least one variance estimates were negative", "=", sum(converged == 4), "\n"))
			cat(paste("   5.", "At least one correlation estimates were greater than 1 or less than -1", "=", sum(converged == 5), "\n"))
			if (length(unique(n)) > 1) 
				cat("NOTE: The sample size is varying.\n")
			if (length(unique(pmMCAR)) > 1) 
				cat("NOTE: The percent of MCAR is varying.\n")
			if (length(unique(pmMAR)) > 1) 
				cat("NOTE: The percent of MAR is varying.\n")
			if (!is.null(paramValue)) {
				if ((ncol(coef) != ncol(paramValue)) | ((ncol(coef) == 
					ncol(paramValue)) && any(sort(colnames(coef)) != sort(colnames(paramValue))))) 
					cat("NOTE: The data generation model is not the same as the analysis model. See the summary of the population underlying data generation by the summaryPopulation function.\n")
			}
		} else {
			cat("========= Population Values ============\n")
			print(summaryPopulation(cleanObj), digits)	
			if(!(all(dim(misspecValue) == 0))) {
				cat("========= Model Misspecification ============\n")
				print(round(summaryMisspec(cleanObj), digits))	
			}
		}
     }, 
	 summaryShort = function(alpha=0.05, digits=3) {
		cat("RESULT OBJECT\n")
		cat(paste("Model Type:", print(modelType),"\n"))
		if(paramOnly) {
			cat("This object contains only real and misspecified parameter values.\nUsers may use 'summaryPopulation', 'summaryMisspec', and 'plotMisfit' functions to investigate the parmeter values.\n")
		} else {
			cleanObj <- clean()
			cat(paste("Convergence", sum(converged == 0), "/", nRep, "\n"))
			if (length(unique(n)) > 1) {
				cat(paste("Sample size:", min(n), "to", max(n),"\n"))
			} else {
				cat(paste("Sample size:", unique(n),"\n"))
			}
			if (length(unique(pmMCAR)) > 1) {
				cat(paste("Percent Completely Missing at Random:", min(pmMCAR), "to", max(pmMCAR),"\n"))
			} else {
				cat(paste("Percent Completely Missing at Random:", unique(pmMCAR),"\n"))
			}
			if (length(unique(pmMAR)) > 1) {
				cat(paste("Percent Missing at Random:", min(pmMAR), "to", max(pmMAR),"\n"))
			} else {
				cat(paste("Percent Missing at Random:", unique(pmMAR),"\n"))
			}
			cat("========= Fit Indices Cutoffs ============\n")
			print(round(summaryFit(cleanObj, alpha = alpha), digits))
			if (!is.null(paramValue)) {
				if ((ncol(coef) != ncol(paramValue)) | ((ncol(coef) == 
					ncol(paramValue)) && any(sort(colnames(coef)) != sort(colnames(paramValue))))) 
					cat("NOTE: The data generation model is not the same as the analysis model. See the summary of the population underlying data generation by the summaryPopulation function.\n")
			}
		}
	 },
	 show = function() {
		summaryShort()
	 }
     ))

SimMissing <- setRefClass("SimMissing",
      fields = list(cov = "vector", pmMCAR = "numeric", pmMAR = "numeric", logit = "character", 
    nforms = "numeric", itemGroups = "list", twoMethod = "vector", prAttr = "vector", m = "numeric", chi = "character", 
    package = "character", convergentCutoff = "numeric", timePoints = "numeric", ignoreCols = "vector", threshold = "numeric", 
    covAsAux = "logical", logical = "matrix", args = "list"),
	contain = "SimSemParentClass",
      methods = list(
     summary = function() {
		cat("MISSING OBJECT\n")
		handling <- "Maximum Likelihood"
		cat(paste("The method of missing data handling:", handling, "\n"))
		printcov <- "Covariates:"
		if (length(cov) == 1 && cov == 0) {
			printcov <- paste(printcov, "none", "\n")
		} else {
			printcov <- paste(printcov, paste(cov, collapse = ", "), "\n")
		}
		cat(printcov)
		printignorecol <- "Ignored Variables:"
		if (length(ignoreCols) == 1 && ignoreCols == 0) {
			printignorecol <- paste(printignorecol, "none", "\n")
		} else {
			printignorecol <- paste(printignorecol, paste(ignoreCols, collapse = ", "), "\n")
		}
		cat(printignorecol)
		if (pmMCAR != 0) {
			cat(paste("Proportion of MCAR:", round(pmMCAR, 3), "\n"))
		}
		if (pmMAR != 0) {
			cat(paste("Proportion of MAR:", round(pmMAR, 3), "\n"))
		}
		if (nchar(logit) > 0) {
			cat("Logistic-regression MAR:\n")
			cat(paste(logit, "\n"))
		}
		if (nforms != 0) {
			cat("==========PLANNED MISSING DATA==========\n")
			cat("---------- N-Forms Design ----------\n")
			cat(paste("Number of forms:", ceiling(nforms), "\n"))
			if (!(is.vector(itemGroups) && length(itemGroups) == 1 && itemGroups == 
				0)) {
				if (is.list(itemGroups)) {
					cat("Item Grouping in n-forms design:\n")
					for (i in 1:length(itemGroups)) {
					  cat(paste(i, ". ", paste(itemGroups[[i]], collapse = ", "), 
						"\n", sep = ""))
					}
				}
			}
			cat("=====================================\n")
		}
		if (!(length(twoMethod) == 1 && twoMethod == 0)) {
			cat("==========PLANNED MISSING DATA==========\n")
			cat("---------- Two-Method Design ----------\n")
			cat(paste("Proportion of the missing form:", twoMethod[2], "\n"))
			cat(paste("Variables in the missing form:", paste(twoMethod[1], collapse = ", "), 
				"\n"))
		}
     }, 
	 summaryShort = function() {
		summary()
	 },
	 show = function() {
		summaryShort()
	 }
     ))

printIfNotNull <- function(object, name = NULL) {
    if (!is.null(object)) {
        if (!is.null(name)) 
            cat(name, "\n")
        object$summaryShort()
    }
} 
