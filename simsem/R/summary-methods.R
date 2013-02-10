# summary: Provide description of an object

setMethod("summary", signature = "SimMatrix", definition = function(object) {
    print("Random Full Matrix Object.")
    print("Free/Fixed Parameters:")
    print(object@free)
    print("Population Parameters/Starting Values:")
    print(object@popParam)
    print("Model misspecification")
    print(object@misspec)
	print(paste("Symmetric:", object@symmetric))
})

setMethod("summary", signature = "SimVector", definition = function(object) {
    print("Random Vector Object.")
    print("Free/Fixed Parameters:")
    print(object@free)
    print("Population Parameters/Starting Values:")
    print(object@popParam)
    print("Model misspecification")
    print(object@misspec)
})

setMethod("summary", signature = "SimSem", definition = function(object) {
    cat("Model Type\n")
    print(object@modelType)
    temp <- object@dgen
	if("PS" %in% names(temp)) temp <- list(temp)
	if(length(temp) > 1) {
		cat(paste("Number of groups:", length(temp), "\n"))
		cat("Grouping Variable Label\n")
		print(object@groupLab)		
	}
    cat("========================Lavaan Analysis Model========================\n")
    print(as.data.frame(object@pt))
    cat("========================Data Generation Template========================\n")
	for(i in 1:length(temp)) {
		cat(paste0("-------- Group ", i, " --------\n"))
		dgen <- temp[[i]]
		printIfNotNull(dgen$LY, "\nLY: Loading of Indicator.Y on Factor.ETA")
		printIfNotNull(dgen$TE, "\nTE: Covariance of Measurement.Error.EPSILON")
		printIfNotNull(dgen$VTE, "\nVTE: Variance of Measurement.Error.EPSILON")
		printIfNotNull(dgen$RTE, "\nRTE: Correlation of Measurement.Error.EPSILON")
		printIfNotNull(dgen$VY, "\nVY: Variance of Indicator.Y")
		printIfNotNull(dgen$TY, "\nTY: Measurement Intercept of Indicator.Y")
		printIfNotNull(dgen$MY, "\nMY: mean of Indicator.Y")
		printIfNotNull(dgen$BE, "\nBE: Regression Coefficient among Factor.ETA")
		printIfNotNull(dgen$PS, "\nPS: Covariance of Regression.Residual.PSI")
		printIfNotNull(dgen$VPS, "\nVPS: Variance of Regression.Residual.PSI")
		printIfNotNull(dgen$RPS, "\nRPS: Correlation of Regression.Residual.PSI")
		printIfNotNull(dgen$VE, "\nVE: Variance of Factor.ETA")
		printIfNotNull(dgen$AL, "\nAL: Regression Intercept of Factor.ETA")
		printIfNotNull(dgen$ME, "\nME: Mean of Factor.ETA")
		printIfNotNull(dgen$GA, "\nGA: Regression Coefficient of Factor.ETA on Covariates")
		printIfNotNull(dgen$KA, "\nKA: Regression Coefficient of Indicator.Y on Covariates")
		cat("--------------------------", "\n")
	}
	if(any(!(dim(object@con) == 0))) {
		cat("========================Model Constraints========================\n")
		print(as.data.frame(object@con))
	}
})

setMethod("summary", signature = "SimResult", definition = function(object, digits = 3, 
    usedFit = NULL, alpha = NULL) {
	usedFit <- cleanUsedFit(usedFit, colnames(object@fit))
    cat("RESULT OBJECT\n")
    cat("Model Type\n")
    print(object@modelType)
    cleanObj <- clean(object)
    if(!object@paramOnly) {
		cat("========= Fit Indices Cutoffs ============\n")
		print(round(summaryFit(object, alpha = alpha), digits))
		cat("========= Parameter Estimates and Standard Errors ============\n")
		print(summaryParam(object, digits=digits))
		# Correlation between Fit Indices
		fit <- cleanObj@fit[, usedFit]
		if (length(unique(object@n)) > 1) 
			fit <- data.frame(fit, n = cleanObj@n)
		if (length(unique(object@pmMCAR)) > 1) 
			fit <- data.frame(fit, pmMCAR = cleanObj@pmMCAR)
		if (length(unique(object@pmMAR)) > 1) 
			fit <- data.frame(fit, pmMAR = cleanObj@pmMAR)
		if(nrow(fit) > 1) {
			variableCol <- apply(fit, 2, sd, na.rm=TRUE) > 0
			if(sum(variableCol) >= 2) {
				cat("========= Correlation between Fit Indices ============\n")
				print(round(cor(fit[,variableCol]), digits))
			}
		}
		cat("================== Replications =====================\n")
		cat(paste("Number of replications", "=", object@nRep, "\n"))
		cat(paste("Number of converged replications", "=", sum(object@converged == 0), "\n"))
		cat("Number of nonconverged replications: \n")
		cat(paste("   1.", "Nonconvergent Results", "=", sum(object@converged == 1), "\n"))
		cat(paste("   2.", "Nonconvergent results from multiple imputation", "=", sum(object@converged == 2), "\n"))
		cat(paste("   3.", "At least one SE were negative or NA", "=", sum(object@converged == 3), "\n"))
		cat(paste("   4.", "At least one variance estimates were negative", "=", sum(object@converged == 4), "\n"))
		cat(paste("   5.", "At least one correlation estimates were greater than 1 or less than -1", "=", sum(object@converged == 5), "\n"))
		if(any(object@converged == 6)) {
			cat(paste("   6.", "(OpenMx only) Optimal estimates could not be obtained (Status 6)", "=", sum(object@converged == 6), "\n"))
		}
		if (length(unique(object@n)) > 1) 
			cat("NOTE: The sample size is varying.\n")
		if (length(unique(object@pmMCAR)) > 1) 
			cat("NOTE: The percent of MCAR is varying.\n")
		if (length(unique(object@pmMAR)) > 1) 
			cat("NOTE: The percent of MAR is varying.\n")
		if (!is.null(object@paramValue)) {
			targetVar <- match(colnames(object@coef), colnames(object@paramValue))
			targetVar <- targetVar[!is.na(targetVar)]
			targetVar <- colnames(object@paramValue)[targetVar]
			if ((ncol(object@coef) != length(targetVar)) || !all(colnames(object@coef) == targetVar)) 
				cat("NOTE: The data generation model is not the same as the analysis model. See the summary of the population underlying data generation by the summaryPopulation function.\n")
		}
	} else {
		cat("========= Population Values ============\n")
		print(summaryPopulation(object), digits)	
		if(!(all(dim(object@misspecValue) == 0))) {
			cat("========= Model Misspecification ============\n")
			print(round(summaryMisspec(object), digits))	
		}
	}
})

setMethod("summary", signature = "SimMissing", definition = function(object) {
    cat("MISSING OBJECT\n")
    handling <- "Maximum Likelihood"
    cat(paste("The method of missing data handling:", handling, "\n"))
    printcov <- "Covariates:"
    if (length(object@cov) == 1 && object@cov == 0) {
        printcov <- paste(printcov, "none", "\n")
    } else {
        printcov <- paste(printcov, paste(object@cov, collapse = ", "), "\n")
    }
	cat(printcov)
    printignorecol <- "Ignored Variables:"
    if (length(object@ignoreCols) == 1 && object@ignoreCols == 0) {
        printignorecol <- paste(printignorecol, "none", "\n")
    } else {
        printignorecol <- paste(printignorecol, paste(object@ignoreCols, collapse = ", "), "\n")
    }
    cat(printignorecol)
    if (object@pmMCAR != 0) {
        cat(paste("Proportion of MCAR:", round(object@pmMCAR, 3), "\n"))
    }
    if (object@pmMAR != 0) {
        cat(paste("Proportion of MAR:", round(object@pmMAR, 3), "\n"))
    }
    if (nchar(object@logit) > 0) {
        cat("Logistic-regression MAR:\n")
		cat(paste(object@logit, "\n"))
    }
    if (object@nforms != 0) {
        cat("==========PLANNED MISSING DATA==========\n")
        cat("---------- N-Forms Design ----------\n")
        cat(paste("Number of forms:", ceiling(object@nforms), "\n"))
        if (!(is.vector(object@itemGroups) && length(object@itemGroups) == 1 && object@itemGroups == 
            0)) {
            if (is.list(object@itemGroups)) {
                cat("Item Grouping in n-forms design:\n")
                for (i in 1:length(object@itemGroups)) {
                  cat(paste(i, ". ", paste(object@itemGroups[[i]], collapse = ", "), 
                    "\n", sep = ""))
                }
            }
        }
        cat("=====================================\n")
    }
    if (!(length(object@twoMethod) == 1 && object@twoMethod == 0)) {
        cat("==========PLANNED MISSING DATA==========\n")
        cat("---------- Two-Method Design ----------\n")
        cat(paste("Proportion of the missing form:", object@twoMethod[2], "\n"))
        cat(paste("Variables in the missing form:", paste(object@twoMethod[1], collapse = ", "), 
            "\n"))
    }
})

setMethod("summary", signature = "SimDataDist", definition = function(object) {
    cat("DATA DISTRIBUTION OBJECT\n")
    cat(paste("The number of variables is", object@p, "\n"))
    cat(paste("Keep means and variances of the original scales:", paste(object@keepScale, 
        collapse = " / "), "\n"))
    cat(paste("Reverse (mirror) distribution:", paste(object@reverse, collapse = " / "), 
        "\n"))
	if(any(is.na(object@skewness))) {
		cat("The list of distributions:\n")
		attr <- sapply(object@paramMargins, function(x) paste0(names(x), " = ", x, collapse = ", "))
		out <- paste0(object@margins, ": ", attr)
		for (i in 1:object@p) {
			cat(i, ". ", out[i], "\n", sep = "")
		}
		if(!is(object@copula, "NullCopula")) {
			cat(paste("Multivariate Copula:\n"))
			show(object@copula)
		}
	} else {
		cat(paste("Skewness:", paste(object@skewness, 
			collapse = " / "), "\n"))
		cat(paste("(Excessive) Kurtosis:", paste(object@kurtosis, 
			collapse = " / "), "\n"))	
	}
}) 

# printIfNotNull: Provide basic summary of each object if that object is not
# NULL. Mainly call from summary function from SimSet.c object.

# \title{
	# Provide basic summary of each object if that object is not NULL. 
# }
# \description{
	# Provide basic summary of each object if that object is not NULL. This function is mainly used in the \code{summary} function from the \code{linkS4class{SimSet}} object.
# }
# \usage{
# printIfNotNull(object, name=NULL)
# }
# \arguments{
  # \item{object}{
	# The target object to be printed, which can be \code{linkS4class{SimMatrix}}, \code{linkS4class{SymMatrix}}, or \code{linkS4class{SimVector}}.
# }
  # \item{name}{
	# The name of the target object
# }
# }
# \value{
	# None. This function will print only.
# }

printIfNotNull <- function(object, name = NULL) {
    if (!is.null(object)) {
        if (!is.null(name)) 
            cat(name, "\n")
        summaryShort(object)
    }
} 

cleanUsedFit <- function(txt, ...) {
	arg <- list(...)
    if (is.null(txt)) {
		if("chisq.scaled" %in% arg[[1]]) {
			txt <- getKeywords()$usedFitScaled
		} else {
			txt <- getKeywords()$usedFit
		}
	} else {
		txt <- tolower(txt)
		txt[txt == "chi"] <- "chisq"
		txt
	}
	if(length(arg) > 0) {
		txt <- intersection(txt, ...)
		if(length(txt) == 0) stop("The name of fit indices does not match with the saved fit indices.")
	}
	txt
}

# From R-help mailing list posted by John Fox
intersection <- function(x, y, ...){
    if (missing(...)) intersect(x, y)
	else intersect(x, intersection(y, ...))
}