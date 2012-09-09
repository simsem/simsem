# summary: Provide description of an object

setMethod("summary", signature = "SimMatrix", definition = function(object) {
    print("Random Full Matrix Object.")
    print("Free/Fixed Parameters:")
    print(object@free)
    print("Population Parameters/Starting Values:")
    print(object@popParam)
    print("Model misspecification")
    print(object@misspec)
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
    cat("Lavaan Analysis Model")
    as.data.frame(object@pt)
    cat("Data Generation Template")
    dgen <- object@dgen
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
    printIfNotNull(dgen$ME, "\nME: mean of Factor.ETA")
    cat("--------------------------", "\n")
})

setMethod("summary", signature = "SimResult", definition = function(object, digits = 3, 
    usedFit = NULL, alpha = NULL) {
    if (is.null(usedFit)) 
        usedFit <- getKeywords()$usedFit
    cat("RESULT OBJECT\n")
    cat("Model Type\n")
    print(object@modelType)
    cleanObj <- clean(object)
    
    cat("========= Fit Indices Cutoffs ============\n")
    print(summaryFit(object, alpha = alpha), digits)
    cat("========= Parameter Estimates and Standard Errors ============\n")
    print(round(summaryParam(object), digits))
    cat("========= Correlation between Fit Indices ============\n")
    fit <- cleanObj@fit[, usedFit]
    if (length(unique(object@n)) > 1) 
        fit <- data.frame(fit, n = cleanObj@n)
    if (length(unique(object@pmMCAR)) > 1) 
        fit <- data.frame(fit, pmMCAR = cleanObj@pmMCAR)
    if (length(unique(object@pmMAR)) > 1) 
        fit <- data.frame(fit, pmMAR = cleanObj@pmMAR)
    print(round(cor(fit), digits))
    cat("================== Replications =====================\n")
    cat("Number of Replications\n")
    print(object@nRep)
    cat("Number of Converged Replications\n")
    print(sum(object@converged == TRUE))
    if (length(unique(object@n)) > 1) 
        cat("NOTE: The sample size is varying.\n")
    if (length(unique(object@pmMCAR)) > 1) 
        cat("NOTE: The percent of MCAR is varying.\n")
    if (length(unique(object@pmMAR)) > 1) 
        cat("NOTE: The percent of MAR is varying.\n")
    if (!is.null(object@paramValue)) {
        if ((ncol(object@coef) != ncol(object@paramValue)) | ((ncol(object@coef) == 
            ncol(object@paramValue)) && any(sort(colnames(object@coef)) != sort(colnames(object@paramValue))))) 
            cat("NOTE: The data generation model is not the same as the analysis model. See the summary of the population underlying data generation by the summaryPopulation function.\n")
    }
})

setMethod("summary", signature = "SimMissing", definition = function(object) {
    cat("MISSING OBJECT\n")
    handling <- "Maximum Likelihood"
    cat(paste("The method of missing data handling:", handling, "\n"))
    printcov <- "Covariates (will not impose any missing values):"
    if (length(object@cov) == 1 && object@cov == 0) {
        printcov <- paste(printcov, "none", "\n")
    } else {
        printcov <- paste(printcov, paste(object@cov, collapse = ", "), "\n")
    }
    cat(printcov)
    if (object@pmMCAR != 0) {
        cat(paste("Proportion of MCAR:", round(object@pmMCAR, 3), "\n"))
    }
    if (object@pmMAR != 0) {
        cat(paste("Proportion of MAR:", round(object@pmMAR, 3), "\n"))
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
    cat("The list of distributions:\n")
    attr <- sapply(object@paramMargins, function(x) paste0(names(x), " = ", x, collapse = ", "))
    out <- paste0(object@margins, ": ", attr)
    for (i in 1:object@p) {
        cat(i, ". ", out[i], "\n", sep = "")
    }
    cat(paste("Reverse (mirror) distribution:", paste(object@reverse, collapse = " / "), 
        "\n"))
}) 
