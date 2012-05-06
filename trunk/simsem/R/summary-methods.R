# summary: Provide description of an object

# Distribution object: Provide a summary of each distribution object

setMethod("summary", signature(object = "SimNorm"), function(object) {
    print("Random Normal Distribution Object.")
    print(paste("Mean is ", format(object@mean, digits = 3), ".", sep = ""))
    print(paste("Standard deviation is ", format(object@sd, digits = 3), ".", sep = ""))
})

setMethod("summary", signature(object = "SimUnif"), function(object) {
    print("Random Uniform Distribution Object.")
    print(paste("Minimum is ", format(object@min, digits = 3), ".", sep = ""))
    print(paste("Maximum is ", format(object@max, digits = 3), ".", sep = ""))
})

setMethod("summary", signature(object = "SimBeta"), function(object) {
    print("Random Beta Distribution Object.")
    print(paste("Shape 1 (alpha) is ", format(object@shape1, digits = 3), ".", sep = ""))
    print(paste("Shape 2 (beta) is ", format(object@shape2, digits = 3), ".", sep = ""))
    print(paste("Non-centrality parameter is ", format(object@ncp, digits = 3), ".", 
        sep = ""))
})

setMethod("summary", signature(object = "SimBinom"), function(object) {
    print("Random Binomial Distribution Object.")
    print(paste("Number of trials is ", format(object@size, digits = 3), ".", sep = ""))
    print(paste("Probability of success is ", format(object@prob, digits = 3), ".", 
        sep = ""))
})

setMethod("summary", signature(object = "SimCauchy"), function(object) {
    print("Random Cauchy Distribution Object.")
    print(paste("Location parameter is ", format(object@location, digits = 3), ".", 
        sep = ""))
    print(paste("Scale parameter is ", format(object@scale, digits = 3), ".", sep = ""))
})

setMethod("summary", signature(object = "SimChisq"), function(object) {
    print("Random Chi-squared Distribution Object.")
    print(paste("Degree of freedom is ", format(object@df, digits = 3), ".", sep = ""))
    print(paste("Non-centrality parameter is ", format(object@ncp, digits = 3), ".", 
        sep = ""))
})

setMethod("summary", signature(object = "SimExp"), function(object) {
    print("Random Exponential Distribution Object.")
    print(paste("Rate parameter is ", format(object@rate, digits = 3), ".", sep = ""))
})

setMethod("summary", signature(object = "SimF"), function(object) {
    print("Random F Distribution Object.")
    print(paste("Numerator degree of freedom is ", format(object@df1, digits = 3), 
        ".", sep = ""))
    print(paste("Denominator degree of freedom is ", format(object@df2, digits = 3), 
        ".", sep = ""))
    print(paste("Non-centrality parameter is ", format(object@ncp, digits = 3), ".", 
        sep = ""))
})

setMethod("summary", signature(object = "SimGamma"), function(object) {
    print("Random Gamma Distribution Object.")
    print(paste("Shape parameter (alpha) is ", format(object@shape, digits = 3), 
        ".", sep = ""))
    print(paste("Rate parameter (beta) is ", format(object@rate, digits = 3), ".", 
        sep = ""))
})

setMethod("summary", signature(object = "SimGeom"), function(object) {
    print("Random Geometric Distribution Object.")
    print(paste("Probability of successes is ", format(object@prob, digits = 3), 
        ".", sep = ""))
})

setMethod("summary", signature(object = "SimHyper"), function(object) {
    print("Random Hypergeometric Distribution Object.")
    print(paste("The number of successes is ", format(object@m, digits = 3), ".", 
        sep = ""))
    print(paste("The number of failures is ", format(object@n, digits = 3), ".", 
        sep = ""))
    print(paste("The number of drawns is ", format(object@k, digits = 3), ".", sep = ""))
})

setMethod("summary", signature(object = "SimLnorm"), function(object) {
    print("Random Log Normal Distribution Object.")
    print(paste("Mean in log scale is ", format(object@meanlog, digits = 3), ".", 
        sep = ""))
    print(paste("Standard deviation in log scale is ", format(object@sdlog, digits = 3), 
        ".", sep = ""))
})

setMethod("summary", signature(object = "SimLogis"), function(object) {
    print("Random Logistic Distribution Object.")
    print(paste("Location parameter is ", format(object@location, digits = 3), ".", 
        sep = ""))
    print(paste("Scale parameter is ", format(object@scale, digits = 3), ".", sep = ""))
})

setMethod("summary", signature(object = "SimNbinom"), function(object) {
    print("Random Negative Binomial Distribution Object.")
    print(paste("The target number of successful trials is ", format(object@size, 
        digits = 3), ".", sep = ""))
    print(paste("The probability of successes is ", format(object@prob, digits = 3), 
        ".", sep = ""))
})

setMethod("summary", signature(object = "SimPois"), function(object) {
    print("Random Poisson Distribution Object.")
    print(paste("Lambda parameter (mean and variance) is ", format(object@lambda, 
        digits = 3), ".", sep = ""))
})

setMethod("summary", signature(object = "SimT"), function(object) {
    print("Random t Distribution Object.")
    print(paste("Degree of freedom is ", format(object@df, digits = 3), ".", sep = ""))
    print(paste("Non-centrality parameter is ", format(object@ncp, digits = 3), ".", 
        sep = ""))
})

setMethod("summary", signature(object = "SimWeibull"), function(object) {
    print("Random Weibull Distribution Object.")
    print(paste("Shape parameter is ", format(object@shape, digits = 3), ".", sep = ""))
    print(paste("Scale parameter is ", format(object@scale, digits = 3), ".", sep = ""))
})

################################################################################

setMethod("summary", signature = "SimMatrix", definition = function(object) {
    print("Random Full Matrix Object.")
    print("Free/Fixed Parameters:")
    print(object@free)
    print("Parameter/Starting Values:")
    print(object@param)
})

setMethod("summary", signature = "SymMatrix", definition = function(object) {
    print("Random Symmetric Matrix Object.")
    print("Free/Fixed Parameters:")
    print(object@free)
    print("Parameter/Starting Values:")
    print(object@param)
})

setMethod("summary", signature = "SimVector", definition = function(object) {
    print("Random Vector Object.")
    print("Free/Fixed Parameters:")
    print(object@free)
    print("Parameter/Starting Values:")
    print(object@param)
})

setMethod("summary", signature = "SimSet", definition = function(object) {
    cat("SET OF MODEL MATRICES\n")
    cat("Model Type\n")
    print(object@modelType)
    cat("-- Endogeneous Variable --\n")
    printIfNotNull(object@LY, "\nLY: Loading of Indicator.Y on Factor.ETA")
    printIfNotNull(object@TE, "\nTE: Covariance of Measurement.Error.EPSILON")
    printIfNotNull(object@VTE, "\nVTE: Variance of Measurement.Error.EPSILON")
    printIfNotNull(object@RTE, "\nRTE: Correlation of Measurement.Error.EPSILON")
    printIfNotNull(object@VY, "\nVY: Variance of Indicator.Y")
    printIfNotNull(object@TY, "\nTY: Measurement Intercept of Indicator.Y")
    printIfNotNull(object@MY, "\nMY: mean of Indicator.Y")
    printIfNotNull(object@BE, "\nBE: Regression Coefficient among Factor.ETA")
    printIfNotNull(object@PS, "\nPS: Covariance of Regression.Residual.PSI")
    printIfNotNull(object@VPS, "\nVPS: Variance of Regression.Residual.PSI")
    printIfNotNull(object@RPS, "\nRPS: Correlation of Regression.Residual.PSI")
    printIfNotNull(object@VE, "\nVE: Variance of Factor.ETA")
    printIfNotNull(object@AL, "\nAL: Regression Intercept of Factor.ETA")
    printIfNotNull(object@ME, "\nME: mean of Factor.ETA")
    cat("--------------------------", "\n")
    if (object@modelType == "SEM.exo" | object@modelType == "Path.exo") {
        cat("-- Exogeneous Variable --\n")
        printIfNotNull(object@LX, "\nLX: Loading of Indicator.X on Factor.KSI")
        printIfNotNull(object@TD, "\nTD: Covariance of Measurement.Error.DELTA")
        printIfNotNull(object@VTD, "\nVTD: Variance of Measurement.Error.DELTA")
        printIfNotNull(object@RTD, "\nRTD: Correlation of Measurement.Error.DELTA")
        printIfNotNull(object@VX, "\nVX: Variance of Indicator.X")
        printIfNotNull(object@TX, "\nTX: Measurement Intercept of Indicator.X")
        printIfNotNull(object@MX, "\nMX: mean of Indicator.X")
        printIfNotNull(object@GA, "\nGA: Regression Coefficient of Factor.ETA on Factor.KSI")
        printIfNotNull(object@PH, "\nPH: Covariance of Factor.KSI")
        printIfNotNull(object@VPH, "\nVPH: Variance of Factor.KSI")
        printIfNotNull(object@RPH, "\nRPH: Correlation of Factor.KSI")
        printIfNotNull(object@KA, "\nKA: mean of Factor.KSI")
        printIfNotNull(object@TH, "\nTH: Covariance of Measurement.Error.DELTA and Measurement.Error.EPSILON")
        printIfNotNull(object@RTH, "\nRTH: Correlation of Measurement.Error.DELTA and Measurement.Error.EPSILON")
        cat("--------------------------", "\n")
    }
})

setMethod("summary", signature = "SimMisspec", definition = function(object) {
    cat("SET OF MODEL MISSPECIFICATION MATRICES\n")
    cat("Model Type\n")
    print(object@modelType)
    cat("-- Endogeneous Variable --\n")
    printIfNotNull(object@LY, "\nLY: Loading of Indicator.Y on Factor.ETA")
    printIfNotNull(object@TE, "\nTE: Covariance of Measurement.Error.EPSILON")
    printIfNotNull(object@VTE, "\nVTE: Variance of Measurement.Error.EPSILON")
    printIfNotNull(object@RTE, "\nRTE: Correlation of Measurement.Error.EPSILON")
    printIfNotNull(object@VY, "\nVY: Variance of Indicator.Y")
    printIfNotNull(object@TY, "\nTY: Measurement Intercept of Indicator.Y")
    printIfNotNull(object@MY, "\nMY: mean of Indicator.Y")
    printIfNotNull(object@BE, "\nBE: Regression Coefficient among Factor.ETA")
    printIfNotNull(object@PS, "\nPS: Covariance of Regression.Residual.PSI")
    printIfNotNull(object@VPS, "\nVPS: Variance of Regression.Residual.PSI")
    printIfNotNull(object@RPS, "\nRPS: Correlation of Regression.Residual.PSI")
    printIfNotNull(object@VE, "\nVE: Variance of Factor.ETA")
    printIfNotNull(object@AL, "\nAL: Regression Intercept of Factor.ETA")
    printIfNotNull(object@ME, "\nME: mean of Factor.ETA")
    cat("--------------------------", "\n")
    if (object@modelType == "SEM.exo" | object@modelType == "Path.exo") {
        cat("-- Exogeneous Variable --\n")
        printIfNotNull(object@LX, "\nLX: Loading of Indicator.X on Factor.KSI")
        printIfNotNull(object@TD, "\nTD: Covariance of Measurement.Error.DELTA")
        printIfNotNull(object@VTD, "\nVTD: Variance of Measurement.Error.DELTA")
        printIfNotNull(object@RTD, "\nRTD: Correlation of Measurement.Error.DELTA")
        printIfNotNull(object@VX, "\nVX: Variance of Indicator.X")
        printIfNotNull(object@TX, "\nTX: Measurement Intercept of Indicator.X")
        printIfNotNull(object@MX, "\nMX: mean of Indicator.X")
        printIfNotNull(object@GA, "\nGA: Regression Coefficient of Factor.ETA on Factor.KSI")
        printIfNotNull(object@PH, "\nPH: Covariance of Factor.KSI")
        printIfNotNull(object@VPH, "\nVPH: Variance of Factor.KSI")
        printIfNotNull(object@RPH, "\nRPH: Correlation of Factor.KSI")
        printIfNotNull(object@KA, "\nKA: mean of Factor.KSI")
        printIfNotNull(object@TH, "\nTH: Covariance of Measurement.Error.DELTA and Measurement.Error.EPSILON")
        printIfNotNull(object@RTH, "\nRTH: Correlation of Measurement.Error.DELTA and Measurement.Error.EPSILON")
        cat("--------------------------", "\n")
    }
	cat("Constain objects BEFORE or AFTER adding misspecification\n")
	ifelse(object@conBeforeMis, print("Before"), print("After"))
	cat("Misfit bound\n")
	if (!isNullObject(object@misfitBound)) {
		print(paste("min =", object@misfitBound[1]))
		print(paste("max =", object@misfitBound[2]))
	} else {
		print("No")
	}
})

setMethod("summary", signature = "SimEqualCon", definition = function(object) {
    cat("CONSTRAINT OBJECT\n")
    cat("Model Type\n")
    print(object@modelType)
    cat("-------------Constraint----------------\n")
    for (i in 1:length(object@con)) {
        cat(i, ".\n", sep = "")
        print(object@con[[i]])
        cat("---------------------------------------\n")
    }
})

setMethod("summary", signature = "MatrixSet", definition = function(object) {
    cat("RANDOM NUMBERS OF MODEL MATRICES\n")
    cat("Model Type\n")
    print(object@modelType)
    cat("-- Endogeneous Variable --\n")
    printIfNotNull(object@LY, "\nLY: Loading of Indicator.Y on Factor.ETA")
    printIfNotNull(object@TE, "\nTE: Covariance of Measurement.Error.EPSILON")
    printIfNotNull(object@VTE, "\nVTE: Variance of Measurement.Error.EPSILON")
    printIfNotNull(object@RTE, "\nRTE: Correlation of Measurement.Error.EPSILON")
    printIfNotNull(object@VY, "\nVY: Variance of Indicator.Y")
    printIfNotNull(object@TY, "\nTY: Measurement Intercept of Indicator.Y")
    printIfNotNull(object@MY, "\nMY: mean of Indicator.Y")
    printIfNotNull(object@BE, "\nBE: Regression Coefficient among Factor.ETA")
    printIfNotNull(object@PS, "\nPS: Covariance of Regression.Residual.PSI")
    printIfNotNull(object@VPS, "\nVPS: Variance of Regression.Residual.PSI")
    printIfNotNull(object@RPS, "\nRPS: Correlation of Regression.Residual.PSI")
    printIfNotNull(object@VE, "\nVE: Variance of Factor.ETA")
    printIfNotNull(object@AL, "\nAL: Regression Intercept of Factor.ETA")
    printIfNotNull(object@ME, "\nME: mean of Factor.ETA")
    cat("-------------------------------------------------", "\n")
    if (object@modelType == "SEM.exo" | object@modelType == "path.exo") {
        cat("-- Exogeneous Variable --\n")
        printIfNotNull(object@LX, "\nLX: Loading of Indicator.X on Factor.KSI")
        printIfNotNull(object@TD, "\nTD: Covariance of Measurement.Error.DELTA")
        printIfNotNull(object@VTD, "\nVTD: Variance of Measurement.Error.DELTA")
        printIfNotNull(object@RTD, "\nRTD: Correlation of Measurement.Error.DELTA")
        printIfNotNull(object@VX, "\nVX: Variance of Indicator.X")
        printIfNotNull(object@TX, "\nTX: Measurement Intercept of Indicator.X")
        printIfNotNull(object@MX, "\nMX: mean of Indicator.X")
        printIfNotNull(object@GA, "\nGA: Regression Coefficient of Factor.ETA on Factor.KSI")
        printIfNotNull(object@PH, "\nPH: Covariance of Factor.KSI")
        printIfNotNull(object@VPH, "\nVPH: Variance of Factor.KSI")
        printIfNotNull(object@RPH, "\nRPH: Correlation of Factor.KSI")
        printIfNotNull(object@KA, "\nKA: mean of Factor.KSI")
        printIfNotNull(object@TH, "\nTH: Covariance of Measurement.Error.DELTA and Measurement.Error.EPSILON")
        printIfNotNull(object@RTH, "\nRTH: Correlation of Measurement.Error.DELTA and Measurement.Error.EPSILON")
        cat("-------------------------------------------------", "\n")
    }
})

setMethod("summary", signature = "SimParam", definition = function(object) {
    cat("SET OF ESTIMATED PARAMETERS\n")
    cat("Model Type\n")
    print(object@modelType)
    cat("-- Endogeneous Variable --\n")
    printIfNotNull(object@LY, "\nLY: Loading of Indicator.Y on Factor.ETA")
    printIfNotNull(object@TE, "\nTE: Covariance of Measurement.Error.EPSILON")
    printIfNotNull(object@TY, "\nTY: Measurement Intercept of Indicator.Y")
    printIfNotNull(object@BE, "\nBE: Regression Coefficient among Factor.ETA")
    printIfNotNull(object@PS, "\nPS: Covariance of Regression.Residual.PSI")
    printIfNotNull(object@AL, "\nAL: Regression Intercept of Factor.ETA")
    cat("-------------------------------------------------", "\n")
    if (object@modelType == "SEM.exo" | object@modelType == "Path.exo") {
        cat("-- Exogeneous Variable --\n")
        printIfNotNull(object@LX, "\nLX: Loading of Indicator.X on Factor.KSI")
        printIfNotNull(object@TD, "\nTD: Correlation of Measurement.Error.DELTA")
        printIfNotNull(object@TX, "\nTX: Measurement Intercept of Indicator.X")
        printIfNotNull(object@GA, "\nGA: Regression Coefficient of Factor.ETA on Factor.KSI")
        printIfNotNull(object@PH, "\nPH: Correlation of Factor.KSI")
        printIfNotNull(object@KA, "\nKA: mean of Factor.KSI")
        printIfNotNull(object@TH, "\nTH: Correlation of Measurement.Error.DELTA and Measurement.Error.EPSILON")
        cat("-------------------------------------------------", "\n")
    }
})

setMethod("summary", signature = "SimLabels", definition = function(object) {
    cat("SET OF ESTIMATED PARAMETERS\n")
    cat("Model Type\n")
    print(object@modelType)
    cat("-- Endogeneous Variable --\n")
    printIfNotNull(object@LY, "\nLY: Loading of Indicator.Y on Factor.ETA")
    printIfNotNull(object@TE, "\nTE: Covariance of Measurement.Error.EPSILON")
    printIfNotNull(object@TY, "\nTY: Measurement Intercept of Indicator.Y")
    printIfNotNull(object@BE, "\nBE: Regression Coefficient among Factor.ETA")
    printIfNotNull(object@PS, "\nPS: Covariance of Regression.Residual.PSI")
    printIfNotNull(object@AL, "\nAL: Regression Intercept of Factor.ETA")
    cat("-------------------------------------------------", "\n")
    if (object@modelType == "SEM.exo" | object@modelType == "path.exo") {
        cat("-- Exogeneous Variable --\n")
        printIfNotNull(object@LX, "\nLX: Loading of Indicator.X on Factor.KSI")
        printIfNotNull(object@TD, "\nTD: Correlation of Measurement.Error.DELTA")
        printIfNotNull(object@TX, "\nTX: Measurement Intercept of Indicator.X")
        printIfNotNull(object@GA, "\nGA: Regression Coefficient of Factor.ETA on Factor.KSI")
        printIfNotNull(object@PH, "\nPH: Correlation of Factor.KSI")
        printIfNotNull(object@KA, "\nKA: mean of Factor.KSI")
        printIfNotNull(object@TH, "\nTH: Correlation of Measurement.Error.DELTA and Measurement.Error.EPSILON")
        cat("-------------------------------------------------", "\n")
    }
})

setMethod("summary", signature = "SimRSet", definition = function(object) {
    cat("RANDOM NUMBERS OF MODEL MATRICES\n")
    cat("Model Type\n")
    print(object@modelType)
    cat("-- Endogeneous Variable --\n")
    printIfNotNull(object@LY, "\nLY: Loading of Indicator.Y on Factor.ETA")
    printIfNotNull(object@TE, "\nTE: Covariance of Measurement.Error.EPSILON")
    printIfNotNull(object@TY, "\nTY: Measurement Intercept of Indicator.Y")
    printIfNotNull(object@BE, "\nBE: Regression Coefficient among Factor.ETA")
    printIfNotNull(object@PS, "\nPS: Covariance of Regression.Residual.PSI")
    printIfNotNull(object@AL, "\nAL: Regression Intercept of Factor.ETA")
    cat("-------------------------------------------------", "\n")
    if (object@modelType == "SEM.exo" | object@modelType == "path.exo") {
        cat("-- Exogeneous Variable --\n")
        printIfNotNull(object@LX, "\nLX: Loading of Indicator.X on Factor.KSI")
        printIfNotNull(object@TD, "\nTD: Correlation of Measurement.Error.DELTA")
        printIfNotNull(object@TX, "\nTX: Measurement Intercept of Indicator.X")
        printIfNotNull(object@GA, "\nGA: Regression Coefficient of Factor.ETA on Factor.KSI")
        printIfNotNull(object@PH, "\nPH: Correlation of Factor.KSI")
        printIfNotNull(object@KA, "\nKA: mean of Factor.KSI")
        printIfNotNull(object@TH, "\nTH: Correlation of Measurement.Error.DELTA and Measurement.Error.EPSILON")
        cat("-------------------------------------------------", "\n")
    }
})

setMethod("summary", signature = "SimData", definition = function(object, 
    detail = FALSE) {
    cat("DATA OBJECT\n")
    cat("Model Type\n")
    print(object@modelType)
    cat("Sample Size\n")
    print(object@n)
    cat("========= Parameters Set ============\n")
    summary(object@param)
    cat("Number of free parameters = ", countFreeParameters(object@param), "\n")
    cat("=====================================\n")
    if (detail) {
        cat("============Misspecified Set================\n")
        ifelse(!isNullObject(object@misspec), summary(object@misspec), print("None"))
        cat("============================================\n")
        cat("=============Constraint=====================\n")
        ifelse(!isNullObject(object@equalCon), summary(object@SimEqualCon), print("None"))
        cat("============================================\n")
    } else {
        cat("Adding Misspecification?\n")
        ifelse(!isNullObject(object@misspec), print("Yes"), print("No"))
        cat("Adding Constraint?\n")
        ifelse(!isNullObject(object@equalCon), print("Yes"), print("No"))
    }
    cat("Maximum Random Sampling Parameters\n")
    print(object@maxDraw)
})

setMethod("summary", signature = "SimModel", definition = function(object, 
    con = FALSE, start = FALSE) {
    cat("MODEL OBJECT\n")
    cat("Model Type\n")
    print(object@modelType)
    cat("========= Parameters Set ============\n")
    summary(object@param)
	nFree <- countFreeParameters(object@param)
	if(!isNullObject(object@equalCon)) nFree <- nFree + countFreeParameters(object@equalCon)
    cat("Number of free parameters = ", nFree, "\n")
    cat("=====================================\n")
    if (start) {
        cat("============Starting Values================\n")
        ifelse(!isNullObject(object@start), summary(object@start), print("None"))
        cat("============================================\n")
    }
    if (con) {
        cat("=============Constraint=====================\n")
        ifelse(!isNullObject(object@equalCon), summary(object@equalCon), print("None"))
        cat("============================================\n")
    } else {
        cat("Adding Constraint?\n")
        ifelse(!isNullObject(object@equalCon), print("Yes"), print("No"))
    }
    cat("Analysis Package\n")
    print(object@package)
})

setMethod("summary", signature = "SimResult", definition = function(object, 
    digits = 3, usedFit = NULL, alpha = NULL) {
    if (is.null(usedFit)) 
        usedFit <- getKeywords()$usedFit
    cat("RESULT OBJECT\n")
    cat("Model Type\n")
    print(object@modelType)
    cat("========= Fit Indices Cutoffs ============\n")
    if (is.null(alpha)) 
        alpha <- c(0.1, 0.05, 0.01, 0.001)
    cleanObj <- clean(object)
    cutoffs <- round(sapply(alpha, getCutoff, object = cleanObj, usedFit = usedFit), 
        digits)
    if (ncol(as.matrix(cutoffs)) == 1) {
        cutoffs <- t(cutoffs)
        rownames(cutoffs) <- usedFit
    }
    colnames(cutoffs) <- alpha
    names(dimnames(cutoffs)) <- c("Fit Indices", "Alpha")
    print(cutoffs)
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
    if (!isNullObject(object@paramValue)) {
        if ((ncol(object@coef) != ncol(object@paramValue)) | ((ncol(object@coef) == 
            ncol(object@paramValue)) && any(colnames(object@coef) != colnames(object@paramValue)))) 
            cat("NOTE: The data generation model is not the same as the analysis model. See the summary of the population underlying data generation by the summaryPopulation function.\n")
    }
})

setMethod("summary", signature = "SimModelOut", definition = function(object, 
    digits = 3) {
    cat("MODEL ANALYSIS RESULT OBJECT\n")
    cat("Fit Indices\n")
    print(round(object@fit, digits))
    cat("========= Parameter Estimates and Standard Errors ============\n")
    param <- summaryParam(object)
    param[, -ncol(param)] <- round(param[, -ncol(param)], digits)
    print(param)
    cat("Converged\n")
    print(object@converged)
})

setMethod("summary", signature = "SimDataOut", definition = function(object, 
    detail = FALSE) {
    cat("DATA RESULT OBJECT\n")
    cat("Model Type\n")
    print(object@modelType)
    cat("Sample Size\n")
    print(nrow(object@data))
    cat("Data Summary")
    print(summary(object@data))
    if (detail) {
        cat("========= Parameters Set ============\n")
        summary(object@param)
        cat("Number of free parameters = ", countFreeParameters(object@param), "\n")
        cat("=====================================\n")
        cat("============Parameter Values================\n")
        summary(object@paramOut)
        cat("============================================\n")
        cat("============Parameter Values after adding trivial misspecification=====================\n")
        ifelse(!isNullObject(object@misspecOut), summary(object@misspecOut), print("None"))
        cat("============================================\n")
    } else {
        if (isNullObject(object@misspecOut)) {
            cat("============Parameter Values================\n")
            summary(object@paramOut)
            cat("============================================\n")
        } else {
            cat("============Parameter Values after adding trivial misspecification=====================\n")
            summary(object@misspecOut)
            cat("============================================\n")
        }
    }
})

setMethod("summary", signature = "SimMissing", definition = function(object) {
    cat("MISSING OBJECT\n")
    handling <- "Maximum Likelihood"
    if (object@numImps > 0) 
        handling <- paste("Multiple Imputation with", object@numImps, "imputations")
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
    cat(paste("Keep means and variances of the original scales:", object@keepScale, 
        "\n"))
    cat("The list of distributions:\n")
    for (i in 1:object@p) {
        dist <- object@dist[[i]]
        out <- capture.output(summaryShort(dist))
        cat(i, ". ", out, "\n", sep = "")
    }
    cat(paste("Reverse (mirror) distribution:", paste(object@reverse, collapse = " / "), 
        "\n"))
})

setMethod("summary", signature(object = "SimFunction"), definition = function(object) {
    cat("FUNCTION OBJECT\n")
    x <- as.list(object@callfun)
    cat("Function Name = ", x$fun, "\n")
    if (length(x) > 2) {
        cat("Addition attributes = ", paste(names(x)[3:length(x)], collapse = ", "), 
            "\n")
    }
}) 

setMethod("summary", signature(object = "SimResultParam"), definition = function(object, digits=3, usedFit=NULL) {
    if (is.null(usedFit)) 
        usedFit <- getKeywords()$usedFitPop
    cat("PARAMETER RESULT OBJECT\n")
    cat("Model Type\n")
    print(object@modelType)
    cat("========= Parameter Values ============\n")
    print(round(summaryParam(object), digits))
    cat("========= Misspecification Values ============\n")
    misspecAverage <- colMeans(object@misspec, na.rm = TRUE)
    misspecSE <- sapply(object@misspec, sd, na.rm = TRUE)   
    mis <- data.frame(mean = misspecAverage, sd = misspecSE)
    print(round(mis, digits))
    cat("========= Fit Indices Distributions ============\n")
    quantileValue <- c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95)
    cutoffs <- round(apply(object@fit, 2, quantile, quantileValue), digits)
    names(dimnames(cutoffs)) <- c("Fit Indices", "Quantile")
    fitAverage <- colMeans(object@fit, na.rm = TRUE)
    fitSE <- sapply(object@fit, sd, na.rm = TRUE)
	cutoffs <- rbind(cutoffs, fitAverage, fitSE)
    print(round(cutoffs, digits))
	cat("========= Correlation between Fit Indices and Parameter Misspecification ============\n")
    fit <- data.frame(object@misspec, object@fit[, usedFit])
    print(round(cor(fit), digits))
}) 
