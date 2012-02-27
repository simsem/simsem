# summary
# Methods -- simsem package
# Provide description of an object
# Generic Function: summary(object, ...)
# Argument:
#	object: The target object that is used to summarize
# 	... : Other arguments (None is specified currently)
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: February 23, 2012

################################################################################
# Distribution object: Provide a summary of each distribution object

setMethod("summary",
    signature(object = "SimNorm"),
    function (object)
    {
		print("Random Normal Distribution Object.")
		print(paste("Mean is ", format(object@mean, digits=3), ".", sep=""))
		print(paste("Standard deviation is ", format(object@sd, digits=3), ".", sep=""))
    }
)
# Normal Distribution
# Argument: mean = population mean, sd = standard deviation

setMethod("summary",
    signature(object = "SimUnif"),
    function (object) 
    {
		print("Random Uniform Distribution Object.")
		print(paste("Minimum is ", format(object@min, digits=3), ".", sep=""))
		print(paste("Maximum is ", format(object@max, digits=3), ".", sep=""))
    }
)
# Uniform Distribution
# Argument: min = lower bound, max = upper bound

setMethod("summary",
    signature(object = "SimBeta"),
    function (object)
    {
		print("Random Beta Distribution Object.")
		print(paste("Shape 1 (alpha) is ", format(object@shape1, digits=3), ".", sep=""))
		print(paste("Shape 2 (beta) is ", format(object@shape2, digits=3), ".", sep=""))
		print(paste("Non-centrality parameter is ", format(object@ncp, digits=3), ".", sep=""))
    }
)
# Beta Distribution
# Attributes: shape1, shape2 = positive numbers of beta distributions, ncp = non-centrality parameter (shape1, shape2 > 0)

setMethod("summary",
    signature(object = "SimBinom"),
    function (object)
    {
		print("Random Binomial Distribution Object.")
		print(paste("Number of trials is ", format(object@size, digits=3), ".", sep=""))
		print(paste("Probability of success is ", format(object@prob, digits=3), ".", sep=""))
    }
)
# Binomial Distribution
# Attributes: size = Number of trials (zero or more), prob = probability of success on each trial (0 to 1)

setMethod("summary",
    signature(object = "SimCauchy"),
    function (object)
    {
		print("Random Cauchy Distribution Object.")
		print(paste("Location parameter is ", format(object@location, digits=3), ".", sep=""))
		print(paste("Scale parameter is ", format(object@scale, digits=3), ".", sep=""))
    }
)
# Cauchy Distribution
# Attributes: location = location parameter, scale = scale parameter (> 0)

setMethod("summary",
    signature(object = "SimChisq"),
    function (object)
    {
		print("Random Chi-squared Distribution Object.")
		print(paste("Degree of freedom is ", format(object@df, digits=3), ".", sep=""))
		print(paste("Non-centrality parameter is ", format(object@ncp, digits=3), ".", sep=""))
    }
)
# Chi-squared Distribution
# Attributes: df = degrees of freedom (non-negative), ncp = non-centrality parameter (non-negative)

setMethod("summary",
    signature(object = "SimExp"),
    function (object)
    {
		print("Random Exponential Distribution Object.")
		print(paste("Rate parameter is ", format(object@rate, digits=3), ".", sep=""))
    }
)
# Exponential Distribution
# Attributes: rate = rate parameter

setMethod("summary",
    signature(object = "SimF"),
    function (object)
    {
		print("Random F Distribution Object.")
		print(paste("Numerator degree of freedom is ", format(object@df1, digits=3), ".", sep=""))
		print(paste("Denominator degree of freedom is ", format(object@df2, digits=3), ".", sep=""))
		print(paste("Non-centrality parameter is ", format(object@ncp, digits=3), ".", sep=""))
    }
)
# F-distribution
# Attributes: df1, df2 = degrees of freedom (>0), ncp = non-centrality parameter (>=0)

setMethod("summary",
    signature(object = "SimGamma"),
    function (object)
    {
		print("Random Gamma Distribution Object.")
		print(paste("Shape parameter (alpha) is ", format(object@shape, digits=3), ".", sep=""))
		print(paste("Rate parameter (beta) is ", format(object@rate, digits=3), ".", sep=""))
    }
)
# Gamma Distribution
# Attributes: shape = Shape parameter, scale = Scale parameter

setMethod("summary",
    signature(object = "SimGeom"),
    function (object)
    {
		print("Random Geometric Distribution Object.")
		print(paste("Probability of successes is ", format(object@prob, digits=3), ".", sep=""))
    }
)
# Geometric Distribution
# Attributes: prob = probability of successes

setMethod("summary",
    signature(object = "SimHyper"),
    function (object)
    {
		print("Random Hypergeometric Distribution Object.")
		print(paste("The number of successes is ", format(object@m, digits=3), ".", sep=""))
		print(paste("The number of failures is ", format(object@n, digits=3), ".", sep=""))
		print(paste("The number of drawns is ", format(object@k, digits=3), ".", sep=""))
    }
)
# Hypergeometric Distribution
# Attributes: m = The number of successes, n = The number of failures, k =  The number of drawns (All are integers)

setMethod("summary",
    signature(object = "SimLnorm"),
    function (object)
    {
		print("Random Log Normal Distribution Object.")
		print(paste("Mean in log scale is ", format(object@meanlog, digits=3), ".", sep=""))
		print(paste("Standard deviation in log scale is ", format(object@sdlog, digits=3), ".", sep=""))
    }
)
# Log Normal Distribution
# Attributes: meanlog = mean of the distribution in log scale, sdlog = standard deviation of the distribution in log scale (sdlog > 0)

setMethod("summary",
    signature(object = "SimLogis"),
    function (object)
    {
		print("Random Logistic Distribution Object.")
		print(paste("Location parameter is ", format(object@location, digits=3), ".", sep=""))
		print(paste("Scale parameter is ", format(object@scale, digits=3), ".", sep=""))
    }
)
# Logistic Distribution
# Attributes: location = location parameter, scale = scale parameter (> 0)

setMethod("summary",
    signature(object = "SimNbinom"),
    function (object)
    {
		print("Random Negative Binomial Distribution Object.")
		print(paste("The target number of successful trials is ", format(object@size, digits=3), ".", sep=""))
		print(paste("The probability of successes is ", format(object@prob, digits=3), ".", sep=""))
    }
)
# Negative Binomial Distribution
# Attributes: size = Target for number of sucessful trials (> 0), prob = probability of each trials (0 < p < 1)

setMethod("summary",
    signature(object = "SimPois"),
    function (object)
    {
		print("Random Poisson Distribution Object.")
		print(paste("Lambda parameter (mean and variance) is ", format(object@lambda, digits=3), ".", sep=""))
    }
)
# Poisson Distribution
# Attributes: lambda = mean and variance (> 0)

setMethod("summary",
    signature(object = "SimT"),
    function (object)
    {
		print("Random t Distribution Object.")
		print(paste("Degree of freedom is ", format(object@df, digits=3), ".", sep=""))
		print(paste("Non-centrality parameter is ", format(object@ncp, digits=3), ".", sep=""))
    }
)
# Student t Distribution
# Attributes: df = degree of freedom (> 0), ncp = non-centrality parameter

setMethod("summary",
    signature(object = "SimWeibull"),
    function (object)
    {
		print("Random Weibull Distribution Object.")
		print(paste("Shape parameter is ", format(object@shape, digits=3), ".", sep=""))
		print(paste("Scale parameter is ", format(object@scale, digits=3), ".", sep=""))
    }
)
# Weibull Distribution
# Attributes: shape = shape parameter, scale = scale parameter (> 0)

################################################################################

setMethod("summary", signature="SimMatrix", definition = function(object) {
		print("Random Full Matrix Object.")
		print("Free/Fixed Parameters:")
		print(object@free)
		print("Parameter/Starting Values:")
		print(object@param)
	}	
)
#Arguments: 
#	object:	SimMatrix.c that users wish to summarize
#Description: This function will print free/fixed parameters and parameter/starting values.
#Return: 	NONE. Results will print on screen only.

setMethod("summary", signature="SymMatrix", definition = function(object) {
		print("Random Symmetric Matrix Object.")
		print("Free/Fixed Parameters:")
		print(object@free)
		print("Parameter/Starting Values:")
		print(object@param)
	}	
)
#Arguments: 
#	object:	SymMatrix.c that users wish to summarize
#Description: This function will print free/fixed parameters and parameter/starting values.
#Return: 	NONE. Results will print on screen only.

setMethod("summary", signature="SimVector", definition = function(object) {
		print("Random Vector Object.")
		print("Free/Fixed Parameters:")
		print(object@free)
		print("Parameter/Starting Values:")
		print(object@param)
	}	
)
#Arguments: 
#	object:	SimVector.c that users wish to summarize
#Description: This function will print free/fixed parameters and parameter/starting values.
#Return: 	NONE. Results will print on screen only.

setMethod("summary", signature="SimSet", definition= function(object) {
		cat("SET OF MODEL MATRICES\n")
		cat("Model Type\n")
		print(object@modelType)
		cat("-- Endogeneous Variable --\n")
		print.if.not.null(object@LY, "\nLY: Loading of Indicator.Y on Factor.ETA")
		print.if.not.null(object@VTE, "\nVTE: Variance of Measurement.Error.EPSILON")
		print.if.not.null(object@TE, "\nTE: Correlation of Measurement.Error.EPSILON")
		print.if.not.null(object@VY, "\nVY: Variance of Indicator.Y")
		print.if.not.null(object@TY, "\nTY: Measurement Intercept of Indicator.Y")
		print.if.not.null(object@MY, "\nMY: mean of Indicator.Y")
		print.if.not.null(object@BE, "\nBE: Regression Coefficient among Factor.ETA")
		print.if.not.null(object@VPS, "\nVPS: Variance of Regression.Residual.PSI")
		print.if.not.null(object@PS, "\nPS: Correlation of Regression.Residual.PSI")
		print.if.not.null(object@VE, "\nVE: Variance of Factor.ETA")
		print.if.not.null(object@AL, "\nAL: Regression Intercept of Factor.ETA")
		print.if.not.null(object@ME, "\nME: mean of Factor.ETA")
		cat("--------------------------", "\n")
		if(object@modelType == "SEM.exo" | object@modelType == "path.exo") {
			cat("-- Exogeneous Variable --\n")
			print.if.not.null(object@LX, "\nLX: Loading of Indicator.X on Factor.KSI")
			print.if.not.null(object@VTD, "\nVTD: Variance of Measurement.Error.DELTA")
			print.if.not.null(object@TD, "\nTD: Correlation of Measurement.Error.DELTA")
			print.if.not.null(object@VX, "\nVX: Variance of Indicator.X")
			print.if.not.null(object@TX, "\nTX: Measurement Intercept of Indicator.X")
			print.if.not.null(object@MX, "\nMX: mean of Indicator.X")
			print.if.not.null(object@GA, "\nGA: Regression Coefficient of Factor.ETA on Factor.KSI")
			print.if.not.null(object@VPH, "\nVPH: Variance of Factor.KSI")
			print.if.not.null(object@PH, "\nPH: Correlation of Factor.KSI")
			print.if.not.null(object@KA, "\nKA: mean of Factor.KSI")
			print.if.not.null(object@TH, "\nTH: Correlation of Measurement.Error.DELTA and Measurement.Error.EPSILON")
			cat("--------------------------", "\n")
		}
	}
)
#Arguments: 
#	object:	SimSet.c that users wish to summarize
#Description: This function will print every object that is not null in the model in short format (summaryShort).
#Return: 	NONE. Results will print on screen only.

setMethod("summary", signature="SimEqualCon", definition=function(object){
	cat("CONSTRAINT OBJECT\n")
	cat("Model Type\n")
	print(object@modelType)
	cat("-------------Constraint----------------\n")
	for(i in 1:length(object@con)) {
		cat(i, ".\n", sep="")
		print(object@con[[i]])
		cat("---------------------------------------\n")
	}
})
#Arguments: 
#	object:	SimEqualCon.c that users wish to summarize
#Description: This function will print all constraints in the model.
#Return: 	NONE. Results will print on screen only.

setMethod("summary", signature="MatrixSet", definition=function(object) {
		cat("RANDOM NUMBERS OF MODEL MATRICES\n")
		cat("Model Type\n")
		print(object@modelType)		
		cat("-- Endogeneous Variable --\n")
		print.if.not.null(object@LY, "\nLY: Loading of Indicator.Y on Factor.ETA")
		print.if.not.null(object@VTE, "\nVTE: Variance of Measurement.Error.EPSILON")
		print.if.not.null(object@TE, "\nTE: Correlation of Measurement.Error.EPSILON")
		print.if.not.null(object@VY, "\nVY: Variance of Indicator.Y")
		print.if.not.null(object@TY, "\nTY: Measurement Intercept of Indicator.Y")
		print.if.not.null(object@MY, "\nMY: mean of Indicator.Y")
		print.if.not.null(object@BE, "\nBE: Regression Coefficient among Factor.ETA")
		print.if.not.null(object@VPS, "\nVPS: Variance of Regression.Residual.PSI")
		print.if.not.null(object@PS, "\nPS: Correlation of Regression.Residual.PSI")
		print.if.not.null(object@VE, "\nVE: Variance of Factor.ETA")
		print.if.not.null(object@AL, "\nAL: Regression Intercept of Factor.ETA")
		print.if.not.null(object@ME, "\nME: mean of Factor.ETA")
		cat("-------------------------------------------------", "\n")
		if(object@modelType == "SEM.exo" | object@modelType == "path.exo") {
		cat("-- Exogeneous Variable --\n")
			print.if.not.null(object@LX, "\nLX: Loading of Indicator.X on Factor.KSI")
			print.if.not.null(object@VTD, "\nVTD: Variance of Measurement.Error.DELTA")
			print.if.not.null(object@TD, "\nTD: Correlation of Measurement.Error.DELTA")
			print.if.not.null(object@VX, "\nVX: Variance of Indicator.X")
			print.if.not.null(object@TX, "\nTX: Measurement Intercept of Indicator.X")
			print.if.not.null(object@MX, "\nMX: mean of Indicator.X")
			print.if.not.null(object@GA, "\nGA: Regression Coefficient of Factor.ETA on Factor.KSI")
			print.if.not.null(object@VPH, "\nVPH: Variance of Factor.KSI")
			print.if.not.null(object@PH, "\nPH: Correlation of Factor.KSI")
			print.if.not.null(object@KA, "\nKA: mean of Factor.KSI")
			print.if.not.null(object@TH, "\nTH: Correlation of Measurement.Error.DELTA and Measurement.Error.EPSILON")
			cat("-------------------------------------------------", "\n")
		}
	}
)
#Arguments: 
#	object:	MatrixSet.c that users wish to summarize
#Description: This function will print all matrices or vectors in the MatrixSet.c if not NULL.
#Return: 	NONE. Results will print on screen only.

setMethod("summary", signature="SimFreeParam", definition=function(object) {
		cat("SET OF ESTIMATED PARAMETERS\n")
		cat("Model Type\n")
		print(object@modelType)		
		cat("-- Endogeneous Variable --\n")
		print.if.not.null(object@LY, "\nLY: Loading of Indicator.Y on Factor.ETA")
		print.if.not.null(object@TE, "\nTE: Correlation of Measurement.Error.EPSILON")
		print.if.not.null(object@TY, "\nTY: Measurement Intercept of Indicator.Y")
		print.if.not.null(object@BE, "\nBE: Regression Coefficient among Factor.ETA")
		print.if.not.null(object@PS, "\nPS: Correlation of Regression.Residual.PSI")
		print.if.not.null(object@AL, "\nAL: Regression Intercept of Factor.ETA")
		cat("-------------------------------------------------", "\n")
		if(object@modelType == "SEM.exo" | object@modelType == "path.exo") {
			cat("-- Exogeneous Variable --\n")
			print.if.not.null(object@LX, "\nLX: Loading of Indicator.X on Factor.KSI")
			print.if.not.null(object@TD, "\nTD: Correlation of Measurement.Error.DELTA")
			print.if.not.null(object@TX, "\nTX: Measurement Intercept of Indicator.X")
			print.if.not.null(object@GA, "\nGA: Regression Coefficient of Factor.ETA on Factor.KSI")
			print.if.not.null(object@PH, "\nPH: Correlation of Factor.KSI")
			print.if.not.null(object@KA, "\nKA: mean of Factor.KSI")
			print.if.not.null(object@TH, "\nTH: Correlation of Measurement.Error.DELTA and Measurement.Error.EPSILON")
			cat("-------------------------------------------------", "\n")
		}
	}
)
#Arguments: 
#	object:	SimFreeParam.c that users wish to summarize
#Description: This function will print all matrices or vectors in the SimFreeParam.c if not NULL.
#Return: 	NONE. Results will print on screen only.

setMethod("summary", signature="SimLabels", definition=function(object) {
		cat("SET OF ESTIMATED PARAMETERS\n")
		cat("Model Type\n")
		print(object@modelType)		
		cat("-- Endogeneous Variable --\n")
		print.if.not.null(object@LY, "\nLY: Loading of Indicator.Y on Factor.ETA")
		print.if.not.null(object@TE, "\nTE: Correlation of Measurement.Error.EPSILON")
		print.if.not.null(object@TY, "\nTY: Measurement Intercept of Indicator.Y")
		print.if.not.null(object@BE, "\nBE: Regression Coefficient among Factor.ETA")
		print.if.not.null(object@PS, "\nPS: Correlation of Regression.Residual.PSI")
		print.if.not.null(object@AL, "\nAL: Regression Intercept of Factor.ETA")
		cat("-------------------------------------------------", "\n")
		if(object@modelType == "SEM.exo" | object@modelType == "path.exo") {
			cat("-- Exogeneous Variable --\n")
			print.if.not.null(object@LX, "\nLX: Loading of Indicator.X on Factor.KSI")
			print.if.not.null(object@TD, "\nTD: Correlation of Measurement.Error.DELTA")
			print.if.not.null(object@TX, "\nTX: Measurement Intercept of Indicator.X")
			print.if.not.null(object@GA, "\nGA: Regression Coefficient of Factor.ETA on Factor.KSI")
			print.if.not.null(object@PH, "\nPH: Correlation of Factor.KSI")
			print.if.not.null(object@KA, "\nKA: mean of Factor.KSI")
			print.if.not.null(object@TH, "\nTH: Correlation of Measurement.Error.DELTA and Measurement.Error.EPSILON")
			cat("-------------------------------------------------", "\n")
		}
	}
)
#Arguments: 
#	object:	SimLabels.c that users wish to summarize
#Description: This function will print all matrices or vectors in the SimLabels.c if not NULL.
#Return: 	NONE. Results will print on screen only.

setMethod("summary", signature="SimRSet", definition=function(object) {
		cat("RANDOM NUMBERS OF MODEL MATRICES\n")
		cat("Model Type\n")
		print(object@modelType)		
		cat("-- Endogeneous Variable --\n")
		print.if.not.null(object@LY, "\nLY: Loading of Indicator.Y on Factor.ETA")
		print.if.not.null(object@TE, "\nTE: Correlation of Measurement.Error.EPSILON")
		print.if.not.null(object@TY, "\nTY: Measurement Intercept of Indicator.Y")
		print.if.not.null(object@BE, "\nBE: Regression Coefficient among Factor.ETA")
		print.if.not.null(object@PS, "\nPS: Correlation of Regression.Residual.PSI")
		print.if.not.null(object@AL, "\nAL: Regression Intercept of Factor.ETA")
		cat("-------------------------------------------------", "\n")
		if(object@modelType == "SEM.exo" | object@modelType == "path.exo") {
			cat("-- Exogeneous Variable --\n")
			print.if.not.null(object@LX, "\nLX: Loading of Indicator.X on Factor.KSI")
			print.if.not.null(object@TD, "\nTD: Correlation of Measurement.Error.DELTA")
			print.if.not.null(object@TX, "\nTX: Measurement Intercept of Indicator.X")
			print.if.not.null(object@GA, "\nGA: Regression Coefficient of Factor.ETA on Factor.KSI")
			print.if.not.null(object@PH, "\nPH: Correlation of Factor.KSI")
			print.if.not.null(object@KA, "\nKA: mean of Factor.KSI")
			print.if.not.null(object@TH, "\nTH: Correlation of Measurement.Error.DELTA and Measurement.Error.EPSILON")
			cat("-------------------------------------------------", "\n")
		}
	}
)
#Arguments: 
#	object:	SimRSet.c that users wish to summarize
#Description: This function will print all matrices or vectors in the SimRSet.c if not NULL.
#Return: 	NONE. Results will print on screen only.

setMethod("summary", signature="SimData", definition=function(object, detail=FALSE) {
		cat("DATA OBJECT\n")
		cat("Model Type\n")
		print(object@modelType)
		cat("Sample Size\n")
		print(object@n)
		cat("========= Parameters Set ============\n")
		summary(object@param)
		cat("Number of free parameters = ", count.random.object(object@param), "\n")
		cat("=====================================\n")
		if(detail) {
			cat("============Misspecified Set================\n")
			ifelse(!is.null.object(object@misspec), summary(object@misspec), print("None"))
			cat("============================================\n")
			cat("=============Constraint=====================\n")
			ifelse(!is.null.object(object@equalCon), summary(object@SimEqualCon), print("None"))
			cat("============================================\n")
		} else {
			cat("Adding Misspecification?\n")
			ifelse(!is.null.object(object@misspec), print("Yes"), print("No"))
			cat("Adding Constraint?\n")
			ifelse(!is.null.object(object@equalCon), print("Yes"), print("No"))
		}
		if(!is.null.object(object@misspec) & !is.null.object(object@equalCon)) {
			cat("Constain objects BEFORE or AFTER adding misspecification\n")
			ifelse(object@conBeforeMis, print("Before"), print("After"))
		}
		if(!is.null.object(object@misspec)) {
			cat("Misfit bound\n")
			if(!is.null.object(object@misfitBound)) {
				print(paste("min =", object@misfitBound[1]))
				print(paste("max =", object@misfitBound[2]))
			} else {
				print("No")
			}
		}
		cat("Maximum Random Sampling Parameters\n")
		print(object@maxDraw)
	}
)
#Arguments: 
#	object:	SimData.c that users wish to summarize
#Description: This function will print all elements in the SimData.c.
#Return: 	NONE. Results will print on screen only.

setMethod("summary", signature="SimModel", definition=function(object, con=FALSE, start=FALSE) {
		cat("MODEL OBJECT\n")
		cat("Model Type\n")
		print(object@modelType)
		cat("========= Parameters Set ============\n")
		summary(object@param)
		cat("Number of free parameters = ", count.random.object(object@param), "\n")
		cat("=====================================\n")
		if(start) {
			cat("============Starting Values================\n")
			ifelse(!is.null.object(object@start), summary(object@start), print("None"))
			cat("============================================\n")		
		}
		if(con) {
			cat("=============Constraint=====================\n")
			ifelse(!is.null.object(object@equalCon), summary(object@SimEqualCon), print("None"))
			cat("============================================\n")
		} else {
			cat("Adding Constraint?\n")
			ifelse(!is.null.object(object@equalCon), print("Yes"), print("No"))
		}
		cat("Analysis Package\n")
		print(object@package)
	}
)
#Arguments: 
#	object:	SimModel.c that users wish to summarize
#Description: This function will print all elements in the SimModel.c.
#Return: 	NONE. Results will print on screen only.

setMethod("summary", signature="SimResult", definition=function(object, digits=3) {
		cat("RESULT OBJECT\n")
		cat("Model Type\n")
		print(object@modelType)
		cat("========= Fit Indices Cutoffs ============\n")
		alpha <- c(0.10, 0.05, 0.01, 0.001)
		cutoffs <- round(sapply(alpha, getCutoff, object=object), digits)
		colnames(cutoffs) <- alpha
		names(dimnames(cutoffs)) <- c("Fit Indices", "Alpha")
		print(cutoffs)
		cat("========= Parameter Estimates and Standard Errors ============\n")
		print(round(summaryParam(object), digits))
		cat("Number of Replications\n")
		print(object@nRep)
		cat("Number of Converged Replications\n")
		print(sum(object@converged == TRUE))
	}
)
#Arguments: 
#	object:	SimResult.c that users wish to summarize
#Description: This function will print all elements in the SimResult.c.
#Return: 	NONE. Results will print on screen only.

setMethod("summary", signature="SimModelOut", definition=function(object, digits=3) {
		cat("MODEL ANALYSIS RESULT OBJECT\n")
		cat("Fit Indices\n")
		print(round(object@fit, digits))
		cat("========= Parameter Estimates and Standard Errors ============\n")
		param <- summaryParam(object)
		param[,-ncol(param)] <- round(param[,-ncol(param)], digits)
		print(param)
		cat("Converged\n")
		print(object@converged)
	}
)
#Arguments: 
#	object:	SimModelOut.c that users wish to summarize
#Description: This function will print all elements in the SimModelOut.c.
#Return: 	NONE. Results will print on screen only.

setMethod("summary", signature="SimDataOut", definition=function(object, detail=FALSE) {
		cat("DATA RESULT OBJECT\n")
		cat("Model Type\n")
		print(object@modelType)
		cat("Sample Size\n")
		print(nrow(object@data))
		cat("Data Summary")
		print(summary(object@data))
		if(detail) {
			cat("========= Parameters Set ============\n")
			summary(object@param)
			cat("Number of free parameters = ", count.random.object(object@param), "\n")
			cat("=====================================\n")
			cat("============Parameter Values================\n")
			summary(object@paramOut)
			cat("============================================\n")
			cat("============Parameter Values after adding trivial misspecification=====================\n")
			ifelse(!is.null.object(object@misspecOut), summary(object@misspecOut), print("None"))
			cat("============================================\n")
		} else {
			if(is.null.object(object@misspecOut)) {
				cat("============Parameter Values================\n")
				summary(object@paramOut)
				cat("============================================\n")
			} else {
				cat("============Parameter Values after adding trivial misspecification=====================\n")
				summary(object@misspecOut)
				cat("============================================\n")
			}
		}
	}
)
#Arguments: 
#	object:	SimDataOut.c that users wish to summarize
#Description: This function will print all elements in the SimDataOut.c.
#Return: 	NONE. Results will print on screen only.

setMethod("summary", signature="SimMissing", definition=function(object) {
		cat("MISSING OBJECT\n")
		handling <- "Maximum Likelihood"
		if(object@numImps > 0) handling <- paste("Multiple Imputation with", object@numImps, "imputations")
 		cat(paste("The method of missing data handling:", handling, "\n"))
		printcov <- "Covariates (will not impose any missing values):"
		if(length(object@covs) == 1 && object@covs==0) {
			printcov <- paste(printcov, "none", "\n") 
		} else {
			printcov <- paste(printcov, paste(object@covs, collapse=", "), "\n") 
		}
		cat(printcov)
		if (object@pmMCAR!=0) { cat(paste("Proportion of MCAR:", round(object@pmMCAR, 3), "\n")) }
		if (object@pmMAR!=0) { cat(paste("Proportion of MAR:", round(object@pmMAR, 3), "\n")) } 
		if (object@nforms!=0) {
			cat("==========PLANNED MISSING DATA==========\n")
			cat("---------- N-Forms Design ----------\n")
			cat(paste("Number of forms:", ceiling(object@nforms), "\n"))
			if (!(is.vector(object@itemGroups) && length(object@itemGroups) == 1 && object@itemGroups==0)) {
				if(is.list(object@itemGroups)) {
					cat("Item Grouping in n-forms design:\n")
					for(i in 1:length(object@itemGroups)) {
						cat(paste(i, ". ", paste(object@itemGroups[[i]], collapse=", "), "\n", sep=""))
					}
				}
			}
			cat("=====================================\n")
		}
		if (!(length(object@twoMethod) == 1 && object@twoMethod==0)) {
			cat("==========PLANNED MISSING DATA==========\n")
			cat("---------- Two-Method Design ----------\n")
			cat(paste("Proportion of the missing form:", object@twoMethod[2], "\n"))
			cat(paste("Variables in the missing form:", paste(object@twoMethod[1], collapse=", "), "\n"))
			###############################################
		}
})
#Arguments: 
#	object:	SimMissing class that users wish to summarize
#Description: This function will print all elements in the SimMissing.
#Return: 	NONE. Results will print on screen only.


setMethod("summary", signature="SimDataDist", definition=function(object) {
		cat("DATA DISTRIBUTION OBJECT\n")
 		cat(paste("The number of variables is", object@p, "\n"))
		cat(paste("Keep means and variances of the original scales:", object@keepScale, "\n"))
		cat("The list of distributions:\n")
		for(i in 1:object@p) {
			dist <- object@dist[[i]]
			out <- capture.output(summaryShort(dist))
			cat(i, ". ", out, "\n", sep="")
		}
		cat(paste("Reverse (mirror) distribution:", paste(object@reverse, collapse=" / "), "\n"))
})
#Arguments: 
#	object:	SimDataDist class that users wish to summarize
#Description: This function will print all elements in the SimDataDist.
#Return: 	NONE. Results will print on screen only.

