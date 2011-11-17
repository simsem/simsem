# summary
# Methods -- simsem package
# Provide description of an object
# Generic Function: summary(object, ...)
# Argument:
#	object: The target object that is used to summarize
# 	... : Other arguments (None is specified currently)
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: November 16, 2011

setMethod("summary",
    signature(object = "SimNorm"),
    function (object)
    {
		print("Random Normal Distribution Object.")
		print(paste("Mean is ", format(object@mean, digits=3), ".", sep=""))
		print(paste("Standard deviation is ", format(object@sd, digits=3), ".", sep=""))
    }
)
#Arguments: 
#	object:	SimNorm.c that users wish to summarize
#Description: This function will print all attributes of an object
#Return: 	NONE. Results will print on screen only.

setMethod("summary",
    signature(object = "SimUnif"),
    function (object) 
    {
		print("Random Uniform Distribution Object.")
		print(paste("Minimum is ", format(object@min, digits=3), ".", sep=""))
		print(paste("Maximum is ", format(object@max, digits=3), ".", sep=""))
    }
)
#Arguments: 
#	object:	SimUnif.c that users wish to summarize
#Description: This function will print all attributes of an object
#Return: 	NONE. Results will print on screen only.

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
		print(round(summaryParam(object), digits))
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


