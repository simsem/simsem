setMethod("summary",
    signature(object = "Rnorm"),
    function (object)
    {
		print("Random Normal Distribution Object.")
		print(paste("Mean is ", format(object@Mean, digits=3), ".", sep=""))
		print(paste("Standard deviation is ", format(object@SD, digits=3), ".", sep=""))
    }
)

setMethod("summary",
    signature(object = "Runif"),
    function (object) 
    {
		print("Random Uniform Distribution Object.")
		print(paste("Lower bound is ", format(object@Lower, digits=3), ".", sep=""))
		print(paste("Upper bound is ", format(object@Upper, digits=3), ".", sep=""))
    }
)

setMethod("summary", signature="simMatrix", definition = function(object) {
		print("Random Full Matrix Object.")
		print("Parameters:")
		print(object@Data)
		print("Starting Values:")
		print(object@Labels)
	}	
)

setMethod("summary", signature="symMatrix", definition = function(object) {
		print("Random Symmetric Matrix Object.")
		print("Parameters:")
		print(object@Data)
		print("Starting Values:")
		print(object@Labels)
	}	
)

setMethod("summary", signature="simVector", definition = function(object) {
		print("Random Vector Object.")
		print("Parameters:")
		print(object@Data)
		print("Starting Values:")
		print(object@Labels)
	}	
)

setMethod("summary", signature="simMatrixSet", definition= function(object) {
		cat("SET OF MODEL MATRICES\n")
		cat("Type\n")
		print(object@Tag)
		cat("-- Endogeneous Variable --\n")
		print.if.not.null(object@LY, "\nLY: Loading of Indicator.Y on Factor.ETA")
		print.if.not.null(object@VTE, "\nVTE: Variance of Measurement.Error.EPSILON")
		print.if.not.null(object@TE, "\nTE: Correlation of Measurement.Error.EPSILON")
		print.if.not.null(object@VY, "\nVY: Variance of Indicator.Y")
		print.if.not.null(object@TY, "\nTY: Measurement Intercept of Indicator.Y")
		print.if.not.null(object@MY, "\nMY: Mean of Indicator.Y")
		print.if.not.null(object@BE, "\nBE: Regression Coefficient among Factor.ETA")
		print.if.not.null(object@VPS, "\nVPS: Variance of Regression.Residual.PSI")
		print.if.not.null(object@PS, "\nPS: Correlation of Regression.Residual.PSI")
		print.if.not.null(object@VE, "\nVE: Variance of Factor.ETA")
		print.if.not.null(object@AL, "\nAL: Regression Intercept of Factor.ETA")
		print.if.not.null(object@ME, "\nME: Mean of Factor.ETA")
		cat("-------------------------------------------------", "\n")
		cat("-- Exogeneous Variable --\n")
		#browser()
		print.if.not.null(object@LX, "\nLX: Loading of Indicator.X on Factor.KSI")
		print.if.not.null(object@VTD, "\nVTD: Variance of Measurement.Error.DELTA")
		print.if.not.null(object@TD, "\nTD: Correlation of Measurement.Error.DELTA")
		print.if.not.null(object@VX, "\nVX: Variance of Indicator.X")
		print.if.not.null(object@TX, "\nTX: Measurement Intercept of Indicator.X")
		print.if.not.null(object@MX, "\nMX: Mean of Indicator.X")
		print.if.not.null(object@GA, "\nGA: Regression Coefficient of Factor.ETA on Factor.KSI")
		print.if.not.null(object@VPH, "\nVPH: Variance of Factor.KSI")
		print.if.not.null(object@PH, "\nPH: Correlation of Factor.KSI")
		print.if.not.null(object@KA, "\nKA: Mean of Factor.KSI")
		print.if.not.null(object@TH, "\nTH: Correlation of Measurement.Error.DELTA and Measurement.Error.EPSILON")
		cat("-------------------------------------------------", "\n")
	}
)
