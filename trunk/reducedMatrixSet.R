setClass("reducedMatrixSet", 
	contains="blankReducedMatrixSet"
)

reduce.matrices <- function(object) {
	if(!is(object, "matrixSet")) stop("The object is not a matrixSet object")
	PS <- cor2cov(object@PS, sqrt(object@VPS))
	if(object@Tag == "CFA" | object@Tag == "SEM" | object@Tag == "SEM.exo") {
		#browser()
		TE <- cor2cov(object@TE, sqrt(object@VTE))
	} else {
		TE <- .NULL.matrix
	}
	if(object@Tag == "Path.exo" | object@Tag == "SEM.exo") {
		PH <- cor2cov(object@PH, sqrt(object@VPH))
	} else {
		PH <- .NULL.matrix
	}
	if(object@Tag == "SEM.exo") {
		TD <- cor2cov(object@TD, sqrt(object@VTD))
	} else {
		TD <- .NULL.matrix
	}
	
	Output <- new("reducedMatrixSet", Tag=object@Tag, PS=PS, BE=object@BE, AL=object@AL, TE=TE, LY=object@LY, TY=object@TY,
		PH=PH, GA=object@GA, KA=object@KA, TD=TD, LX=object@LX, TX=object@TX, TH=object@TH)
	return(Output)
}

setMethod("create.implied.MACS", signature="reducedMatrixSet", definition=function(object) {
		implied.mean <- NULL
		implied.covariance <- NULL
		ID <- matrix(0, nrow(object@PS), nrow(object@PS))
		diag(ID) <- 1
		if(object@Tag == "CFA") {
			implied.mean <- object@LY %*% object@AL + object@TY
			implied.covariance <- (object@LY %*% object@PS %*% t(object@LY)) + object@TE
		} else if(object@Tag == "Path" | object@Tag == "SEM") {
			implied.mean <- solve(ID - object@BE) %*% object@AL
			implied.covariance <- solve(ID - object@BE) %*% object@PS %*% t(solve(ID - object@BE))
			if(object@Tag == "SEM") {
				implied.mean <- object@TY + (object@LY %*% implied.mean)
				implied.covariance <- (object@LY %*% implied.covariance %*% t(object@LY)) + object@TE
			}
		} else if(object@Tag == "Path.exo" | object@Tag == "SEM.exo") {
			implied.mean.1 <- as.matrix(object@KA)
			implied.mean.2 <- solve(ID - object@BE) %*% (object@AL + (object@GA %*% object@KA))
			implied.covariance.1.1 <- object@PH
			implied.covariance.1.2 <- object@PH %*% t(object@GA) %*% t(solve(ID - object@BE))
			implied.covariance.2.2 <- solve(ID - object@BE) %*% ((object@GA %*% object@PH %*% t(object@GA)) + object@PS) %*% t(solve(ID - object@BE))
			if(object@Tag == "SEM.exo") {
				implied.mean.1 <- object@TX + (object@LX %*% implied.mean.1)
				implied.mean.2 <- object@TY + (object@LY %*% implied.mean.2)
				implied.covariance.1.1 <- (object@LX %*% implied.covariance.1.1 %*% t(object@LX)) + object@TD
				implied.covariance.1.2 <- (object@LX %*% implied.covariance.1.2 %*% t(object@LY)) + object@TH
				implied.covariance.2.2 <- (object@LY %*% implied.covariance.2.2 %*% t(object@LY)) + object@TE
			}
			implied.mean <- rbind(implied.mean.1, implied.mean.2)
			implied.covariance.2.1 <- t(implied.covariance.1.2)
			implied.covariance.1 <- cbind(implied.covariance.1.1, implied.covariance.1.2)
			implied.covariance.2 <- cbind(implied.covariance.2.1, implied.covariance.2.2)
			implied.covariance <- rbind(implied.covariance.1, implied.covariance.2)
		}
		return(list(M = as.vector(implied.mean), CM = implied.covariance))
	}
)

setMethod("summary", signature="reducedMatrixSet", definition=function(object) {
		cat("RANDOM NUMBERS OF MODEL MATRICES\n")
		cat("Type\n")
		print(object@Tag)		
		cat("-- Endogeneous Variable --\n")
		print.if.not.null(object@LY, "\nLY: Loading of Indicator.Y on Factor.ETA")
		print.if.not.null(object@TE, "\nTE: Correlation of Measurement.Error.EPSILON")
		print.if.not.null(object@TY, "\nTY: Measurement Intercept of Indicator.Y")
		print.if.not.null(object@BE, "\nBE: Regression Coefficient among Factor.ETA")
		print.if.not.null(object@PS, "\nPS: Correlation of Regression.Residual.PSI")
		print.if.not.null(object@AL, "\nAL: Regression Intercept of Factor.ETA")
		cat("-------------------------------------------------", "\n")
		cat("-- Exogeneous Variable --\n")
		print.if.not.null(object@LX, "\nLX: Loading of Indicator.X on Factor.KSI")
		print.if.not.null(object@TD, "\nTD: Correlation of Measurement.Error.DELTA")
		print.if.not.null(object@TX, "\nTX: Measurement Intercept of Indicator.X")
		print.if.not.null(object@GA, "\nGA: Regression Coefficient of Factor.ETA on Factor.KSI")
		print.if.not.null(object@PH, "\nPH: Correlation of Factor.KSI")
		print.if.not.null(object@KA, "\nKA: Mean of Factor.KSI")
		print.if.not.null(object@TH, "\nTH: Correlation of Measurement.Error.DELTA and Measurement.Error.EPSILON")
		cat("-------------------------------------------------", "\n")
	}
)




