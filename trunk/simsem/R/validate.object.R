validate.object <- function(object, detail = FALSE) {
	#browser()
	if(!is(object, "MatrixSet")) stop("The object is not a MatrixSet object")
	if(validate.covariance(object@VPS, object@RPS, object@VE) == FALSE) return(FALSE)
	if(object@modelType == "Path" | object@modelType == "Path.exo" | object@modelType == "SEM" | object@modelType == "SEM.exo") {
		if(validate.path(object@BE, object@VE, object@VE) == FALSE) return(FALSE)
	}
	if(object@modelType == "CFA" | object@modelType == "SEM" | object@modelType == "SEM.exo") {
		if(validate.covariance(object@VTE, object@RTE, object@VY) == FALSE) return(FALSE)
		if(validate.path(object@LY, object@VE, object@VY) == FALSE) return(FALSE)
	}
	if(object@modelType == "Path.exo" | object@modelType == "SEM.exo") {
		if(validate.covariance(object@VPH, object@RPH) == FALSE) return(FALSE)
		if(validate.path(object@GA, object@VPH, object@VE) == FALSE) return(FALSE)
	}
	if(object@modelType == "SEM.exo") {
		if(validate.covariance(object@VTD, object@RTD, object@VX) == FALSE) return(FALSE)
		if(validate.path(object@LX, object@VPH, object@VX) == FALSE) return(FALSE)
		maximum.RTH <- sqrt(object@VTD) %o% sqrt(object@VTE)
		abs.RTH <- abs(object@RTH)
		if(sum(abs.RTH > maximum.RTH) > 0) return(FALSE)
	}
	return(TRUE)
}
