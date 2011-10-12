validate.object <- function(object, detail = FALSE) {
	#browser()
	if(!is(object, "matrixSet")) stop("The object is not a matrixSet object")
	if(validate.covariance(object@VPS, object@PS, object@VE) == FALSE) return(FALSE)
	if(object@Tag == "Path" | object@Tag == "Path.exo" | object@Tag == "SEM" | object@Tag == "SEM.exo") {
		if(validate.path(object@BE, object@VE, object@VE) == FALSE) return(FALSE)
	}
	if(object@Tag == "CFA" | object@Tag == "SEM" | object@Tag == "SEM.exo") {
		if(validate.covariance(object@VTE, object@TE, object@VY) == FALSE) return(FALSE)
		if(validate.path(object@LY, object@VE, object@VY) == FALSE) return(FALSE)
	}
	if(object@Tag == "Path.exo" | object@Tag == "SEM.exo") {
		if(validate.covariance(object@VPH, object@PH) == FALSE) return(FALSE)
		if(validate.path(object@GA, object@VPH, object@VE) == FALSE) return(FALSE)
	}
	if(object@Tag == "SEM.exo") {
		if(validate.covariance(object@VTD, object@TD, object@VX) == FALSE) return(FALSE)
		if(validate.path(object@LX, object@VPH, object@VX) == FALSE) return(FALSE)
		maximum.TH <- sqrt(object@VTD) %o% sqrt(object@VTE)
		abs.TH <- abs(object@TH)
		if(sum(abs.TH > maximum.TH) > 0) return(FALSE)
	}
	return(TRUE)
}
