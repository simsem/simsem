reduce.matrices <- function(object) {
	if(!is(object, "matrixSet")) stop("The object is not a matrixSet object")
	PS <- cor2cov(object@PS, sqrt(object@VPS))
	if(object@Tag == "CFA" | object@Tag == "SEM" | object@Tag == "SEM.exo") {
		#browser()
		TE <- cor2cov(object@TE, sqrt(object@VTE))
	} else {
		TE <- new("nullMatrix")
	}
	if(object@Tag == "Path.exo" | object@Tag == "SEM.exo") {
		PH <- cor2cov(object@PH, sqrt(object@VPH))
	} else {
		PH <- new("nullMatrix")
	}
	if(object@Tag == "SEM.exo") {
		TD <- cor2cov(object@TD, sqrt(object@VTD))
	} else {
		TD <- new("nullMatrix")
	}
	
	Output <- new("reducedMatrixSet", Tag=object@Tag, PS=PS, BE=object@BE, AL=object@AL, TE=TE, LY=object@LY, TY=object@TY,
		PH=PH, GA=object@GA, KA=object@KA, TD=TD, LX=object@LX, TX=object@TX, TH=object@TH)
	return(Output)
}
