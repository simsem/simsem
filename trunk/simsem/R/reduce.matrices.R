reduce.matrices <- function(object) {
	if(!is(object, "MatrixSet")) stop("The object is not a MatrixSet object")
	if(sum(object@VPS < 0) > 0) return(new("NullRSet"))
	PS <- cor2cov(object@PS, sqrt(object@VPS))
	if(object@modelType == "CFA" | object@modelType == "SEM" | object@modelType == "SEM.exo") {
		#browser()
		if(sum(object@VTE < 0) > 0) return(new("NullRSet"))
		TE <- cor2cov(object@TE, sqrt(object@VTE))
	} else {
		TE <- new("NullMatrix")
	}
	if(object@modelType == "Path.exo" | object@modelType == "SEM.exo") {
		if(sum(object@VPH < 0) > 0) return(new("NullRSet"))
		PH <- cor2cov(object@PH, sqrt(object@VPH))
	} else {
		PH <- new("NullMatrix")
	}
	if(object@modelType == "SEM.exo") {
		if(sum(object@VTD < 0) > 0) return(new("NullRSet"))
		TD <- cor2cov(object@TD, sqrt(object@VTD))
	} else {
		TD <- new("NullMatrix")
	}
	
	Output <- new("SimRSet", modelType=object@modelType, PS=PS, BE=object@BE, AL=object@AL, TE=TE, LY=object@LY, TY=object@TY,
		PH=PH, GA=object@GA, KA=object@KA, TD=TD, LX=object@LX, TX=object@TX, TH=object@TH)
	return(Output)
}
