# reduceMatrices
# function -- simsem package
# Reduce the set with correlation/variance/covariance objects into the SimSet
# Argument:
#	object: MatrixSet with correlation/variance objects
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)

reduceMatrices <- function(object) {
	library(lavaan)
	if(!is(object, "MatrixSet")) stop("The object is not a MatrixSet object")
	if(isNullObject(object@PS)) object@PS <- cor2cov(object@RPS, sqrt(object@VPS))
	if(object@modelType == "CFA" | object@modelType == "SEM" | object@modelType == "SEM.exo") {
		if(isNullObject(object@TE)) object@TE <- cor2cov(object@RTE, sqrt(object@VTE))
	} 
	if(object@modelType == "Path.exo" | object@modelType == "SEM.exo") {
		if(isNullObject(object@PH)) object@PH <- cor2cov(object@RPH, sqrt(object@VPH))
	} 
	if(object@modelType == "SEM.exo") {
		if(isNullObject(object@TD)) object@TD <- cor2cov(object@RTD, sqrt(object@VTD))
	} 
	
	Output <- new("SimRSet", modelType=object@modelType, PS=object@PS, BE=object@BE, AL=object@AL, TE=object@TE, LY=object@LY, TY=object@TY, PH=object@PH, GA=object@GA, KA=object@KA, TD=object@TD, LX=object@LX, TX=object@TX, TH=object@TH)
	return(Output)
}
