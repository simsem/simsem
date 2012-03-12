# reduce.matrices
# function -- simsem package
# Reduce the set with correlation/variance/covariance objects into the SimSet
# Argument:
#	object: MatrixSet with correlation/variance objects
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: March 10, 2012

reduce.matrices <- function(object) {
	if(!is(object, "MatrixSet")) stop("The object is not a MatrixSet object")
	Output <- new("SimRSet", modelType=object@modelType, PS=object@PS, BE=object@BE, AL=object@AL, TE=object@TE, LY=object@LY, TY=object@TY, PH=object@PH, GA=object@GA, KA=object@KA, TD=object@TD, LX=object@LX, TX=object@TX, TH=object@TH)
	return(Output)
}
