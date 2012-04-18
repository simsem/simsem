# countFreeParameters
# Methods -- simsem package
# Count how many parameters in each object
# Generic Function: countFreeParameters(object)
# Argument:
#	object: 	A desired object that users wish to count the number of free parameters
# Return:	The number of free parameters
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)

setMethod("countFreeParameters", signature="SimMatrix", definition=function(object) {
		if(isNullObject(object)) {
			return(0)
		} else {
			Labels <- object@param
			return(sum(!is.na(Labels)))
		}
	}
)
#Description: Count the number of NA in free parameter matrix

setMethod("countFreeParameters", signature="SymMatrix", definition=function(object) {
		if(isNullObject(object)) {
			return(0)
		} else {
			Labels <- object@param
			return(sum(!is.na(Labels[upper.tri(Labels, diag=TRUE)])))
		}
	}
)
#Description: Count the number of NA in upper diagonal elements in free parameter matrix

setMethod("countFreeParameters", signature="SimVector", definition=function(object) {
		if(isNullObject(object)) {
			return(0)
		} else {
			Labels <- object@param
			return(sum(!is.na(Labels)))
		}
	}
)
#Description: Count the number of NA in the elements in free parameter matrix

setMethod("countFreeParameters", signature="SimSet", definition=function(object) {
	return(sum(c(countFreeParameters(object@LY),
		countFreeParameters(object@RTE),
		countFreeParameters(object@VTE),
		countFreeParameters(object@RPS),
		countFreeParameters(object@VPS),
		countFreeParameters(object@BE),
		countFreeParameters(object@TY),
		countFreeParameters(object@AL),
		countFreeParameters(object@ME),
		countFreeParameters(object@MY),
		countFreeParameters(object@VE),
		countFreeParameters(object@VY),
		countFreeParameters(object@LX),
		countFreeParameters(object@RTD),
		countFreeParameters(object@VTD),
		countFreeParameters(object@RPH),
		countFreeParameters(object@GA),
		countFreeParameters(object@TX),
		countFreeParameters(object@KA),
		countFreeParameters(object@MX),
		countFreeParameters(object@VPH),
		countFreeParameters(object@VX),
		countFreeParameters(object@RTH))))
	}
)
#Description: Count the number of NA in the all objects

setMethod("countFreeParameters", signature="matrix", definition=function(object, symmetric=FALSE) {
	if(symmetric){
		return(sum(is.na(object[upper.tri(object, diag=TRUE)])))
	} else {
		return(sum(is.na(object)))
	}
})
#Description: Count the number of NA in the elements in a matrix (in symmetric matrix, the lower tri part is ignored)

setMethod("countFreeParameters", signature="vector", definition=function(object){
	return(sum(is.na(object)))
})
#Description: Count the number of NA in the elements in a vector

setMethod("countFreeParameters", signature="VirtualRSet", definition=function(object) {
	return(sum(c(countFreeParameters(object@LY, symmetric=FALSE),
		countFreeParameters(object@TE, symmetric=TRUE),
		countFreeParameters(object@PS, symmetric=TRUE),
		countFreeParameters(object@BE, symmetric=FALSE),
		countFreeParameters(object@TY),
		countFreeParameters(object@AL),
		countFreeParameters(object@LX, symmetric=FALSE),
		countFreeParameters(object@TD, symmetric=TRUE),
		countFreeParameters(object@PH, symmetric=TRUE),
		countFreeParameters(object@GA, symmetric=FALSE),
		countFreeParameters(object@TX),
		countFreeParameters(object@KA),
		countFreeParameters(object@TH, symmetric=FALSE))))
	}
)
#Description: Count the number of NA in the elements in all objects
