# findOpenMxValues
# Methods -- simsem package
# Rearrange starting values such that it is appropriate for OpenMx matrix specification such that free parameters are set to be TRUE/FALSE and values meaning be both fixed value or starting values
# Generic Function: findOpenMxValues(param, start)
# Argument:
#	param: 	Set of free parameters (NA = free parameters; numbers = fixed value with a specified number)
# 	start: 	Parameter/Starting values
# 	... : Other arguments (do not have now)
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 6, 2011

setMethod("findOpenMxValues", signature(param="vector", start="vector"), definition=function(param, start) {
	if(isNullObject(param)) {
		return(start)
	} else {
		start[!is.na(param)] <- param[!is.na(param)]
		return(round(start, 3))
	}
})
#Arguments: 	vector.c of parameters and vector.c of parameter/starting values
#Description: 	This function will add fixed value from parameters vector into starting value vector
#Return: 	Resulting vector.c that includes numbers of fixed parameters and starting values of free parameters.
#Example:
#parameter <- c(NA, NA, 0, 0)
#startingValues <- c(2, 5, 0, 0)
#findOpenMxValues(parameter, startingValues)

setMethod("findOpenMxValues", signature(param="matrix", start="matrix"), definition=function(param, start) {
	if(isNullObject(param)) {
		return(start)
	} else {
		start[!is.na(param)] <- param[!is.na(param)]
		return(round(start, 3))
	}
})
#Arguments: 	matrix.c of parameters and matrix.c of parameter/starting values
#Description: 	This function will add fixed value from parameters matrix into starting value matrix
#Return: 	Resulting matrix.c that includes numbers of fixed parameters and starting values of free parameters.

setMethod("findOpenMxValues", signature(param="SimFreeParam", start="SimRSet"), definition=function(param, start) {
	start@LY <- findOpenMxValues(param@LY, start@LY)
	start@TE <- findOpenMxValues(param@TE, start@TE)
	start@PS <- findOpenMxValues(param@PS, start@PS)
	start@BE <- findOpenMxValues(param@BE, start@BE)
	start@TY <- findOpenMxValues(param@TY, start@TY)
	start@AL <- findOpenMxValues(param@AL, start@AL)
	start@LX <- findOpenMxValues(param@LX, start@LX)
	start@TD <- findOpenMxValues(param@TD, start@TD)
	start@PH <- findOpenMxValues(param@PH, start@PH)
	start@GA <- findOpenMxValues(param@GA, start@GA)
	start@TX <- findOpenMxValues(param@TX, start@TX)
	start@KA <- findOpenMxValues(param@KA, start@KA)
	start@TH <- findOpenMxValues(param@TH, start@TH)
	return(start)
})
#Arguments: 	
#	param:	SimFreeParam.c that contains specification of free parameters and all fixed value
#	start:	SimRSet.c containing starting values in each vector and matrix 
#Description: 	This function will put all fixed values in SimFreeParam.c into set of starting value matrices
#Return: 	Resulting SimRSet.c that includes numbers of fixed parameters and starting values of free parameters.
