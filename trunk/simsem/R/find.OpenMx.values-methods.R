# find.OpenMx.values
# Methods -- simsem package
# Rearrange starting values such that it is appropriate for OpenMx matrix specification such that free parameters are set to be TRUE/FALSE and values meaning be both fixed value or starting values
# Generic Function: find.OpenMx.values(param, start)
# Argument:
#	param: 	Set of free parameters (NA = free parameters; numbers = fixed value with a specified number)
# 	start: 	Parameter/Starting values
# 	... : Other arguments (do not have now)
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 6, 2011

setMethod("find.OpenMx.values", signature(param="vector", start="vector"), definition=function(param, start) {
	if(is.null.object(param)) {
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
#starting.values <- c(2, 5, 0, 0)
#find.OpenMx.Values(parameter, starting.values)

setMethod("find.OpenMx.values", signature(param="matrix", start="matrix"), definition=function(param, start) {
	if(is.null.object(param)) {
		return(start)
	} else {
		start[!is.na(param)] <- param[!is.na(param)]
		return(round(start, 3))
	}
})
#Arguments: 	matrix.c of parameters and matrix.c of parameter/starting values
#Description: 	This function will add fixed value from parameters matrix into starting value matrix
#Return: 	Resulting matrix.c that includes numbers of fixed parameters and starting values of free parameters.

setMethod("find.OpenMx.values", signature(param="SimFreeParam", start="SimRSet"), definition=function(param, start) {
	start@LY <- find.OpenMx.values(param@LY, start@LY)
	start@TE <- find.OpenMx.values(param@TE, start@TE)
	start@PS <- find.OpenMx.values(param@PS, start@PS)
	start@BE <- find.OpenMx.values(param@BE, start@BE)
	start@TY <- find.OpenMx.values(param@TY, start@TY)
	start@AL <- find.OpenMx.values(param@AL, start@AL)
	start@LX <- find.OpenMx.values(param@LX, start@LX)
	start@TD <- find.OpenMx.values(param@TD, start@TD)
	start@PH <- find.OpenMx.values(param@PH, start@PH)
	start@GA <- find.OpenMx.values(param@GA, start@GA)
	start@TX <- find.OpenMx.values(param@TX, start@TX)
	start@KA <- find.OpenMx.values(param@KA, start@KA)
	start@TH <- find.OpenMx.values(param@TH, start@TH)
	return(start)
})
#Arguments: 	
#	param:	SimFreeParam.c that contains specification of free parameters and all fixed value
#	start:	SimRSet.c containing starting values in each vector and matrix 
#Description: 	This function will put all fixed values in SimFreeParam.c into set of starting value matrices
#Return: 	Resulting SimRSet.c that includes numbers of fixed parameters and starting values of free parameters.
