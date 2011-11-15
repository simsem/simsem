# find.OpenMx.values
# Methods -- simsem package
# Rearrange starting values such that it is appropriate for OpenMx matrix specification such that free parameters are set to be TRUE/FALSE and values meaning be both fixed value or starting values
# Generic Function: find.OpenMx.values(Parameters, Starting.Values)
# Argument:
#	Parameters: 	Set of free parameters (NA = free parameters; numbers = fixed value with a specified number)
# 	Starting.Values: 	Parameter/Starting values
# 	... : Other arguments (do not have now)
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 6, 2011

setMethod("find.OpenMx.values", signature(Parameters="vector", Starting.Values="vector"), definition=function(Parameters, Starting.Values) {
	if(is.null.object(Parameters)) {
		return(Starting.Values)
	} else {
		Starting.Values[!is.na(Parameters)] <- Parameters[!is.na(Parameters)]
		return(round(Starting.Values, 3))
	}
})
#Arguments: 	vector.c of parameters and vector.c of parameter/starting values
#Description: 	This function will add fixed value from parameters vector into starting value vector
#Return: 	Resulting vector.c that includes numbers of fixed parameters and starting values of free parameters.
#Example:
#parameter <- c(NA, NA, 0, 0)
#starting.values <- c(2, 5, 0, 0)
#find.OpenMx.Values(parameter, starting.values)

setMethod("find.OpenMx.values", signature(Parameters="matrix", Starting.Values="matrix"), definition=function(Parameters, Starting.Values) {
	if(is.null.object(Parameters)) {
		return(Starting.Values)
	} else {
		Starting.Values[!is.na(Parameters)] <- Parameters[!is.na(Parameters)]
		return(round(Starting.Values, 3))
	}
})
#Arguments: 	matrix.c of parameters and matrix.c of parameter/starting values
#Description: 	This function will add fixed value from parameters matrix into starting value matrix
#Return: 	Resulting matrix.c that includes numbers of fixed parameters and starting values of free parameters.

setMethod("find.OpenMx.values", signature(Parameters="SimFreeParam", Starting.Values="SimRSet"), definition=function(Parameters, Starting.Values) {
	Starting.Values@LY <- find.OpenMx.values(Parameters@LY, Starting.Values@LY)
	Starting.Values@TE <- find.OpenMx.values(Parameters@TE, Starting.Values@TE)
	Starting.Values@PS <- find.OpenMx.values(Parameters@PS, Starting.Values@PS)
	Starting.Values@BE <- find.OpenMx.values(Parameters@BE, Starting.Values@BE)
	Starting.Values@TY <- find.OpenMx.values(Parameters@TY, Starting.Values@TY)
	Starting.Values@AL <- find.OpenMx.values(Parameters@AL, Starting.Values@AL)
	Starting.Values@LX <- find.OpenMx.values(Parameters@LX, Starting.Values@LX)
	Starting.Values@TD <- find.OpenMx.values(Parameters@TD, Starting.Values@TD)
	Starting.Values@PH <- find.OpenMx.values(Parameters@PH, Starting.Values@PH)
	Starting.Values@GA <- find.OpenMx.values(Parameters@GA, Starting.Values@GA)
	Starting.Values@TX <- find.OpenMx.values(Parameters@TX, Starting.Values@TX)
	Starting.Values@KA <- find.OpenMx.values(Parameters@KA, Starting.Values@KA)
	Starting.Values@TH <- find.OpenMx.values(Parameters@TH, Starting.Values@TH)
	return(Starting.Values)
})
#Arguments: 	
#	Parameters:	SimFreeParam.c that contains specification of free parameters and all fixed value
#	Starting.Values:	SimRSet.c containing starting values in each vector and matrix 
#Description: 	This function will put all fixed values in SimFreeParam.c into set of starting value matrices
#Return: 	Resulting SimRSet.c that includes numbers of fixed parameters and starting values of free parameters.
