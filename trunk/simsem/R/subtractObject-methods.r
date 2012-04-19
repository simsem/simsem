# subtractObject 
# Methods -- simsem package
# Make a subtract of each element 
# Generic Function: subtractObject(object1, object2)
# Argument:
#	Object1: The first object
# 	Object2: The second object
# 	... : Other arguments (do not have now)
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)

setMethod("subtractObject", signature(object1="SimRSet", object2="SimRSet"), definition= function(object1, object2) {
		modelType <- object1@modelType
		exo <- (modelType == "SEM.exo") | (modelType == "Path.exo")
		nx <- ny <- ne <- nk <- 0
		if(modelType == "CFA") {
			object1@LY <- object1@LY - object2@LY
			object1@PS <- object1@PS - object2@PS
			object1@TE <- object1@TE - object2@TE
			object1@TY <- object1@TY - object2@TY
			object1@AL <- object1@AL - object2@AL
		} else if (modelType == "Path") {
			object1@PS <- object1@PS - object2@PS
			object1@AL <- object1@AL - object2@AL
			object1@BE <- object1@BE - object2@BE
		} else if (modelType == "Path.exo") {
			object1@PS <- object1@PS - object2@PS
			object1@AL <- object1@AL - object2@AL
			object1@BE <- object1@BE - object2@BE
			object1@PH <- object1@PH - object2@PH
			object1@KA <- object1@KA - object2@KA
			object1@GA <- object1@GA - object2@GA
		} else if (modelType == "SEM") {
			object1@LY <- object1@LY - object2@LY
			object1@PS <- object1@PS - object2@PS
			object1@TE <- object1@TE - object2@TE
			object1@TY <- object1@TY - object2@TY
			object1@AL <- object1@AL - object2@AL
			object1@BE <- object1@BE - object2@BE
		} else if (modelType == "SEM.exo") {
			object1@LY <- object1@LY - object2@LY
			object1@PS <- object1@PS - object2@PS
			object1@TE <- object1@TE - object2@TE
			object1@TY <- object1@TY - object2@TY
			object1@AL <- object1@AL - object2@AL
			object1@BE <- object1@BE - object2@BE
			object1@LX <- object1@LX - object2@LX
			object1@PH <- object1@PH - object2@PH
			object1@TD <- object1@TD - object2@TD
			object1@TX <- object1@TX - object2@TX
			object1@KA <- object1@KA - object2@KA
			object1@GA <- object1@GA - object2@GA
			object1@TH <- object1@TH - object2@TH
		} else {
			stop("something wrong!")
		}
		return(object1)
	}
)
#Arguments: object1 are SimRSet.c for the parameter estimates
#			object2 are SimRSet.c for the real parameters
#Description: This function will find the bias by subtracting for parameter estimates from the real parameters.
#Return: SimRSet.c containing bias of parameter estimates.
