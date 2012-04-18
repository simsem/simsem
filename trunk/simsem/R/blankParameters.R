# blankParameters
# Function -- simsem package
# Change all elements in the non-null objects to be all NAs.
# Argument:
#	object: target VirtualRSet class
# Return:	The target object that all elements are NA
# Author: 	Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)

blankParameters <- function(object) {
	if(!isNullObject(object@LY)) object@LY[,] <- NA
	if(!isNullObject(object@TE)) object@TE[,] <- NA
	if(!isNullObject(object@PS)) object@PS[,] <- NA
	if(!isNullObject(object@BE)) object@BE[,] <- NA
	if(!isNullObject(object@TY)) object@TY[] <- NA
	if(!isNullObject(object@AL)) object@AL[] <- NA
	if(!isNullObject(object@LX)) object@LX[,] <- NA
	if(!isNullObject(object@TD)) object@TD[,] <- NA
	if(!isNullObject(object@PH)) object@PH[,] <- NA
	if(!isNullObject(object@GA)) object@GA[,] <- NA
	if(!isNullObject(object@TX)) object@TX[] <- NA
	if(!isNullObject(object@KA)) object@KA[] <- NA
	if(!isNullObject(object@TH)) object@TH[,] <- NA
	return(object)
}
