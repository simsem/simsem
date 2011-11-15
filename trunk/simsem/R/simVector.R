# simVector
# Function -- simsem package
# Description: 		Create SimVector.c object that save free parameters and starting values, as well as fixed values. 
#		This will be used for model specification later, such as for factor mean vector or measurement error variance vector.
# Function: simVector(free, param = NULL)
# Argument:
#	free:		Vector of free parameters. Use NA to specify free parameters. Use number as fixed value (including zero)
#	param:	Starting values. Can be either one element or vector with the same dimension as free parameter vector. 
#						Each element can be numbers (in either numeric or character format) or the name of distribution object VirtualDist.c.
# Return: 	SimVector.c object that will be used for model specification later.
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 6, 2011

simVector <- function(free, param = NULL) {
	Length <- length(free)
	lab <- rep("", Length)
	if(is.null(param)) {
		return(new("SimVector", free=free, param=lab))
	} else {
		if(length(param) > 1) {
			if(length(param) == Length) {
				for(i in 1:Length) {
					if(is.na(free[i])) lab[i] <- param[i]
				}
			} else {
				stop("The length of desired vector and label are not equal")
			}
		} else {
			for(i in 1:Length) {
				if(is.na(free[i])) lab[i] <- param
			}
		}
		return(new("SimVector", free=free, param=lab))
	}
}

#Examples:
#factor.mean <- rep(NA, 4)
#AL <- simVector(factor.mean, 0)
#n02 <- simNorm(0, 0.2)
#factor.start <- rep("n02", 4)
#KA <- simVector(factor.mean, factor.start)
