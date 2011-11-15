# simVector
# Function -- simsem package
# Description: 		Create SimVector.c object that save free parameters and starting values, as well as fixed values. 
#		This will be used for model specification later, such as for factor mean vector or measurement error variance vector.
# Function: simVector(Vector, name.dist.object = NULL)
# Argument:
#	Vector:		Vector of free parameters. Use NA to specify free parameters. Use number as fixed value (including zero)
#	name.dist.object:	Starting values. Can be either one element or vector with the same dimension as free parameter vector. 
#						Each element can be numbers (in either numeric or character format) or the name of distribution object VirtualDist.c.
# Return: 	SimVector.c object that will be used for model specification later.
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 6, 2011

simVector <- function(Vector, name.dist.object = NULL) {
	Length <- length(Vector)
	Labels <- rep("", Length)
	if(is.null(name.dist.object)) {
		return(new("SimVector", free=Vector, param=Labels))
	} else {
		if(length(name.dist.object) > 1) {
			if(length(name.dist.object) == Length) {
				for(i in 1:Length) {
					if(is.na(Vector[i])) Labels[i] <- name.dist.object[i]
				}
			} else {
				stop("The length of desired vector and label are not equal")
			}
		} else {
			for(i in 1:Length) {
				if(is.na(Vector[i])) Labels[i] <- name.dist.object
			}
		}
		return(new("SimVector", free=Vector, param=Labels))
	}
}

#Examples:
#factor.mean <- rep(NA, 4)
#AL <- simVector(factor.mean, 0)
#n02 <- simNorm(0, 0.2)
#factor.start <- rep("n02", 4)
#KA <- simVector(factor.mean, factor.start)
