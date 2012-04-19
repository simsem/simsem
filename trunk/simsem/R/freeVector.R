# freeVector
# Function -- simsem package
# Create a free parameters vector with a starting values in SimVector.c
# Function: freeVector(constant, ni)
# Argument:
#	start: 	Number or character that is used to be the starting values/parameter values
# 	ni: 		Number of items
# Return: free SimVector.c with starting values
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)

freeVector <- function(start, ni) {
	return(new("SimVector", free=rep(NA, ni), param=rep(start, ni)))
}

# Example
# freeVector(0, 5)
