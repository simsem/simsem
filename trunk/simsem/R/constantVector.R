# constantVector 
# Function -- simsem package
# Create a constant SimVector.c
# Function: constant.vector(constant, ni)
# Argument:
#	constant: 	Number or character that is used to be the constant
# 	ni: 		Number of items
# Return: Constant SimVector.c
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: November 16, 2011

constantVector <- function(constant, ni) {
	return(new("SimVector", free=rep(constant, ni), param=rep(NA, ni)))
}

# Example
# constantVector(0, 5)
