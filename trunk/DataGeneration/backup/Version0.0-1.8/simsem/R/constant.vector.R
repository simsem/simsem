# constant.vector 
# Function -- simsem package
# Create a constant simVector.c
# Function: constant.vector(constant, ni)
# Argument:
#	constant: 	Number or character that is used to be the constant
# 	ni: 		Number of items
# Return: Constant simVector.c
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 5, 2011

constant.vector <- function(constant, ni) {
	return(new("simVector", Data=rep(constant, ni), Labels=rep(NA, ni)))
}

# Example
# constant.vector(0, 5)
