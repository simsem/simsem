# simUnif
# Function -- simsem package
# Description:	Create random uniform distribution object. Random uniform distribution object will save mean and standard deviation parameter. 
#		This will use in specifying parameters that distributed as normal distribution.
# Function: simUnif(min, max)
# Argument:
#	min: 	min bound of the uniform distribution
# 	max: 	max bound of the uniform distribution
# Return: 	SimUnif.c object that saves parameters of uniform distribution
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 6, 2011

simUnif <-
function(min, max) {
	temp <- new("SimUnif", min=min, max=max)
}

#Example:
#u1 <- simUnif(-0.1, 0.1)
#run(u1)
