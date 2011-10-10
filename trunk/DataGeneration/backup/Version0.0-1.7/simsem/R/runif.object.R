# runif.object
# Function -- simsem package
# Description:	Create random uniform distribution object. Random uniform distribution object will save mean and standard deviation parameter. 
#		This will use in specifying parameters that distributed as normal distribution.
# Function: runif.object(Lower, Upper)
# Argument:
#	Lower: 	Lower bound of the uniform distribution
# 	Upper: 	Upper bound of the uniform distribution
# Return: 	Runif.c object that saves parameters of uniform distribution
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 6, 2011

runif.object <-
function(Lower, Upper) {
	temp <- new("Runif", Lower=Lower, Upper=Upper)
}

#Example:
#u1 <- runif.object(-0.1, 0.1)
#run(u1)
