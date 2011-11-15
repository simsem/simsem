# simNorm
# Function -- simsem package
# Description: Create random normal distribution object. Random normal distribution object will save mean and standard deviation parameter. 
#		This will use in specifying parameters that distributed as normal distribution.
# Function: simNorm(mean, sd)
# Argument:
#	mean: 	Desired population mean
# 	sd: 	Desired population standard deviation
# Return: 	SimNorm.c object that save mean and standard deviation of normal distribution object
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 6, 2011

simNorm <-
function(mean, sd) {
	temp <- new("SimNorm", mean=mean, sd=sd)
}

#Example:
#    n02 <- simNorm(0, 0.2)
#    run(n02)
