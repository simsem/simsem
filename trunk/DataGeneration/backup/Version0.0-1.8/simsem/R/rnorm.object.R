# rnorm.object
# Function -- simsem package
# Description: Create random normal distribution object. Random normal distribution object will save mean and standard deviation parameter. 
#		This will use in specifying parameters that distributed as normal distribution.
# Function: rnorm.object(Mean, SD)
# Argument:
#	Mean: 	Desired population mean
# 	SD: 	Desired population standard deviation
# Return: 	Rnorm.c object that save mean and standard deviation of normal distribution object
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 6, 2011

rnorm.object <-
function(Mean, SD) {
	temp <- new("Rnorm", Mean=Mean, SD=SD)
}

#Example:
#    n02 <- rnorm.object(0, 0.2)
#    run(n02)
