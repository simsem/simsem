# weightedMean
# function -- simsem package
# Calculate the weighted mean of a variable
# Argument:
#	x: vector of a variable
# 	weight: weight variable
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: February 26, 2012

weightedMean <- function(x, weight=NULL) {
	if(is.null(weight)) weight <- rep(1, length(x))
	wm <- sum(weight * x)/sum(weight)
	return(wm)
}
# Example
# weightedMean(1:5, c(1,1,1,1,2))