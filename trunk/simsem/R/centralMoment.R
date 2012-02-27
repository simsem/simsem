# centralMoment
# function -- simsem package
# Calculate central moments of a variable
# Argument:
#	x: vector of a variable
# 	ord: order of the moment
# 	weight: weight variable
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: February 26, 2012

centralMoment <- function(x, ord, weight=NULL) {
	if(ord < 2) stop("Central moment can be calculated for order 2 or more in an integer.")
	if(is.null(weight)) weight <- rep(1, length(x))	
	wm <- weightedMean(x, weight)
	result <- sum(weight * ((x - wm)^(ord)))/sum(weight)
	return(result)
}
# Example
# centralMoment(1:5, 2)