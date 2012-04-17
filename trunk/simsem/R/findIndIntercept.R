# findIndIntercept
# Function -- simsem package
# Find the measurement intercept if factor loading, total factor covariance, and total indicator variances are specified
# Argument:
#	lambda: 	Factor loading
#	factorMean:	Factor total mean
#	indicatorMean:	Indicator total means
# Return:
#	A vector of measurement intercepts
# Author: 	Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: April 16, 2012

findIndIntercept <- function(lambda, factorMean = NULL, indicatorMean = NULL) {
	ni <- nrow(lambda)
	nk <- ncol(lambda)
	if(is.null(factorMean)) factorMean <- rep(0, nk)
	if(is.null(indicatorMean)) indicatorMean <- rep(0, ni)
	factor.part <- lambda %*% factorMean
	intercept <- indicatorMean - factor.part
	return(as.vector(intercept))
}
