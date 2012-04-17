# findIndMean
# Function -- simsem package
# Find indicator means based on loading matrix, factor means, and measurement intercept.
# Argument:
#	lambda: 	Factor loading matrix
#	factorMean:	Factor means
#	tau:	Measurement intercepts
# Return:
#	A vector of indicator means
# Author: 	Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: April 16, 2012

findIndMean <- function(lambda, factorMean = NULL, tau = NULL) {
	ni <- nrow(lambda)
	nk <- ncol(lambda)
	if(is.null(factorMean)) factorMean <- rep(0, nk)
	if(is.null(tau)) tau <- rep(0, ni)
	factor.part <- lambda %*% factorMean
	indicator.mean <- tau + factor.part
	return(as.vector(indicator.mean))
}
