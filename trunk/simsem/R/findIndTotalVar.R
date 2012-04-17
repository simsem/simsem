# findIndTotalVar
# Function -- simsem package
# Find indicator total variances based on loading matrix, total factor covariance, and measurement error variances.
# Argument:
#	lambda: 	Factor loading matrix
#	totalFactorCov:	Total factor covariance
#	residualVarTheta:	Measurement error variances
# Return:
#	A vector of indicator variances
# Author: 	Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: April 16, 2012

findIndTotalVar <- function(lambda, totalFactorCov, residualVarTheta) {
	factor.part <- lambda %*% totalFactorCov %*% t(lambda)
	indicator.var <- diag(factor.part) + residualVarTheta
	return(as.vector(indicator.var))
}
