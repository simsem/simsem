# findFactorTotalCov
# Function -- simsem package
# Find the factor total covariance if regression coefficients and factor covariances (which may be made from factor correlation, total factor variances, and error factor variances) are specified
# Argument:
#	beta: 	Factor regression coefficient matrix
#	psi:	Factor (residual) covariance
#	corPsi:	Factor (residual) correlation
#	totalVarPsi:	Total factor variances
#	errorVarPsi:	Residual error variances
# Return:
#	A matrix of factor covariance
# Author: 	Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: April 16, 2012

findFactorTotalCov <- function(beta, psi=NULL, corPsi=NULL, totalVarPsi = NULL, errorVarPsi=NULL) {
	if(is.null(psi)) {
		library(lavaan)
		if(is.null(errorVarPsi)) errorVarPsi <- findFactorResidualVar(beta, corPsi, totalVarPsi)
		psi <- cor2cov(corPsi, sqrt(errorVarPsi))
	} 
	iden <- diag(nrow(beta))
	facTotalCov <- solve(iden - beta) %*% psi %*% t(solve(iden - beta))
	return(facTotalCov)
}
