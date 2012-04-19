# findFactorTotalVar
# Function -- simsem package
# Find the factor total variance if regression coefficients, factor correlation, and factor residual variances are specified.
# Argument:
#	beta: 	Factor regression coefficient matrix
#	corPsi:	Factor (or residual) correlation.
#	residualVarPsi:	Residual variance of factors.
# Return:
#	A vector of factor total variances
# Author: 	Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)

findFactorTotalVar <- function(beta, corPsi, residualVarPsi) {
	library(lavaan)
	ni <- nrow(beta)
	set <- findRecursiveSet(beta)
	real.psi <- cor2cov(corPsi, sqrt(residualVarPsi))
	ID <- matrix(0, ni, ni)
	diag(ID) <- 1
	iv.cov <- solve(ID - beta) %*% real.psi %*% t(solve(ID - beta))
	factor.var <- diag(iv.cov)
	return(as.vector(factor.var))
}
