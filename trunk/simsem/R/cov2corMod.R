# cov2corMod
# function -- simsem package
# The cov2cor function that takes care of the zero-variance variables
# Argument:
#	V: Covariance matrix
# Return: Correlation matrix
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)

cov2corMod <- function(V) {
	targetCol <- which(diag(V) != 0)
	V[targetCol, targetCol] <- cov2cor(V[targetCol, targetCol])
	return(V)
}
