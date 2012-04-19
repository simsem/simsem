# findFactorIntercept
# Function -- simsem package
# Find the factor intercept if regression coefficients and factor means are specified
# Argument:
#	beta: 	Factor regression coefficient matrix
#	factorMean:	Factor total mean
# Return:
#	A matrix of factor intercept
# Author: 	Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)

findFactorIntercept <- function(beta, factorMean = NULL) {
	ni <- nrow(beta)
	set <- findRecursiveSet(beta)
	intercept <- rep(0, ni)
	if(is.null(factorMean)) factorMean <- rep(0, ni)
	intercept[set[[1]]] <- factorMean[set[[1]]]
	iv <- NULL
	iv.mean <- factorMean[set[[1]]]
	for(i in 1:(length(set) - 1)) {
		iv <- c(iv, set[[i]])
		dv <- set[[i + 1]]
		temp.path <- matrix(beta[dv, iv], nrow = length(dv), ncol = length(iv))
		mean.reg <- (temp.path %*% iv.mean)
		dv.mean <- factorMean[dv]
		intercept[dv] <- dv.mean - mean.reg
		if(i < (length(set) - 1)) {
			agg <- c(iv, dv)
			iv.mean <- factorMean[agg]
		}
	}
	return(as.vector(intercept))
}
