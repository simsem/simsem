# findFactorMean
# Function -- simsem package
# Find the factor mean if regression coefficients and factor intercept are specified.
# Argument:
#	beta: 	Factor regression coefficient matrix
#	alpha:	Factor intercept from the regression equations. The default is 0.
# Return:
#	A vector of factor means
# Author: 	Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)

findFactorMean <- function(beta, alpha = NULL) {
	ni <- nrow(beta)
	set <- findRecursiveSet(beta)
	factor.mean <- rep(0, ni)
	if(is.null(alpha)) alpha <- rep(0, ni)
	factor.mean[set[[1]]] <- alpha[set[[1]]]
	iv <- NULL
	iv.mean <- factor.mean[set[[1]]]
	for(i in 1:(length(set) - 1)) {
		iv <- c(iv, set[[i]])
		dv <- set[[i + 1]]
		temp.path <- matrix(beta[dv, iv], nrow = length(dv), ncol = length(iv))
		mean.reg <- (temp.path %*% iv.mean)
		factor.mean[dv] <- alpha[dv] + mean.reg
		if(i < (length(set) - 1)) {
			agg <- c(iv, dv)
			iv.mean <- factor.mean[agg]
		}
	}
	return(as.vector(factor.mean))
}
