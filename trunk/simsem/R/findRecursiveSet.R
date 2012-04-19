# findRecursiveSet
# Function -- simsem package
# Group variables together regarding the position in the mediation chain
# Argument:
#	beta: 	Regression coefficient matrix
# Return:
#	Groupped variables
# Author: 	Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)

findRecursiveSet <- function(beta) {
	result <- list()
	ni <- nrow(beta)
	fix.variable <- rep(FALSE, ni)
	ni.sofar <- 0
	i <- 1
	while(ni.sofar < ni) {
		temp <- findRowZero(beta, fix.variable)
		if(is.null(temp)) stop("The matrix is not recursive.")
		fix.variable[temp] <- TRUE
		result[[i]] <- temp
		i <- i + 1
		ni.sofar <- ni.sofar + length(temp)
	}
	return(result)
}
