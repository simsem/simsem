# extractDiag
# function -- simsem package
# Extract only diagonal elements from the matrix but still keep the matrix format
# Argument:
#	M: Target Matrix
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: February 21, 2012

extractDiag <- function(M) {
	if(!isSymmetric(M)) stop("Cannot extract nondiagonal matrix")
	x <- diag(M)
	result <- matrix(0, nrow(M), nrow(M))
	diag(result) <- x
	return(result)
}

# Example
# S <- matrix(0.5, 2, 2)
# extractDiag(S)