# isCorMatrix
# Function -- simsem package
# Check whether a matrix is a possible correlation matrix
# Argument:
#	matrixA:	a matrix to be checked
# Return: 	TRUE if it is a possible correlation matrix
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)

isCorMatrix <- function(matrixA) {
	isSymmetric(matrixA) && all(!is.na(diag(matrixA))) && all(diag(matrixA) == 1) && all(matrixA <= 1) && all(matrixA >= -1)
}
