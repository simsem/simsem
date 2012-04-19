# findRowZero
# Function -- simsem package
# Find rows in a matrix that all elements are zero in non-fixed subset rows and columns. This function will be used in findRecursiveSet function.
# Argument:
#	square.matrix: 	Any square matrix
#	is.row.fixed:	A logical vector. The TRUE position 
# Return:
#	A vector of positions that contain rows of all zeros
# Author: 	Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)

findRowZero <- function(square.matrix, is.row.fixed = FALSE) {
	ni <- nrow(square.matrix)
	if(length(is.row.fixed) == 1) {
		if(is.row.fixed == FALSE) is.row.fixed <- rep(FALSE, ni)
	}
	result <- NULL
	desired.zero <- sum(!is.row.fixed)
	for(i in 1:ni) {
		if(is.row.fixed[i] == FALSE) {	
			temp <- sum(square.matrix[i,!is.row.fixed] == 0, na.rm = TRUE)
			if(temp == desired.zero) result <- c(result, i)
		}
	}
	return(result)
}
