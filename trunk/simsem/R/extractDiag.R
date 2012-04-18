# To be deleted. Double diag function can be used.

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