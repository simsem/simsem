isCorMatrix <- function(matrixA) {
	if(dim(matrixA)[1] != dim(matrixA)[2]) {
		return(FALSE)
	} else if(sum(is.na(diag(matrixA))) > 0) {
		return(FALSE)
	} else {
		result <- TRUE
		for(i in 1:dim(matrixA)[1]) {
			if(matrixA[i, i] != 1) result <- FALSE
		}
		return(result)
	}
}
