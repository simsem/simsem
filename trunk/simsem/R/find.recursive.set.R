findRecursiveSet <- function(square.matrix) {
	result <- list()
	ni <- nrow(square.matrix)
	fix.variable <- rep(FALSE, ni)
	ni.sofar <- 0
	i <- 1
	while(ni.sofar < ni) {
		temp <- findRowZero(square.matrix, fix.variable)
		if(is.null(temp)) stop("The matrix is not recursive.")
		fix.variable[temp] <- TRUE
		result[[i]] <- temp
		i <- i + 1
		ni.sofar <- ni.sofar + length(temp)
	}
	return(result)
}
