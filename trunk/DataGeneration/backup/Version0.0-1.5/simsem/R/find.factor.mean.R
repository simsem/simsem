find.factor.mean <- function(path.matrix, intercept = NULL) {
	ni <- nrow(path.matrix)
	set <- find.recursive.set(path.matrix)
	factor.mean <- rep(0, ni)
	if(is.null(intercept)) intercept <- rep(0, ni)
	factor.mean[set[[1]]] <- intercept[set[[1]]]
	iv <- NULL
	iv.mean <- factor.mean[set[[1]]]
	for(i in 1:(length(set) - 1)) {
		iv <- c(iv, set[[i]])
		dv <- set[[i + 1]]
		temp.path <- matrix(path.matrix[dv, iv], nrow = length(dv), ncol = length(iv))
		mean.reg <- (temp.path %*% iv.mean)
		factor.mean[dv] <- intercept[dv] + mean.reg
		if(i < (length(set) - 1)) {
			agg <- c(iv, dv)
			iv.mean <- factor.mean[agg]
		}
	}
	return(as.vector(factor.mean))
}
