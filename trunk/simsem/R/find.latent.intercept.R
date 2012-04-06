findLatentIntercept <- function(path.matrix, factor.mean = NULL) {
	ni <- nrow(path.matrix)
	set <- findRecursiveSet(path.matrix)
	intercept <- rep(0, ni)
	if(is.null(factor.mean)) factor.mean <- rep(0, ni)
	intercept[set[[1]]] <- factor.mean[set[[1]]]
	iv <- NULL
	iv.mean <- factor.mean[set[[1]]]
	for(i in 1:(length(set) - 1)) {
		iv <- c(iv, set[[i]])
		dv <- set[[i + 1]]
		temp.path <- matrix(path.matrix[dv, iv], nrow = length(dv), ncol = length(iv))
		mean.reg <- (temp.path %*% iv.mean)
		dv.mean <- factor.mean[dv]
		intercept[dv] <- dv.mean - mean.reg
		if(i < (length(set) - 1)) {
			agg <- c(iv, dv)
			iv.mean <- factor.mean[agg]
		}
	}
	return(as.vector(intercept))
}
