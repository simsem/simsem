find.factor.var <- function(path.matrix, latent.cor.matrix, error.var) {
	ni <- nrow(path.matrix)
	set <- find.recursive.set(path.matrix)
	real.psi <- cor2cov(latent.cor.matrix, sqrt(error.var))
	ID <- matrix(0, ni, ni)
	diag(ID) <- 1
	iv.cov <- solve(ID - path.matrix) %*% real.psi %*% t(solve(ID - path.matrix))
	factor.var <- diag(iv.cov)
	return(as.vector(factor.var))
}
