averageMisfit <- function(observed.M, observed.CM, implied.M, implied.CM, degree.of.freedom) { 
#Should be renamed to average discrepancy; df is changed to added information
	result <- NULL
	p <- length(observed.M)
	inv <- solve(implied.CM)
	dis.CM <- observed.CM %*% inv
	t.1 <- sum(diag(dis.CM))
	t.1.1 <- det(dis.CM)
	if(t.1.1 > 0) {
		t.2 <- log(t.1.1)
		dis.M <- as.matrix(observed.M - implied.M)
		t.3 <- t(dis.M) %*% inv %*% dis.M
		F.statistic <- t.1 - t.2 - p + t.3
		result <- sqrt(F.statistic/degree.of.freedom)
	}
	return(result)
}
