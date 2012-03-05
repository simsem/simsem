find.measurement.error.var <- function(loading, latent.cor, indicator.var = NULL, factor.var = NULL) {
	#browser()
	if(sum(diag(latent.cor)) == 0) diag(latent.cor) <- 1
	ni <- nrow(loading)
	nk <- ncol(loading)
	if(is.null(factor.var)) factor.var <- rep(1, nk)
	if(is.null(indicator.var)) indicator.var <- rep(1, ni)
	factor.cov <- cor2cov(latent.cor, sqrt(factor.var))
	factor.part <- loading %*% factor.cov %*% t(loading)
	error.var <- indicator.var - diag(factor.part)
	error.var[(error.var < 0) & (indicator.var == 0)] <- 0
	return(as.vector(error.var))
}
