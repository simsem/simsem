find.indicator.var <- function(loading, latent.cor, error.var, factor.var = NULL) {
	ni <- nrow(loading)
	nk <- ncol(loading)
	if(is.null(factor.var)) factor.var <- rep(1, nk)
	factor.cov <- cor2cov(latent.cor, sqrt(factor.var))
	factor.part <- loading %*% factor.cov %*% t(loading)
	indicator.var <- diag(factor.part) + error.var
	return(as.vector(indicator.var))
}
