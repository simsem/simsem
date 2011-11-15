find.indicator.mean <- function(loading, factor.mean = NULL, intercept = NULL) {
	ni <- nrow(loading)
	nk <- ncol(loading)
	if(is.null(factor.mean)) factor.mean <- rep(0, nk)
	if(is.null(intercept)) intercept <- rep(0, ni)
	factor.part <- loading %*% factor.mean
	indicator.mean <- intercept + factor.part
	return(as.vector(indicator.mean))
}
