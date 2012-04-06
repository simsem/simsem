findMeasurementIntercept <- function(loading, factor.mean = NULL, indicator.mean = NULL) {
	ni <- nrow(loading)
	nk <- ncol(loading)
	if(is.null(factor.mean)) factor.mean <- rep(0, nk)
	if(is.null(indicator.mean)) indicator.mean <- rep(0, ni)
	factor.part <- loading %*% factor.mean
	intercept <- indicator.mean - factor.part
	return(as.vector(intercept))
}
