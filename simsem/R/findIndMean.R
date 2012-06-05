# findIndMean: Find indicator means based on loading matrix, factor means, and measurement intercept.

findIndMean <- function(lambda, factorMean = NULL, tau = NULL) {
    ni <- nrow(lambda)
    nk <- ncol(lambda)
    if (is.null(factorMean)) 
        factorMean <- rep(0, nk)
    if (is.null(tau)) 
        tau <- rep(0, ni)
    factor.part <- lambda %*% factorMean
    indicator.mean <- tau + factor.part
    return(as.vector(indicator.mean))
} 
