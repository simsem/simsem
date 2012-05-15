# findIndTotalVar: Find indicator total variances based on loading matrix, total factor covariance, and measurement error variances.

findIndTotalVar <- function(lambda, totalFactorCov, residualVarTheta) {
    factor.part <- lambda %*% totalFactorCov %*% t(lambda)
    indicator.var <- diag(factor.part) + residualVarTheta
    return(as.vector(indicator.var))
} 
