# findIndResidualVar: Find the residual variances of indicators if factor loading, total factor covariance, and total indicator variances are specified

findIndResidualVar <- function(lambda, totalFactorCov, totalVarTheta = NULL) {
    ni <- nrow(lambda)
    if (is.null(totalVarTheta)) 
        totalVarTheta <- rep(1, ni)
    factor.part <- lambda %*% totalFactorCov %*% t(lambda)
    error.var <- totalVarTheta - diag(factor.part)
    error.var[(error.var < 0) & (totalVarTheta == 0)] <- 0
    return(as.vector(error.var))
} 
