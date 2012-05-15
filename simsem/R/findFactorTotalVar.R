# findFactorTotalVar: Find the factor total variance if regression coefficients, factor correlation, and factor residual variances are specified.

findFactorTotalVar <- function(beta, corPsi, residualVarPsi) {
    library(lavaan)
    ni <- nrow(beta)
    set <- findRecursiveSet(beta)
    real.psi <- suppressWarnings(cor2cov(as.matrix(corPsi), sqrt(residualVarPsi)))
    ID <- matrix(0, ni, ni)
    diag(ID) <- 1
    iv.cov <- solve(ID - beta) %*% real.psi %*% t(solve(ID - beta))
    factor.var <- diag(iv.cov)
    return(as.vector(factor.var))
} 
