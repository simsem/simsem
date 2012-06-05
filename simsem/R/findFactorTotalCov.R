# findFactorTotalCov: Find the factor total covariance if regression coefficients and factor covariances (which may be made from
# factor correlation, total factor variances, and error factor variances) are specified

findFactorTotalCov <- function(beta, psi = NULL, corPsi = NULL, totalVarPsi = NULL, errorVarPsi = NULL) {
    if (is.null(psi)) {
        library(lavaan)
        if (is.null(errorVarPsi)) 
            errorVarPsi <- findFactorResidualVar(beta, corPsi, totalVarPsi)
        psi <- suppressWarnings(cor2cov(as.matrix(corPsi), sqrt(errorVarPsi)))
    }
    iden <- diag(nrow(beta))
    facTotalCov <- solve(iden - beta) %*% psi %*% t(solve(iden - beta))
    return(facTotalCov)
} 
