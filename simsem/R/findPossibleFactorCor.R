# findPossibleFactorCor: From the set of regression coefficients, this function
# will find the elements that is possible to free covariances or correlations

findPossibleFactorCor <- function(beta) {
    ni <- nrow(beta)
    set <- findRecursiveSet(beta)
    psi <- matrix(0, ni, ni)
    diag(psi) <- 1
    for (i in 1:length(set)) {
        temp.set <- set[[i]]
        if (length(temp.set) > 1) {
            for (j in 2:length(temp.set)) {
                for (k in 1:(j - 1)) {
                  psi[temp.set[j], temp.set[k]] <- NA
                  psi[temp.set[k], temp.set[j]] <- NA
                }
            }
        }
    }
    return(psi)
} 
