# findFactorResidualVar: Find the factor residual variance if total variances,
# correlation, and regression coefficients are specified.

findFactorResidualVar <- function(beta, corPsi, totalVarPsi = NULL) {
    library(lavaan)
    if (sum(diag(corPsi)) == 0) 
        diag(corPsi) <- 1
    ni <- nrow(beta)
    set <- findRecursiveSet(beta)
    errorVar <- rep(1, ni)
    if (is.null(totalVarPsi)) 
        totalVarPsi <- rep(1, ni)
    errorVar[set[[1]]] <- totalVarPsi[set[[1]]]
    iv <- NULL
    ivCor <- corPsi[set[[1]], set[[1]]]
    startVar <- totalVarPsi[set[[1]]]
    ivCov <- cor2cov(as.matrix(ivCor), sqrt(startVar))
    for (i in 1:(length(set) - 1)) {
        iv <- c(iv, set[[i]])
        dv <- set[[i + 1]]
        tempBeta <- matrix(beta[dv, iv], nrow = length(dv), ncol = length(iv))
        var.reg <- (tempBeta %*% ivCov %*% t(tempBeta))
        tempPsi <- corPsi[dv, dv]
        tempPsiSd <- rep(0, length(dv))
        for (j in 1:length(dv)) {
            errorVar[dv[j]] <- totalVarPsi[dv[j]] - var.reg[j, j]
            if (errorVar[dv[j]] < 0) {
                tempPsiSd[j] <- NaN
            } else {
                tempPsiSd[j] <- sqrt(errorVar[dv[j]])
            }
        }
        if (i < (length(set) - 1)) {
            tempPsi <- cor2cov(tempPsi, tempPsiSd)
            real.tempPsi <- matrix(0, length(iv) + length(dv), length(iv) + length(dv))
            real.tempPsi[1:length(iv), 1:length(iv)] <- ivCov
            real.tempPsi[(length(iv) + 1):(length(iv) + length(dv)), (length(iv) + 
                1):(length(iv) + length(dv))] <- tempPsi
            agg <- c(iv, dv)
            blank.path <- matrix(0, nrow = length(iv), ncol = length(agg))
            temp.path2 <- beta[dv, agg]
            temp.path2 <- rbind(blank.path, temp.path2)
            ID <- matrix(0, length(agg), length(agg))
            diag(ID) <- 1
            ivCov <- solve(ID - temp.path2) %*% real.tempPsi %*% t(solve(ID - temp.path2))
        }
    }
    return(as.vector(errorVar))
} 
