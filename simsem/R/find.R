


# findFactorIntercept: Find the factor intercept if regression coefficients and
# factor means are specified

findFactorIntercept <- function(beta, factorMean = NULL, gamma = NULL, covmean = NULL) {
	if(!is.null(gamma)) {
		beta <- parseGammaToBeta(beta, gamma)
		factorMean <- c(covmean, factorMean)
	}
    ni <- nrow(beta)
    set <- findRecursiveSet(beta)
    intercept <- rep(0, ni)
    if (is.null(factorMean))
        factorMean <- rep(0, ni)
    intercept[set[[1]]] <- factorMean[set[[1]]]
    iv <- NULL
    iv.mean <- factorMean[set[[1]]]
    for (i in seq_len(length(set) - 1)) {
        iv <- c(iv, set[[i]])
        dv <- set[[i + 1]]
        temp.path <- matrix(beta[dv, iv], nrow = length(dv), ncol = length(iv))
        mean.reg <- (temp.path %*% iv.mean)
        dv.mean <- factorMean[dv]
        intercept[dv] <- dv.mean - mean.reg
        if (i < (length(set) - 1)) {
            agg <- c(iv, dv)
            iv.mean <- factorMean[agg]
        }
    }
	if(!is.null(gamma)) {
		intercept <- intercept[(length(covmean) + 1):length(intercept)]
	}
    return(as.vector(intercept))
}

# findFactorResidualVar: Find the factor residual variance if total variances,
# correlation, and regression coefficients are specified.

findFactorResidualVar <- function(beta, corPsi, totalVarPsi = NULL, gamma = NULL, covcov = NULL) {
	if(!is.null(gamma)) {
		beta <- parseGammaToBeta(beta, gamma)
		corPsi <- parseCovCovToPsi(corPsi, cov2cor(covcov))
		totalVarPsi <- c(diag(covcov), totalVarPsi)
	}
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
    ivCov <- suppressWarnings(cor2cov(as.matrix(ivCor), sqrt(startVar)))
    for (i in seq_len(length(set) - 1)) {
        iv <- c(iv, set[[i]])
        dv <- set[[i + 1]]
        tempBeta <- matrix(beta[dv, iv], nrow = length(dv), ncol = length(iv))
        var.reg <- (tempBeta %*% ivCov %*% t(tempBeta))
        tempPsi <- corPsi[dv, dv]
        tempPsiSd <- rep(0, length(dv))
        for (j in 1:length(dv)) {
            errorVar[dv[j]] <- totalVarPsi[dv[j]] - var.reg[j, j]
            if (is.na(errorVar[dv[j]]) || errorVar[dv[j]] < 0) {
                tempPsiSd[j] <- NaN
            } else {
                tempPsiSd[j] <- sqrt(errorVar[dv[j]])
            }
        }
        if (i < (length(set) - 1)) {
            tempPsi <- suppressWarnings(cor2cov(as.matrix(tempPsi), tempPsiSd))
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
	if(!is.null(gamma)) {
		errorVar <- errorVar[(nrow(covcov) + 1):length(errorVar)]
	}
    return(as.vector(errorVar))
}

# findFactorTotalVar: Find the factor total variance if regression
# coefficients, factor correlation, and factor residual variances are
# specified.

findFactorTotalVar <- function(beta, corPsi, residualVarPsi, gamma = NULL, covcov = NULL) {
	if(!is.null(gamma)) {
		beta <- parseGammaToBeta(beta, gamma)
		corPsi <- parseCovCovToPsi(corPsi, cov2cor(covcov))
		residualVarPsi <- c(diag(covcov), residualVarPsi)
	}
    ni <- nrow(beta)
    set <- findRecursiveSet(beta)
    real.psi <- suppressWarnings(cor2cov(as.matrix(corPsi), sqrt(residualVarPsi)))
    ID <- matrix(0, ni, ni)
    diag(ID) <- 1
    iv.cov <- solve(ID - beta) %*% real.psi %*% t(solve(ID - beta))
    factor.var <- diag(iv.cov)
	if(!is.null(gamma)) {
		factor.var <- factor.var[(nrow(covcov) + 1):length(factor.var)]
	}
    return(as.vector(factor.var))
}

# findFactorMean: Find the factor mean if regression coefficients and factor
# intercept are specified.

findFactorMean <- function(beta, alpha = NULL, gamma = NULL, covmean = NULL) {
	if(!is.null(gamma)) {
		beta <- parseGammaToBeta(beta, gamma)
		alpha <- c(covmean, alpha)
	}
    ni <- nrow(beta)
    set <- findRecursiveSet(beta)
    factor.mean <- rep(0, ni)
    if (is.null(alpha)) 
        alpha <- rep(0, ni)
    factor.mean[set[[1]]] <- alpha[set[[1]]]
    iv <- NULL
    iv.mean <- factor.mean[set[[1]]]
    for (i in seq_len(length(set) - 1)) {
        iv <- c(iv, set[[i]])
        dv <- set[[i + 1]]
        temp.path <- matrix(beta[dv, iv], nrow = length(dv), ncol = length(iv))
        mean.reg <- (temp.path %*% iv.mean)
        factor.mean[dv] <- alpha[dv] + mean.reg
        if (i < (length(set) - 1)) {
            agg <- c(iv, dv)
            iv.mean <- factor.mean[agg]
        }
    }
	if(!is.null(gamma)) {
		factor.mean <- factor.mean[(length(covmean) + 1):length(factor.mean)]
	}
    return(as.vector(factor.mean))
}

# findFactorTotalCov: Find the factor total covariance if regression
# coefficients and factor covariances (which may be made from factor
# correlation, total factor variances, and error factor variances) are
# specified

findFactorTotalCov <- function(beta, psi = NULL, corPsi = NULL, totalVarPsi = NULL, 
    errorVarPsi = NULL, gamma = NULL, covcov = NULL) {
    if (is.null(psi)) {
        if (is.null(errorVarPsi)) 
            errorVarPsi <- findFactorResidualVar(beta, corPsi, totalVarPsi)
        psi <- suppressWarnings(cor2cov(as.matrix(corPsi), sqrt(errorVarPsi)))
    }
    iden <- diag(nrow(beta))
	temp <- solve(iden - beta)
    facTotalCov <- temp %*% psi %*% t(temp)
	if(!is.null(gamma)) {
		facTotalCov <- facTotalCov + (temp %*% gamma %*% covcov %*% t(gamma) %*% t(temp))
	}
    return(facTotalCov)
}

# findIndTotalVar: Find indicator total variances based on loading matrix,
# total factor covariance, and measurement error variances.
findIndTotalVar <- function(lambda, totalFactorCov, residualVarTheta, kappa = NULL, covcov = NULL) {
    factor.part <- lambda %*% totalFactorCov %*% t(lambda)
    indicator.var <- diag(factor.part) + residualVarTheta
	
	if(!is.null(kappa)) indicator.var <- indicator.var + diag(kappa %*% covcov %*% t(kappa))
    return(as.vector(indicator.var))
}

# findIndIntercept: Find the measurement intercept if factor loading, total
# factor covariance, and total indicator variances are specified

findIndIntercept <- function(lambda, factorMean = NULL, indicatorMean = NULL, kappa = NULL, covmean = NULL) {
    ni <- nrow(lambda)
    nk <- ncol(lambda)
    if (is.null(factorMean)) 
        factorMean <- rep(0, nk)
    if (is.null(indicatorMean)) 
        indicatorMean <- rep(0, ni)
    factor.part <- lambda %*% factorMean
    intercept <- indicatorMean - factor.part
	if(!is.null(kappa)) intercept <- intercept - (kappa %*% covmean)
    return(as.vector(intercept))
}

# findIndResidualVar: Find the residual variances of indicators if factor
# loading, total factor covariance, and total indicator variances are specified

findIndResidualVar <- function(lambda, totalFactorCov, totalVarTheta = NULL, kappa = NULL, covcov = NULL) {
    ni <- nrow(lambda)
    if (is.null(totalVarTheta)) 
        totalVarTheta <- rep(1, ni)
    factor.part <- lambda %*% totalFactorCov %*% t(lambda)
    error.var <- totalVarTheta - diag(factor.part)
	if(!is.null(kappa)) error.var <- error.var - diag(kappa %*% covcov %*% t(kappa))
    error.var[(error.var < 0) & (totalVarTheta == 0)] <- 0
    return(as.vector(error.var))
}

# findIndMean: Find indicator means based on loading matrix, factor means, and
# measurement intercept.

findIndMean <- function(lambda, factorMean = NULL, tau = NULL, kappa = NULL, covmean = NULL) {
    ni <- nrow(lambda)
    nk <- ncol(lambda)
    if (is.null(factorMean)) 
        factorMean <- rep(0, nk)
    if (is.null(tau)) 
        tau <- rep(0, ni)
    factor.part <- lambda %*% factorMean
    indicator.mean <- tau + factor.part
	if(!is.null(kappa)) indicator.mean <- indicator.mean + (kappa %*% covmean)
    return(as.vector(indicator.mean))
}

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

# findRecursiveSet: Group variables together regarding the position in the
# mediation chain

findRecursiveSet <- function(beta) {
    result <- list()
    ni <- nrow(beta)
    fix.variable <- rep(FALSE, ni)
    ni.sofar <- 0
    i <- 1
    while (ni.sofar < ni) {
        temp <- findRowZero(beta, fix.variable)
        if (is.null(temp)) 
            stop("The matrix is not recursive.")
        fix.variable[temp] <- TRUE
        result[[i]] <- temp
        i <- i + 1
        ni.sofar <- ni.sofar + length(temp)
    }
    return(result)
}

# \title{
	# Find rows in a matrix that all elements are zero in non-fixed subset rows and columns. 
# }
# \description{
	# Find rows in a matrix that all elements are zero in non-fixed subset rows and columns. This function will be used in the \code{\link{findRecursiveSet}} function
# }
# \usage{
# findRowZero(square.matrix, is.row.fixed = FALSE)
# }
# \arguments{
  # \item{square.matrix}{
	# Any square matrix
# }
  # \item{is.row.fixed}{
	# A logical vector with the length equal to the dimension of the \code{square.matrix}. If \code{TRUE}, the function will skip examining this row.
# }
# }
# \value{
	# A vector of positions that contain rows of all zeros
# }

findRowZero <- function(square.matrix, is.row.fixed = FALSE) {
    ni <- nrow(square.matrix)
    if (length(is.row.fixed) == 1) {
        if (is.row.fixed == FALSE) 
            is.row.fixed <- rep(FALSE, ni)
    }
    result <- NULL
    desired.zero <- sum(!is.row.fixed)
    for (i in 1:ni) {
        if (is.row.fixed[i] == FALSE) {
            temp <- sum(square.matrix[i, !is.row.fixed] == 0, na.rm = TRUE)
            if (temp == desired.zero) 
                result <- c(result, i)
        }
    }
    return(result)
} 
