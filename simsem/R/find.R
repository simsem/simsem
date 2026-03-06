### Sunthud Pornprasertmanit, Terrence D. Jorgensen, & Patrick Miller
### Last updated: 6 March 2026
### Parameter calculation utilities for simsem

#' Find factor intercepts
#'
#' Computes factor intercepts implied by regression coefficients and
#' factor means.
#'
#' @param beta Regression coefficient matrix among latent variables.
#' @param factorMean Vector of factor means.
#' @param gamma Optional matrix of regression coefficients from covariates
#' to latent variables.
#' @param covmean Mean vector of covariates.
#'
#' @return A numeric vector containing factor intercepts.
#'
#' @export
findFactorIntercept <- function(beta, factorMean = NULL,
                                gamma = NULL, covmean = NULL) {
	if (!is.null(gamma)) {
	  if (is.null(factorMean)) factorMean <- rep(0, nrow(beta))
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
	if (!is.null(gamma)) {
		intercept <- intercept[(length(covmean) + 1):length(intercept)]
	}
    return(as.vector(intercept))
}


#' Find factor residual variances
#'
#' Computes residual variances of latent variables when total variances,
#' correlations, and regression coefficients are specified.
#'
#' @param beta Regression coefficient matrix among latent variables.
#' @param corPsi Correlation matrix of latent variables.
#' @param totalVarPsi Total variances of latent variables.
#' @param gamma Optional regression coefficients from covariates.
#' @param covcov Covariance matrix of covariates.
#'
#' @return A numeric vector of residual variances.
#'
#' @export
findFactorResidualVar <- function(beta, corPsi, totalVarPsi = NULL,
                                  gamma = NULL, covcov = NULL) {
	if (!is.null(gamma)) {
	  if (is.null(totalVarPsi)) totalVarPsi <- rep(1, nrow(beta))
	  beta <- parseGammaToBeta(beta, gamma)
		corPsi <- parseCovCovToPsi(corPsi, cov2cor(covcov))
		totalVarPsi <- c(diag(covcov), totalVarPsi)
	}
    if(all(diag(corPsi) == 0)) diag(corPsi) <- 1
    ni <- nrow(beta)
    set <- findRecursiveSet(beta)
    errorVar <- rep(1, ni)
    if (is.null(totalVarPsi))  totalVarPsi <- rep(1, ni)
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
	if (!is.null(gamma)) {
		errorVar <- errorVar[(nrow(covcov) + 1):length(errorVar)]
	}
    return(as.vector(errorVar))
}


#' Find factor total variances
#'
#' Computes total variances of latent variables implied by regression
#' coefficients and latent residual variances.
#'
#' @param beta Regression coefficient matrix among latent variables.
#' @param corPsi Correlation matrix of latent variables.
#' @param residualVarPsi Residual variances of latent variables.
#' @param gamma Optional regression coefficients from covariates.
#' @param covcov Covariance matrix of covariates.
#'
#' @return Numeric vector of total latent variances.
#'
#' @export
findFactorTotalVar <- function(beta, corPsi, residualVarPsi,
                               gamma = NULL, covcov = NULL) {
	if (!is.null(gamma)) {
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
	if (!is.null(gamma)) {
		factor.var <- factor.var[(nrow(covcov) + 1):length(factor.var)]
	}
    return(as.vector(factor.var))
}


#' Find factor means
#'
#' Computes latent variable means implied by regression coefficients
#' and factor intercepts.
#'
#' @param beta Regression coefficient matrix among latent variables.
#' @param alpha Factor intercepts.
#' @param gamma Optional regression coefficients from covariates.
#' @param covmean Mean vector of covariates.
#'
#' @return Numeric vector of latent variable means.
#'
#' @export
findFactorMean <- function(beta, alpha = NULL,
                           gamma = NULL, covmean = NULL) {
	if (!is.null(gamma)) {
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
	if (!is.null(gamma)) {
		factor.mean <- factor.mean[(length(covmean) + 1):length(factor.mean)]
	}
    return(as.vector(factor.mean))
}


#' Find factor total covariance
#'
#' Computes the total covariance matrix of latent variables implied by
#' regression coefficients and latent residual covariance structure..
#'
#' @param beta Regression coefficient matrix among latent variables.
#' @param psi Residual covariance matrix of latent variables.
#' @param corPsi Correlation matrix of latent variables.
#' @param totalVarPsi Total variances of latent variables.
#' @param errorVarPsi Residual variances of latent variables.
#' @param gamma Optional regression coefficients from covariates.
#' @param covcov Covariance matrix of covariates.
#'
#' @return Covariance matrix of latent variables.
#'
#' @export
findFactorTotalCov <- function(beta, psi = NULL, corPsi = NULL,
                               totalVarPsi = NULL, errorVarPsi = NULL,
                               gamma = NULL, covcov = NULL) {
  if (is.null(psi)) {
    if (is.null(errorVarPsi))
       errorVarPsi <- findFactorResidualVar(beta, corPsi, totalVarPsi)
    psi <- suppressWarnings(cor2cov(as.matrix(corPsi), sqrt(errorVarPsi)))
  }
  iden <- diag(nrow(beta))
	temp <- solve(iden - beta)
    facTotalCov <- temp %*% psi %*% t(temp)
	if (!is.null(gamma)) {
		facTotalCov <- facTotalCov + (temp %*% gamma %*% covcov %*% t(gamma) %*% t(temp))
	}
  return(facTotalCov)
}


#' Find indicator total variances
#'
#' Computes total variances of indicators implied by factor loadings,
#' factor covariance, and residual variances.
#'
#' @param lambda Factor loading matrix.
#' @param totalFactorCov Total covariance matrix of latent variables.
#' @param residualVarTheta Residual variances of indicators.
#' @param kappa Optional regression coefficients from covariates.
#' @param covcov Covariance matrix of covariates.
#'
#' @return Numeric vector of indicator variances.
#'
#' @export
findIndTotalVar <- function(lambda, totalFactorCov, residualVarTheta,
                            kappa = NULL, covcov = NULL) {
    factor.part <- lambda %*% totalFactorCov %*% t(lambda)
    indicator.var <- diag(factor.part) + residualVarTheta

	if (!is.null(kappa)) indicator.var <- indicator.var + diag(kappa %*% covcov %*% t(kappa))
  return(as.vector(indicator.var))
}


#' Find indicator intercepts
#'
#' Computes measurement intercepts implied by factor loadings,
#' factor means, and indicator means.
#'
#' @param lambda Factor loading matrix.
#' @param factorMean Latent variable means.
#' @param indicatorMean Indicator means.
#' @param kappa Optional regression coefficients from covariates.
#' @param covmean Mean vector of covariates.
#'
#' @return Numeric vector of indicator intercepts.
#'
#' @export
findIndIntercept <- function(lambda, factorMean = NULL, indicatorMean = NULL,
                             kappa = NULL, covmean = NULL) {
  ni <- nrow(lambda)
  nk <- ncol(lambda)
  if (is.null(factorMean)) factorMean <- rep(0, nk)
  if (is.null(indicatorMean)) indicatorMean <- rep(0, ni)
  factor.part <- lambda %*% factorMean
  intercept <- indicatorMean - factor.part
	if (!is.null(kappa)) intercept <- intercept - (kappa %*% covmean)
  return(as.vector(intercept))
}


#' Find indicator residual variances
#'
#' Computes residual variances of indicators implied by factor loadings
#' and total indicator variances.
#'
#' @param lambda Factor loading matrix.
#' @param totalFactorCov Covariance matrix of latent variables.
#' @param totalVarTheta Total indicator variances.
#' @param kappa Optional regression coefficients from covariates.
#' @param covcov Covariance matrix of covariates.
#'
#' @return Numeric vector of residual variances.
#'
#' @export
findIndResidualVar <- function(lambda, totalFactorCov, totalVarTheta = NULL,
                               kappa = NULL, covcov = NULL) {
  ni <- nrow(lambda)
  if (is.null(totalVarTheta)) totalVarTheta <- rep(1, ni)
  factor.part <- lambda %*% totalFactorCov %*% t(lambda)
  error.var <- totalVarTheta - diag(factor.part)
	if (!is.null(kappa)) error.var <- error.var - diag(kappa %*% covcov %*% t(kappa))
  error.var[(error.var < 0) & (totalVarTheta == 0)] <- 0
  return(as.vector(error.var))
}


#' Find indicator means
#'
#' Computes means of observed indicators implied by factor loadings,
#' factor means, and measurement intercepts.
#'
#' @param lambda Factor loading matrix.
#' @param factorMean Latent variable means.
#' @param tau Measurement intercepts.
#' @param kappa Optional regression coefficients from covariates.
#' @param covmean Mean vector of covariates.
#'
#' @return Numeric vector of indicator means.
#'
#' @export
findIndMean <- function(lambda, factorMean = NULL, tau = NULL,
                        kappa = NULL, covmean = NULL) {
  ni <- nrow(lambda)
  nk <- ncol(lambda)
  if (is.null(factorMean)) factorMean <- rep(0, nk)
  if (is.null(tau)) tau <- rep(0, ni)
  factor.part <- lambda %*% factorMean
  indicator.mean <- tau + factor.part
	if (!is.null(kappa)) indicator.mean <- indicator.mean + (kappa %*% covmean)
  return(as.vector(indicator.mean))
}

#' Find possible factor correlations
#'
#' Determines which factor correlations may be freely estimated based on
#' the regression structure among latent variables.
#'
#' @param beta Regression coefficient matrix among latent variables.
#'
#' @return A matrix indicating which factor correlations may be freely estimated.
#'
#' @export
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

#' Find recursive variable sets
#'
#' Groups variables according to their position in a recursive structural model 
#' defined by the regression coefficient matrix.
#'
#' @param beta Regression coefficient matrix.
#'
#' @return A list of variable indices representing recursive levels.
#'
#' @export
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

#' Find rows with zero elements
#'
#' Identifies rows of a square matrix that contain only zeros in the
#' non-fixed subset of columns.
#'
#' @param square.matrix A square matrix.
#' @param is.row.fixed Logical vector indicating rows that should be
#' skipped when checking for zeros.
#'
#' @return Integer vector of row indices containing only zeros.
#'
#' @keywords internal
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
