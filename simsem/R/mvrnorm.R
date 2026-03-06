### Sunthud Pornprasertmanit 
### Last updated: 6 March 2026
### Internal utilities for generating covariance and correlation structures

#' Simulate from a Multivariate Normal Distribution
#'
#' A modified version of \code{\link[MASS]{mvrnorm}} from the MASS package
#' (Venables and Ripley, 2002). The modification ensures that when the
#' random seed is reset using \code{set.seed()}, the first \code{k} rows
#' of a sample of size \code{n} will match the first \code{k} rows of a
#' larger sample drawn from the same distribution.
#'
#' This behavior facilitates reproducibility when comparing datasets of
#' different sample sizes.
#'
#' @param n Number of observations to generate.
#' @param mu Vector of means.
#' @param Sigma Positive-definite covariance matrix.
#' @param tol Tolerance for detecting non-positive definiteness.
#' @param empirical Logical; if \code{TRUE}, \code{mu} and \code{Sigma}
#'   specify empirical moments.
#' @param EISPACK Logical; reproduce results from older MASS versions.
#'
#' @return A matrix of simulated observations.
#'
#' @references
#' Venables, W. N., & Ripley, B. D. (2002).
#' \emph{Modern Applied Statistics with S}. Fourth Edition.
#' Springer, New York.
#'
#' @author
#' Ripley, B. D. with revision by Paul E. Johnson
#'
#' @keywords internal
#' @importFrom stats rnorm
mvrnorm <-
    function(n = 1, mu, Sigma, tol=1e-6, empirical = FALSE, EISPACK = FALSE)
{
    p <- length(mu)
    if(!all(dim(Sigma) == c(p,p))) stop("incompatible arguments")
    if (missing(EISPACK)) EISPACK <- getOption("mvnorm_use_EISPACK", FALSE)
    eS <- eigen(Sigma, symmetric = TRUE, EISPACK = EISPACK)
    ev <- eS$values
    if(!all(ev >= -tol*abs(ev[1L]))) stop("'Sigma' is not positive definite")
    X <- matrix(rnorm(p * n), n, byrow = TRUE)
    if(empirical) {
        X <- scale(X, TRUE, FALSE) # remove means
        X <- X %*% svd(X, nu = 0)$v # rotate to PCs
        X <- scale(X, FALSE, TRUE) # rescale PCs to unit variance
    }
    X <- drop(mu) + eS$vectors %*% diag(sqrt(pmax(ev, 0)), p) %*% t(X)
    nm <- names(mu)
    if(is.null(nm) && !is.null(dn <- dimnames(Sigma))) nm <- dn[[1L]]
    dimnames(X) <- list(nm, NULL)
    if(n == 1) drop(X) else t(X)
}

#' Convert vech to a Correlation Matrix
#'
#' Converts a vector containing the strictly lower triangular values
#' of a correlation matrix into a full symmetric correlation matrix.
#'
#' @param vech Vector of lower-triangular correlation values.
#'
#' @return A correlation matrix with ones on the diagonal.
#'
#' @examples
#' v <- c(0.1, 0.4, -0.9)
#' vech2Corr(v)
#'
#' v <- c(0.1, 0.4, -0.9, 0.4, 0.5, 0.1)
#' vech2Corr(v)
#'
#' @keywords internal
vech2Corr <- function(vech) {
    ##compute number of rows from vech. diag not in the vech!
    n = (sqrt(1 + 8 * length(vech)) + 1)/2
    if (!as.integer(n) == n) stop(deparse(substitute(vech)), " must have the correct number of elelemnts to fill in a strictly lower triangle in a square matrix.")
    if(any(vech > 1 | vech < -1)) stop("All values in ", deparse(substitute(vech)), " must be in the interval [-1,1]")
    X <- matrix(NA, nrow = n, ncol = n)
    X[lower.tri(X, diag = FALSE)] <- vech
    X[upper.tri(X)] <- t(X)[upper.tri(X)]
    diag(X) <- 1
    X
}

#' Create a Covariance Matrix from Correlations and Standard Deviations
#'
#' Constructs a covariance matrix from correlation and standard deviation
#' information supplied in flexible formats.
#'
#' @param Rho Correlation specification.
#' @param Sd Standard deviations.
#' @param d Optional dimension.
#'
#' @return Covariance matrix.
#'
#' @author Paul Johnson
#'
#' @keywords internal
lazyCov <- function(Rho, Sd, d) {
    if (missing(Sd)) stop("lazyCov requires user to specify either a vector or a single common value for all standard deviations")
    if (missing(Rho)) stop("lazyCov requires a symmstric correlation matrix or enough information to create one, either a vech of lower triangular values or a single common correlation value")
    if (!missing(d) && (length(Sd) > 1) && (length(Sd) != d)) stop("lazyCov doesn't require a d argument, but if you provide one, it must be consistent with the length of a supplied Sd vector")
    if (missing(d)){
        if (length(Sd) > 1) d <- length(Sd)
        else if (is.matrix(Rho)) d <- NROW(Rho)
        else if (is.vector(Rho)) {
            d <- (sqrt(1 + 8 * length(Rho)) + 1)/2
            if (!isTRUE(all.equal(as.integer(d)- d, 0))) stop(deparse(substitute(vech)), " must have the correct number of elelemnts to fill in a strictly lower triangle in a square matrix.")
        }
    }
    if (length(Sd) == 1) Sd <- rep(Sd, d)
    Rho <- lazyCor(Rho, d)

    covMat <- diag(Sd) %*% Rho %*% diag(Sd)
    covMat
}

#' Create a Correlation Matrix
#'
#' Generates a correlation matrix from flexible inputs:
#' a scalar correlation, a vech vector, or a full matrix.
#'
#' @param X Correlation specification.
#' @param d Optional dimension.
#'
#' @return Correlation matrix.
#'
#' @author Paul Johnson
#'
#' @keywords internal
lazyCor <- function(X, d) {
    if (is.matrix(X)){
        stopifnot (isSymmetric(X))
        if (!dim(X)[1] == d) stop("lazyCor: the dimension of the matrix supplied is inconsistent with the dimension argument d")
    } else if (length(X) == 1) {
        if ( X < -1 | X > 1 ) stop(paste("The value of of a correlation should be in [-1,1]"))
        X <- matrix(X, nrow = d, ncol = d)
        diag(X) <- 1.0
    } else if (is.vector(X)){
        X <- vech2Corr(X)
    } else {
        stop(paste("lazyCor cannot understand the value supplied for argument", deparse(substitute(X)),".\n That should be either a", d, " x ", d, "symmetric matrix, \n or a vech of the strictly lower triangular part of a matrix, or \n one single value, which we will use to fill up a matrix."))
    }
    X
}

