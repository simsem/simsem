##' Simulate from a Multivariate Normal Distribution
##'
##' This is the \code{\link[MASS]{mvrnorm}} function from the MASS
##' package (Venables and Ripley, 2002), with one small modification
##' to facilitate replication of random samples of various sizes. The
##' aim is to make replicable the first k rows of data generated from
##' mvrnorm, where k < n. This assumes, of course, that the user runs
##' \code{set.seed} to re-initialize the random generator before each
##' usage of mvrnorm.
##'
##' Users who draw a sample size of n=(N+k) may hope that mvrnorm will
##' produce the exact same observations for the first 1:N rows in the
##' output data when k is adjusted. The version of \code{mvrnorm}
##' provided with MASS does not do so.  After re-setting the seed,
##' this function assures that the rows of the smaller set will match
##' the larger sample up to row N. Draws after N will differ, of
##' course, but in a replicable way, so that one could then draw a
##' sample of size (N + k + k2) and the first (N + k) values will
##' match the previous sample. Please run the example for an
##' illustration.
##'
##' Why is this important?  We are trying to isolate the sources of
##' change between samples. \code{mvrnorm} gives the exact same values
##' for column one up to row (n) when a sample size changes, but it
##' gives different results for the other columns. This causes
##' confusion among researchers, some of whom exect the rows should be
##' the same up to a point, while others expect that each column
##' should be completely replaced each time.
##' @param n the number of samples ("rows" of data) required.
##' @param mu a vector giving the means of the variables.
##' @param Sigma positive-definite symmetric matrix specifying the
##'    covariance matrix of the variables.
##' @param tol tolerance (relative to largest variance) for numerical lack
##'    of positive-definiteness in \code{Sigma}
##' @param empirical logical. If true, mu and Sigma specify the empirical
##'    not population mean and covariance matrix.
##' @param EISPACK logical. Set to true to reproduce results from MASS
##'    versions prior to 3.1-21.
##' @import MASS
##' @export
##' @return If \code{n = 1} a vector of the same length as \code{mu}, otherwise an
##'  \code{n} by \code{length(mu)} matrix with one sample in each row.
##' @author Ripley, B.D. with revision by Paul E. Johnson
##' @references
##' Venables, W. N. & Ripley, B. D. (2002) Modern Applied Statistics with
##' S. Fourth Edition. Springer, New York. ISBN 0-387-95457-0
##' @examples
##'
##' library(portableParallelSeeds)
##' set.seed(12345)
##' X0 <- MASS::mvrnorm(n=10, mu = c(0,0,0), Sigma = diag(3))
##' ## create a smaller data set, starting at same position
##' set.seed(12345)
##' X1 <- MASS::mvrnorm(n=5, mu = c(0,0,0), Sigma = diag(3))
##' ## Create a larger data set
##' set.seed(12345)
##' X2 <- MASS::mvrnorm(n=15, mu = c(0,0,0), Sigma = diag(3))
##' ## The first 5 rows in X0, X1, and X2 are not the same
##' identical(X0[1:5, ], X1[1:5, ])
##' identical(X1[1:5, ], X2[1:5, ])
##' set.seed(12345)
##' Y0 <- mvrnorm(n=10, mu = c(0,0,0), Sigma = diag(3))
##' set.seed(12345)
##' Y1 <- mvrnorm(n=5, mu = c(0,0,0), Sigma = diag(3))
##' set.seed(12345)
##' Y2 <- mvrnorm(n=15, mu = c(0,0,0), Sigma = diag(3))
##' identical(Y0[1:5, ], Y1[1:5, ])
##' identical(Y1[1:5, ], Y2[1:5, ])
##'
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
NULL


##' Convert the vech (column of strictly lower trianglar values from a matrix) into a correlation matrix.
##'
##' vech2Corr is a convenience function for creating correlation matrices
##' from a vector of the lower triangular values. It checks the arguments
##' to make sure they are consistent with the requirements of a
##' correlation matrix. All values must be in [-1, 1], and the number
##' of values specified must be correct for a lower triangle.
##'
##' Use this in combination with the \code{covMat} function to
##' convert a vector of standard deviations and the correlation matrix
##' into a covariance matrix.
##'
##' @export
##' @seealso Similar functions exist in many packages, see  \code{vec2sm} in corpcor, \code{xpnd} in MCMCpack
##' @param vech A vector of values to be placed into the strictly lower triangle of a matrix. All values must be in the [0,1] interval (because they are correlations).
##' @return A symmetric correlation matrix, with 1's on the diagonal.
##' Paul Johnson <pauljohn@@ku.edu>
##' @examples
##' v <- c(0.1, 0.4, -0.9)
##' vech2Corr(v)
##' v <- c(0.1, 0.4, -0.9, 0.4, 0.5, 0.1)
##' vech2Corr(v)
##'
vech2Corr <- function(vech){
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

NULL


NULL

##' Create covariance matrix from correlation and standard deviation information
##'
##' This is a flexible function that allows lazy R programmers to create covariance matrix. The user may be lazy because the correlation and standard deviation infomation may be supplied in a variety of formats.
##'
##' @param Rho Required. May be a single value (correlation common among all variables), a vector of the lower triangular values (vech) of a correlation matrix, or a symmetric matrix of correlation coefficients.
##' @param Sd Required. May be a single value (standard deviation common among all variables) or a vector of standard deviations, one for each variable.
##' @param d Optional. lazyCov will try to manufacture correlation and standard deviation matrices and vectors from minimal information, but the required dimension of the final matrix may be needed when the user supplies only a single value for both Rho and Sd.
##' @return covariance matrix suitable for input into mvrnorm.
##' @author <pauljohn@@ku.edu>
##' @export
##' @examples
##' ##correlation 0.8 for all pairs, standard deviation 1.0 of each
##' lazyCov(Rho = 0.8, Sd = 1.0, d = 3)
##' ## supply a vech (lower triangular values in a column)
##' lazyCov(Rho = c(0.1,0.2,0.3), Sd = 1.0)
##' ## supply vech with different standard deviations
##' lazyCov(Rho = c(0.1,0.2,0.3), Sd = c(1.0, 2.2, 3.3))
##' newRho <- lazyCor(c(0.5,0.6, 0.7, -0.1, -0.5, 0.2))
##' lazyCov(Rho = newRho, Sd = 1.0)
##' lazyCov(Rho = newRho, Sd = c(3,4,5,6))
lazyCov <- function(Rho, Sd, d){
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





##' Create correlation matrices.
##'
##' Use can supply either a single value (the common correlation among
##' all variables), a column of the lower triangular values for a
##' correlation matrix, or a candidate matrix. The function will check
##' X and do the right thing. If X is a matrix, check that it
## is a valid correlation matrix. If its a single value, use that
## to fill up a matrix. If itis a vector, try to use it as a vech
## to fill the lower triangle..
##' @param X Required. May be one value, a vech, or a matrix
##' @param d Optional. The number of rows in the correlation matrix to
##' be created. lazyCor will deduce the desired size from X if
##' possible. If X is a single value, d is a required argument.
##' @return A correlation matrix.
##' @export
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @examples
##' lazyCor(0.5, 8)
##' lazyCor(c(0.1, 0.2, 0.3))
##' lazyCor(c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6))
##' lazyCor(c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.10))
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

