### Sunthud Pornprasertmanit and Patrick Miller; with contributions by Terrence D. Jorgensen
### Last updated: 5 March 2026
### functions for constructing parameter matrices used in simulations

#' Create a simulation parameter matrix or vector
#'
#' Creates a \code{SimMatrix} or \code{SimVector} object describing
#' free parameters, population values, and model misspecification
#' used in simulation studies.
#'
#' The function provides a compact interface for specifying model
#' parameters without manually constructing multiple matrices.
#' Parameters are defined using a free/fixed template in which
#' free parameters are indicated by \code{NA} and fixed parameters
#' are numeric values. Character labels can be used to specify
#' equality constraints.
#'
#' @param free A matrix or vector specifying free and fixed parameters.
#'   \itemize{
#'   \item \code{NA} indicates a free parameter.
#'   \item Numeric values indicate fixed parameters.
#'   \item Character labels indicate equality constraints.
#'   }
#'
#' @param popParam Population parameter values used to generate data.
#'   Elements may be numeric values or expressions defining distributions.
#'
#' @param misspec Optional values used to introduce model misspecification.
#'   These values replace the fixed values specified in \code{free} during
#'   data generation.
#'
#' @param symmetric Logical indicating whether the matrix should be treated
#'   as symmetric.
#'
#' @details
#' The interface allows users to specify model parameters using a single
#' free/fixed template rather than constructing separate matrices for
#' population values and misspecification.
#'
#' Free parameters are indicated by \code{NA}. Fixed parameters are
#' numeric values. Parameters sharing the same character label are
#' constrained to be equal.
#'
#' Population values may be specified either as constants or as
#' expressions defining random distributions. When a single value or
#' distribution is provided, it is applied to all relevant parameters.
#'
#' Misspecification can be introduced by assigning values or
#' distributions to the \code{misspec} argument, which replaces the
#' fixed parameter values during data generation.
#'
#' Several validity checks are performed:
#' \itemize{
#' \item Input matrices must have matching dimensions.
#' \item Character expressions must evaluate correctly.
#' \item Population values must be provided for all free parameters.
#' \item Equality constraint labels must be valid.
#' }
#'
#' @return
#' A \code{SimMatrix} or \code{SimVector} object depending on whether
#' \code{free} is a matrix or vector.
#'
#' @seealso
#' \code{\link{binds}}, \code{\link{SimMatrix-class}}, \code{\link{SimVector-class}}
#'
#' @examples
#' free <- matrix(c(NA, 0.5,
#'                  NA, NA), 2, 2)
#'
#' bind(free, popParam = 0.6)
#'
#' @export
bind <- function(free = NULL, popParam = NULL, misspec = NULL, symmetric = FALSE) {
    ## SimMatrix
    if (is.matrix(free)) {

        if (symmetric) stopifnot(isSymmetric(free))

        ## PopParam Must be either character or numeric
        if (is.character(popParam)) {
            tryCatch(eval(parse(text = popParam)), error = function(e) stop(e))
            if (!is.matrix(popParam)) {
                paramMat <- ifelse(is.free(free), popParam, "")
            }
        } else if (is.numeric(popParam) && !is.matrix(popParam)) {
            paramMat <- ifelse(is.free(free), popParam, "")
        }

        # Can optionally also be a matrix
        if (is.matrix(popParam)) {
            if (symmetric) stopifnot(isSymmetric(popParam))
            if (!all(dim(free) == dim(popParam)))
                stop("Free matrix and popParam are not of same dimension")
			popParam[!is.free(free)] <- ""
            if (any(!is.empty(popParam) != is.free(free))) {
                stop("Please assign a value for any free parameters")
            }
            paramMat <- matrix(as.character(popParam), nrow = nrow(popParam), ncol = ncol(popParam))

        }

        if (is.null(popParam)) {
            paramMat <- matrix(NaN)
        }

        # Misspec - same tests as above, almost.
        if (is.character(misspec)) {
            tryCatch(eval(parse(text = misspec)), error = function(e) stop(e))
            if (!is.matrix(misspec))
                misspecMat <- ifelse(!is.free(free), misspec, "")
        } else if (is.numeric(misspec) && !is.matrix(misspec)) {
            misspecMat <- ifelse(!is.free(free), misspec, "")
        }

        if (is.matrix(misspec)) {
            if (symmetric) stopifnot(isSymmetric(misspec))
            if (!all(dim(free) == dim(misspec)))
                stop("Free matrix and misspec are not of same dimension")
            misspecMat <- matrix(as.character(misspec), nrow = nrow(misspec), ncol = ncol(misspec))

        }
        if (is.null(misspec)) {
            misspecMat <- matrix(NaN)
        }

        ## to prevent errors elsewhere, only use indLab= and facLab=
        dimnames(free) <- NULL
        dimnames(paramMat) <- NULL
        dimnames(misspecMat) <- NULL

        return(new("SimMatrix", free = free, popParam = paramMat, misspec = misspecMat,
            symmetric = symmetric))

        ## SimVector
    } else if (is.vector(free)) {

        if (symmetric) stop("A vector cannot be symmetric")

        # popParam
        if (is.character(popParam) && length(popParam == 1)) {
            tryCatch(eval(parse(text = popParam)), error = function(e) stop(e))
            paramVec <- ifelse(is.free(free), popParam, "")
        } else if (is.numeric(popParam) && length(popParam) == 1) {
            paramVec <- ifelse(is.free(free), popParam, "")
        } else if (is.vector(popParam)) {
            if ((length(free) != length(popParam)) && length(popParam) > 1)
                stop("Free vector and popParam are not the same length")
			popParam[!is.free(free)] <- ""
            if (any(!is.empty(popParam) != is.free(free))) {
                stop("Please assign a value for any free parameters")
            }
            paramVec <- as.character(popParam)
        } else {
            paramVec <- vector()
        }

        # Misspec
        if (is.character(misspec) && length(misspec) == 1) {
            tryCatch(eval(parse(text = misspec)), error = function(e) stop(e))
            misspecVec <- ifelse(!is.free(free), misspec, "")
        } else if (is.numeric(misspec) && length(misspec) == 1) {
            misspecVec <- ifelse(!is.free(free), misspec, "")
        } else if (is.vector(misspec)) {
            if ((length(free) != length(misspec)) && length(misspec) > 1)
                stop("Free vector and misspec are not the same length")
            misspecVec <- as.character(misspec)
        } else {
            misspecVec <- vector()
        }

        ## to prevent errors elsewhere, only use indLab= and facLab=
        names(free) <- NULL
        names(paramVec) <- NULL
        names(misspecVec) <- NULL

        new("SimVector", free = free, popParam = paramVec, misspec = misspecVec)

    } else {
        stop("Please specify a free/fixed parameter matrix or vector.")
    }

}

#' Symmetric version of \code{bind}
#'
#' Convenience wrapper for \code{bind()} that sets
#' \code{symmetric = TRUE}.
#'
#' @inheritParams bind
#'
#' @return
#' A \code{SimMatrix} object.
#'
#' @seealso
#' \code{\link{bind}}
#'
#' @export
binds <- function(free = NULL, popParam = NULL, misspec = NULL, symmetric = TRUE) {
    return(bind(free = free, popParam = popParam, misspec = misspec, symmetric = symmetric))
}


#' Check whether elements are empty
#'
#' Internal helper used to identify empty elements
#' (either \code{""} or \code{NA}).
#'
#' @param dat A vector or matrix.
#'
#' @return A logical object of the same shape indicating
#' whether each element is empty.
#'
#' @keywords internal
is.empty <- function(dat) {
    if (is.null(dim(dat))) {
        temp <- sapply(dat, FUN = function(x) if (x == "" || is.na(x)) {
            TRUE
        } else {
            FALSE
        })
        names(temp) <- NULL
        return(temp)
    }
    apply(dat, c(1, 2), FUN = function(x) if (x == "" || is.na(x)) {
        TRUE
    } else {
        FALSE
    })

}


#' Check validity of equality constraints
#'
#' Internal helper that verifies whether equality constraint
#' labels are valid and appear at least twice.
#'
#' @param mat A matrix, vector, or simulation object.
#'
#' @return Logical indicating whether valid constraints exist.
#'
#' @keywords internal
validConstraints <- function(mat) {
    if (class(mat) == "SimMatrix" || class(mat) == "SimVector") {
        mat <- mat@free
    }

    labels <- is.label(mat)
    combs <- combn(labels[labels], 2)
    res <- combs[1, ] & combs[2, ]

    return(any(res))
}

#' Identify character constraint labels
#'
#' Internal helper used to detect equality constraint labels.
#'
#' @param mat A matrix or vector.
#'
#' @return Logical vector indicating whether elements are labels.
#'
#' @keywords internal
is.label <- function(mat) {
    flat <- as.vector(mat)
    flat[is.na(flat)] <- 0
    isLabel <- sapply(flat, FUN = function(x) {
        suppressWarnings(is.na(as.numeric(x)))
    })
    return(isLabel)
}

#' Identify free parameters
#'
#' Internal helper determining whether parameters are free.
#'
#' @param mat A matrix or vector.
#'
#' @return Logical vector indicating free parameters.
#'
#' @keywords internal
is.free <- function(mat) {
    if (is.character(mat)) {
        isFree <- is.na(mat) | is.label(mat)
    } else {
        isFree <- is.na(mat)
    }
    return(isFree)
}
