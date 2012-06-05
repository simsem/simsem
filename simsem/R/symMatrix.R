# symMatrix: Create SymMatrix.c object that save free parameters and starting values, as well as fixed values. This will be used for
# model specification later, such as for factor residual correlation matrix or measurement error correlation matrix.

symMatrix <- function(free = NULL, value = NULL) {
    if (!is.null(free) && !isSymmetric(free)) {
        stop("The free parameters matrix is not symmetric.")
    }
    if (!is.null(value) && is.matrix(value) && !isSymmetric(value)) {
        stop("The value matrix is not symmetric.")
    }
    if (is.null(free)) {
        if (is.matrix(value)) {
            free <- matrix(0, nrow(value), ncol(value))
            free[value == 1] <- 1
            free[!((value == 0) | (value == "") | (value == 1))] <- NA
        } else {
            stop("If the matrix of free parameters is not specified, the parameter value should be specified as a matrix")
        }
    }
    Result <- simMatrix(free, value)
    if (nrow(free) > 1) {
        for (i in 2:nrow(free)) {
            for (j in 1:(i - 1)) {
                Result@free[j, i] <- Result@free[i, j]
                Result@value[j, i] <- Result@value[i, j]
            }
        }
    }
    return(new("SymMatrix", free = Result@free, value = Result@value))
} 
