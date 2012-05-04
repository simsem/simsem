# symMatrix: Create SymMatrix.c object that save free parameters and starting
# values, as well as fixed values. This will be used for model specification
# later, such as for factor residual correlation matrix or measurement error
# correlation matrix.

symMatrix <- function(free, param = NULL) {
    if (!isSymmetric(free)) {
        stop("The input matrix is not symmetric.")
    }
    Nrow <- nrow(free)
    Result <- simMatrix(free, param)
    if (Nrow > 1) {
        for (i in 2:Nrow) {
            for (j in 1:(i - 1)) {
                Result@free[j, i] <- Result@free[i, j]
                Result@param[j, i] <- Result@param[i, j]
            }
        }
    }
    return(new("SymMatrix", free = Result@free, param = Result@param))
} 
