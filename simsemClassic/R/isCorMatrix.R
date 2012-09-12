# isCorMatrix: Check whether a matrix is a possible correlation matrix

isCorMatrix <- function(matrixA) {
    isSymmetric(matrixA) && all(!is.na(diag(matrixA))) && all(diag(matrixA) == 1) && all(matrixA <= 1) && all(matrixA >= -1)
} 
