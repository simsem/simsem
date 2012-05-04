# to be deleted

cor2cov <- function(correlation, stdev) {
    ni <- nrow(correlation)
    if (!is.matrix(stdev)) {
        temp <- matrix(0, ni, ni)
        diag(temp) <- stdev
        stdev <- temp
    }
    covariance <- stdev %*% correlation %*% stdev
    return(covariance)
} 
