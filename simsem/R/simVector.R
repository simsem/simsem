# simVector: Create SimVector.c object that save free parameters and starting values, as well as fixed values. This will be used for
# model specification later, such as for factor mean vector or measurement error variance vector.

simVector <- function(free = NULL, value = NULL) {
    if (is.null(value)) {
        if (any(is.na(free))) {
            stop("There are free parameters but no parameter/starting values are specified.")
        } else {
            return(new("SimVector", free = free, value = rep("", length(free))))
        }
    } else {
        if (is.null(free)) {
            if (is.vector(value)) {
                free <- rep(0, length(value))
                free[!((value == 0) | (value == ""))] <- NA
            } else {
                stop("If the vector of free parameters is not specified, the parameter value should be specified as a vector")
            }
        }
        lab <- rep("", length(free))
        if (any(is.na(free))) {
            if (length(value) > 1) {
                if (length(free) != length(value)) 
                  stop("Two specified vectors do not have the same dimensions.")
                lab[is.na(free)] <- checkInputValueVector(value[is.na(free)])
            } else {
                lab[is.na(free)] <- checkInputValue(value)
            }
        }
        return(new("SimVector", free = free, value = lab))
    }
} 
