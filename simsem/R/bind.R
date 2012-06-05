bind <- function(free = NULL, popParam = NULL, misspec = NULL, prior) {
    if (is.null(value)) {
        if (any(is.na(free))) {
            stop("There are free parameters but no parameter/starting values are specified.")
        } else {
            return(new("SimMatrix", free = free, value = matrix("", nrow(free), ncol(free))))
        }
    } else {
        if (is.null(free)) {
            if (is.matrix(value)) {
                free <- matrix(0, nrow(value), ncol(value))
                free[!((value == 0) | (value == ""))] <- NA
            } else {
                stop("If the matrix of free parameters is not specified, the parameter value should be specified as a matrix")
            }
        }
        lab <- matrix("", nrow(free), ncol(free))
        if (any(is.na(free))) {
            if (is.matrix(value)) {
                if (!(nrow(value) == nrow(free)) | !(ncol(value) == ncol(free))) 
                  stop("Two specified matrices do not have the same dimensions.")
                lab[is.na(free)] <- checkInputValueVector(value[is.na(free)])
            } else {
                lab[is.na(free)] <- checkInputValue(value)
            }
        }
        return(new("SimMatrix", free = free, value = lab))
    }
} 
