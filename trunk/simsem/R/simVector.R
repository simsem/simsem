# simVector: Create SimVector.c object that save free parameters and starting
# values, as well as fixed values. This will be used for model specification
# later, such as for factor mean vector or measurement error variance vector.

simVector <- function(free, param = NULL) {
    Length <- length(free)
    lab <- rep("", Length)
    if (is.null(param)) {
        return(new("SimVector", free = free, param = lab))
    } else {
        if (length(param) > 1) {
            if (length(param) == Length) {
                for (i in 1:Length) {
                  if (is.na(free[i])) 
                    lab[i] <- param[i]
                }
            } else {
                stop("The length of desired vector and label are not equal")
            }
        } else {
            for (i in 1:Length) {
                if (is.na(free[i])) 
                  lab[i] <- param
            }
        }
        return(new("SimVector", free = free, param = lab))
    }
} 
