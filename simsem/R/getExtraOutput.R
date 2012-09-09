# getPopulation: Description: Extract the population value from an object

getExtraOutput <- function(object) {
    if (length(object@extraOut) == 0) {
        stop("This simulation result does not contain any extra results")
    } else {
        return(object@extraOut[object@converged])
    }
} 
