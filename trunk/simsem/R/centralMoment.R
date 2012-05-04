# centralMoment: Calculate central moments of a variable

centralMoment <- function(x, ord, weight = NULL) {
    if (ord < 2) 
        stop("Central moment can be calculated for order 2 or more in an integer.")
    if (is.null(weight)) 
        weight <- rep(1, length(x))
    wm <- weightedMean(x, weight)
    result <- sum(weight * ((x - wm)^(ord)))/sum(weight)
    return(result)
} 
