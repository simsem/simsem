# weightedMean: Calculate the weighted mean of a variable

weightedMean <- function(x, weight = NULL) {
    if (is.null(weight)) 
        weight <- rep(1, length(x))
    wm <- sum(weight * x)/sum(weight)
    return(wm)
} 
