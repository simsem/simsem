# summaryMisspec: This function will summarize the obtained fit indices and
# generate a data frame.

summaryMisspec <- function(object) {
    object <- clean(object)
    if (all(dim(object@misspecValue) == 0)) {
        stop("This object does not have any model misspecification.")
    } else {
        misspecAverage <- colMeans(object@misspecValue, na.rm = TRUE)
        misspecSE <- sapply(object@misspecValue, sd, na.rm = TRUE)
        popAverage <- colMeans(object@popFit, na.rm = TRUE)
        popSE <- sapply(object@popFit, sd, na.rm = TRUE)
        mis <- data.frame(mean = misspecAverage, sd = misspecSE)
        pop <- data.frame(mean = popAverage, sd = popSE)
        return(rbind(pop, mis))
    }
} 
