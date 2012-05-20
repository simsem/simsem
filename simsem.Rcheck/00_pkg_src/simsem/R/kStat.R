# kStat: Calculate the k-statistic (i.e., unbiased estimator of a cumulant) of a variable

kStat <- function(x, ord) {
    # Formula from mathworld wolfram
    n <- length(x)
    if (ord == 1) {
        return(mean(x))
    } else if (ord == 2) {
        return(centralMoment(x, 2) * n/(n - 1))
    } else if (ord == 3) {
        return(centralMoment(x, 3) * n^2/((n - 1) * (n - 2)))
    } else if (ord == 4) {
        num1 <- n^2
        num2 <- (n + 1) * centralMoment(x, 4)
        num3 <- 3 * (n - 1) * centralMoment(x, 2)^2
        denom <- (n - 1) * (n - 2) * (n - 3)
        return((num1 * (num2 - num3))/denom)
    } else {
        stop("Order can be 1, 2, 3, or 4 only.")
    }
} 
