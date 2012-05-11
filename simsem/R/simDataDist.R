# simDataDist: A constructor of data distribution object

simDataDist <- function(..., p = NULL, keepScale = TRUE, reverse = FALSE) {
    List <- list(...)
    check <- sapply(List, is, class2 = "VirtualDist")
    if (sum(check == FALSE) > 0) {
        stop(paste("The object(s) listed in the", paste(which(!check), collapse = ", "), 
            "is/are not a distribution object"))
    } else {
        if (is.null(p)) 
            p <- length(List)
        if (length(reverse) == 1) 
            reverse <- rep(reverse, p)
        if (length(reverse) != p) 
            stop("Please specify the reverse option as TRUE or FALSE or the vector of TRUE/FALSE with the length of the number of the marginal distributions.")
        if (length(List) != p) {
            times <- ceiling(p/length(List))
            List <- rep(List, times)
            List <- List[1:p]
        }
        return(new("SimDataDist", p = p, dist = List, keepScale = keepScale, reverse = reverse))
    }
} 
