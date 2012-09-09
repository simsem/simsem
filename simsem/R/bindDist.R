# bindDist: A constructor of data distribution object

bindDist <- function(margins, ..., p = NULL, keepScale = TRUE, reverse = FALSE) {
    List <- list(...)
    if (is.null(p)) 
        p <- length(List)
    if (length(margins) == 1) 
        margins <- rep(margins, p)
    if (length(reverse) == 1) 
        reverse <- rep(reverse, p)
    if (length(reverse) != p) 
        stop("Please specify the reverse option as TRUE or FALSE or the vector of TRUE/FALSE with the length of the number of the marginal distributions.")
    if (length(margins) != p) 
        stop("Please specify the type of marginal distribution so that the length of the number of the marginal distributions is equal to the number of desired variables.")
    if (length(keepScale) == 1) 
        keepScale <- rep(keepScale, p)
    if (length(keepScale) != p) 
        stop("Please specify the keepScale option as TRUE or FALSE or the vector of TRUE/FALSE with the length of the number of the marginal distributions.")
    if (length(List) != p) 
        List <- rep(List, length.out = p)
    return(new("SimDataDist", margins = margins, paramMargins = List, p = p, keepScale = keepScale, 
        reverse = reverse))
}
 
