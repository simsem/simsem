# bindDist: A constructor of data distribution object

bindDist <- function(margins, ..., p = NULL, keepScale = TRUE, reverse = FALSE, copula = NULL) {
	library(copula)
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
	if (!is.null(copula)) {
		if(!is(copula, "copula")) stop("The 'copula' argument is not a multivariate copula")
		copula@dimension <- as.integer(p)
	} else {
		copula <- new("NullCopula")
	}
    return(SimDataDist$new(margins = margins, paramMargins = List, p = p, keepScale = keepScale, 
        reverse = reverse, copula = copula))
}
 
