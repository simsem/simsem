# bindDist: A constructor of data distribution object

bindDist <- function(margins = NULL, ..., p = NULL, keepScale = TRUE, reverse = FALSE, copula = NULL, skewness = NULL, kurtosis = NULL) {
    List <- list(...)
	if(length(List) > 0) {
		if(!is.null(skewness)) stop("CONFLICT: skewness and list of distributions cannot be both specified.")
		if(!is.null(kurtosis)) stop("CONFLICT: kurtosis and list of distributions cannot be both specified.")
		skewness <- rep(NA, length(List))
		kurtosis <- rep(NA, length(List))
	} else {
		if(!is.null(skewness)) {
			if(!is.null(kurtosis)) {
				if(length(skewness) != length(kurtosis)) stop("CONFLICT: The length of skewness and kurtosis must be equal.")
			} else {
				kurtosis <- rep(0, length(skewness))
			}
		} else {
			if(!is.null(kurtosis)) {
				skewness <- rep(0, length(kurtosis))
			} else {
				stop("CONFLICT: Either the list of distributions and the skewness (or kurtosis) argument must be specified.")
			}		
		}
		List <- rep(list(NA), length(skewness))
	}
    if (is.null(p)) {
		if(length(List) > 0) {
			p <- length(List)
		} else {
			# Already checked for skewness existence above
			p <- length(skewness)
		}
	}
    if (!is.null(margins)) {
		if(length(margins) == 1) margins <- rep(margins, p)
	} else {
		margins <- rep("NA", p)
	}
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
	if (length(skewness) != p)
		skewness <- rep(skewness, length.out = p)
	if (length(kurtosis) != p)
		kurtosis <- rep(kurtosis, length.out = p)
	if (!is.null(copula)) {
		if(!is(copula, "copula")) stop("The 'copula' argument is not a multivariate copula")
		copula@dimension <- as.integer(p)
	} else {
		copula <- new("NullCopula")
	}
    return(new("SimDataDist", margins = margins, paramMargins = List, p = p, keepScale = keepScale, 
        reverse = reverse, copula = copula, skewness = skewness, kurtosis = kurtosis))
}
 
