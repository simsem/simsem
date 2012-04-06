isVarianceConstraint <- function(Name) {
	W <- getKeywords()
	keywords <- c(W$VTE, W$VTD, W$VPH, W$VPS, W$VX, W$VY, W$VE)
	result <- rep(0, length(Name))
	for(i in 1:length(Name)) {
		result[i] <- sum(Name[i] == keywords)
		ifelse(result[i] > 0, result[i] <- 1, result[i] <- 0)
	}
	if(sum(result) == length(Name)) {
		return(TRUE)
	} else if(sum(result) == 0) {
		return(FALSE)
	} else {
		stop("A constraint matrix was mixed between variance and other types of elements.")
	}
}
