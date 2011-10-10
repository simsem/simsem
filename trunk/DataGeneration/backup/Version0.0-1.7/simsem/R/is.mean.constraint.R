is.mean.constraint <- function(Name) {
	W <- get.keywords()
	keywords <- c(W$TX, W$TY, W$KA, W$AL, W$MX, W$MY, W$ME)
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
		stop("A constraint matrix was mixed between mean and other types of elements.")
	}
}
