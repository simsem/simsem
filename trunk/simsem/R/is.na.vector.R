isNAVector <- function(vec) {
	k <- length(vec)
	match <- sum(is.na(vec))
	return(k == match)
}
