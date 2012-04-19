# To be deleted

isNAVector <- function(vec) all(is.na(vec)) {
	return(length(vec) == sum(is.na(vec)))
}
