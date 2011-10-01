contain <- function(element, Vector) {
	ifelse(sum(Vector == element) > 0, return(TRUE), return(FALSE))
}
