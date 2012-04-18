# TO BE DELETED
contain <- function(element, Vector) {
	ifelse(sum(Vector == element) > 0, return(TRUE), return(FALSE))
}

# Example
# contain(0, 1:3)
# contain(1, 1:3)
