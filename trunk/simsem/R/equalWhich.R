equalWhich <- function(x, vec) {
	n.elem <- length(vec)
	result <- 0
	for(i in 1:n.elem) {
		if(sum(vec[i] == x) > 0) return(i)
	}
	return(result)
}
