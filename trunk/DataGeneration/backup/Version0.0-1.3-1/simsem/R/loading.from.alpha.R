loading.from.alpha <-
function(alpha, ni) {
	denominator <- ni - ((ni - 1) * alpha)
	result <- sqrt(alpha/ denominator)
	return(result)
}

