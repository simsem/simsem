# loadingFromAlpha
# Function -- simsem package
# Find a standardized factor loading that provide a specified alpha value
# Argument:
#	alpha: 	A desired coefficient alpha value.
# 	ni: 	A desired number of items
# Return: 	The standardized factor loadings that make desired coefficient alpha with specified number of items.
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)

loadingFromAlpha <- function(alpha, ni) {
	denominator <- ni - ((ni - 1) * alpha)
	result <- sqrt(alpha/ denominator)
	return(result)
}
#Example:
#    loadingFromAlpha(0.8, 4)
