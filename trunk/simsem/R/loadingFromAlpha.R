# loadingFromAlpha
# Function -- simsem package
# Find standardized factor loading from coefficient alpha assuming that all items have equal loadings and error variances (parallel items)
# Function: loadingFromAlpha(alpha, ni)
# Argument:
#	alpha: 	A desired coefficient alpha value.
# 	ni: 	A desired number of items
# Return: 	The standardized factor loadings that make desired coefficient alpha with specified number of items.
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 6, 2011

loadingFromAlpha <- function(alpha, ni) {
	denominator <- ni - ((ni - 1) * alpha)
	result <- sqrt(alpha/ denominator)
	return(result)
}
#Example:
#    loadingFromAlpha(0.8, 4)
