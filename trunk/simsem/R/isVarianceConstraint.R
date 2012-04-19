# isVarianceConstraint
# Function -- simsem package
# Description: Check whether all rownames in a constraint matrix containing symbols of variance vectors
# Argument:
#	Name: 	The rownames of a constraint matrix
# Return: 	TRUE if all rownames are variances vectors
#			FALSE if all rownames are not variances vectors
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)

isVarianceConstraint <- function(Name) {
	W <- getKeywords()
	keywords <- c(W$VTE, W$VTD, W$VPH, W$VPS, W$VX, W$VY, W$VE)
	result <- Name %in% keywords
	if(sum(result) == length(Name)) {
		return(TRUE)
	} else if(sum(result) == 0) {
		return(FALSE)
	} else {
		stop("A constraint matrix was mixed between variance and other types of elements.")
	}
}
