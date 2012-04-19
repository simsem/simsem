# isMeanConstraint
# Function -- simsem package
# Description: Check whether all rownames in a constraint matrix containing symbols of means vectors
# Argument:
#	Name: 	The rownames of a constraint matrix
# Return: 	TRUE if all rownames are means vectors
#			FALSE if all rownames are not means vectors
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)

isMeanConstraint <- function(Name) {
	W <- getKeywords()
	keywords <- c(W$TX, W$TY, W$KA, W$AL, W$MX, W$MY, W$ME)
	result <- Name %in% keywords
	if(sum(result) == length(Name)) {
		return(TRUE)
	} else if(sum(result) == 0) {
		return(FALSE)
	} else {
		stop("A constraint matrix was mixed between mean and other types of elements.")
	}
}
