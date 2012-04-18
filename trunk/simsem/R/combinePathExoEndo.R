# combinePathExoEndo
# Function -- simsem package
# Combine the regression coefficient matrices (exogenous --> endogenous and endogenous --> endogenous)
# Argument:
#	GA: The regression coefficient matrix from exogenous variables to endogenous variables
#	BE:	The regression coefficient matrix from endogenous variables to endogenous variables
#	value:	The value put in the leftovers (such as exo --> exo)
# Return:	The combined object
# Author: 	Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)

combinePathExoEndo <- function(GA, BE, value=0) {
	nk <- ncol(GA)
	ne <- nrow(GA)
	part1 <- matrix(value, nk, nk+ne)
	part2 <- cbind(GA, BE)
	Result <- rbind(part1, part2)
	return(Result)
}
