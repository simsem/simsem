# simFunction
# Function -- simsem package
# Constructor of the SimFunction class
# Function: simFunction(fun, ...)
# Argument:
#	fun:	The function that users need to call
#	...:	Addition arguments for the function
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: April 11, 2012

simFunction <- function(fun, ...) {
	List <- list(...)
	mc <- match.call()
	return(new("SimFunction", fun=fun, attribute=List, callfun=mc))
}

# Example:
# x <- simFunction(rnorm, sd=100, mean=1)
# summary(x)
# run(x, 10)
