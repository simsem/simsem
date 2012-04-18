# combineLoadingExoEndo
# Function -- simsem package
# Combine factor loading from X and Y sides into a single matrix
# Argument:
#	LX: factor loading in the X side
#	LY:	factor loading in the Y side
#	value:	the values filling in the leftovers (such as E --> X)
# Return:	The combined matrix
# Author: 	Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)

combineLoadingExoEndo <- function(LX, LY, value = 0) {
	nx <- nrow(LX)
	ny <- nrow(LY)
	nk <- ncol(LX)
	ne <- ncol(LY)
	part1.2 <- matrix(value, nx, ne)
	part2.1 <- matrix(value, ny, nk)
	part1 <- cbind(LX, part1.2)
	part2 <- cbind(part2.1, LY)
	Result <- rbind(part1, part2)
	return(Result)
}
