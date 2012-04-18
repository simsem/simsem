# combineMeasurementErrorExoEndo
# Function -- simsem package
# Combine measurement error correlation from X and Y sides into a single matrix
# Argument:
#	TD: error of measurement correlation in the X side
#	TE:	error of measurement correlation in the Y side
#	TH: the correlation between the error of measurement in the X and Y sides
# Return:	The combined matrix
# Author: 	Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)

combineMeasurementErrorExoEndo <- function(TD, TE, TH) {
	part1 <- cbind(TD, TH)
	part2 <- cbind(t(TH), TE)
	Result <- rbind(part1, part2)
	return(Result)
}
