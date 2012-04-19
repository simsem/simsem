# validateCovariance
# Function -- simsem package
# Validate whether all elements provides a good covariance matrix
# Argument:
#	resVar:		A vector of residual variances
#	correlation:	A correlation matrix
#	totalVar: 	A vector of total variances
# Return: 	TRUE if the covariance matrix is good
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)

validateCovariance <- function(resVar, correlation, totalVar = NULL) {
	if(!isSymmetric(correlation)) return(FALSE)
	if(sum(resVar < 0) > 0) return(FALSE)
	zero.row <- resVar == 0
	if(sum(zero.row) > 0) {
		target.rows <- extract(correlation, row=which(zero.row), col=NULL)
		for(i in 1:nrow(target.rows)) {
			temp <- target.rows[i, -zero.row[i]]
			if(sum(temp != 0) > 0) return(FALSE)
		}
		if(det(correlation < 0)) return(FALSE)
	} else {
		if(det(correlation) <= 0) return(FALSE)
	}
	if(!is.null(totalVar)) {
		if(sum(totalVar < 0) > 0) return(FALSE)
	}
	return(TRUE)
}
