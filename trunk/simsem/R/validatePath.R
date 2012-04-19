# validatePath
# Function -- simsem package
# Validate whether the regression coefficient (or loading) matrix is good
# Argument:
#	path:		A regression coefficient or loading matrix
#	var.iv:		The variances of variables corresponding to the columns
#	var.dv:		The variances of variables corresponding to the rows
# Return: 	TRUE if the matrix is good
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)

validatePath <- function(path, var.iv, var.dv) {
	inv.var.iv <- 1/var.iv
	max.path <- sqrt(var.dv) %o% sqrt(inv.var.iv)
	abs.path <- abs(path)
	max.path[var.dv == 0, ] <- abs.path[var.dv == 0, ]
	ifelse(sum(abs.path > max.path) > 0, return(FALSE), return(TRUE))
}
