# simMatrix
# Function -- simsem package
# Description: Create SimMatrix.c object that save free parameters and starting values, as well as fixed values. 
#		This will be used for model specification later, such as for factor loading matrix or regression coefficient free.
# Function: simMatrix(free, param = NULL)
# Argument:
#	free:		Matrix of free parameters. Use NA to specify free parameters. Use number as fixed value (including zero)
#	param:	Starting values. Can be either one element or matrix with the same dimension as free parameter matrix. 
#						Each element can be numbers (in either numeric or character format) or the name of distribution object VirtualDist.c.
# Return: 	SimMatrix.c object that will be used for model specification later.
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 6, 2011

simMatrix <- function(free, param = NULL) {
	Nrow <- nrow(free)
	Ncol <- ncol(free)
	lab <- matrix("", Nrow, Ncol)
	if(is.null(param)) {
		return(new("SimMatrix", free=free, param=lab))
	} else {
		if(is.matrix(param)) {
			if(nrow(param) == Nrow & ncol(param) == Ncol) {
				for(i in 1:Nrow) {
					for(j in 1:Ncol) {
						if(is.na(free[i, j])) lab[i, j] <- param[i, j] #first, second)
					}
				}
			} else {
				stop("Desired Matrix and labels do not have the same dimensions")
			}
		} else {
			for(i in 1:Nrow) {
				for(j in 1:Ncol) {
					if(is.na(free[i, j])) lab[i, j] <- param #first, second)
				}
			}
		}
		return(new("SimMatrix", free=free, param=lab))
	}
}

#Example:
#loading <- matrix(0, 6, 2)
#loading[1:3, 1] <- NA
#loading[4:6, 2] <- NA
#loadingValues <- matrix(0, 6, 2)
#loadingValues[1:3, 1] <- 0.7
#loadingValues[4:6, 2] <- 0.7
#LX <- simMatrix(loading, loadingValues)
#summary(LX)
#run(LX)
#n65 <- simNorm(0.6, 0.05)
#LY <- simMatrix(loading, "n65")
#summary(LY)
#run(LY)
