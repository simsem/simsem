# simMatrix
# Function -- simsem package
# Description: Create SimMatrix.c object that save free parameters and starting values, as well as fixed values. 
#		This will be used for model specification later, such as for factor loading matrix or regression coefficient matrix.
# Function: simMatrix(Matrix, name.dist.object = NULL)
# Argument:
#	Matrix:		Matrix of free parameters. Use NA to specify free parameters. Use number as fixed value (including zero)
#	name.dist.object:	Starting values. Can be either one element or matrix with the same dimension as free parameter matrix. 
#						Each element can be numbers (in either numeric or character format) or the name of distribution object VirtualDist.c.
# Return: 	SimMatrix.c object that will be used for model specification later.
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 6, 2011

simMatrix <- function(Matrix, name.dist.object = NULL) {
	Nrow <- nrow(Matrix)
	Ncol <- ncol(Matrix)
	Labels <- matrix("", Nrow, Ncol)
	if(is.null(name.dist.object)) {
		return(new("SimMatrix", free=Matrix, param=Labels))
	} else {
		if(is.matrix(name.dist.object)) {
			if(nrow(name.dist.object) == Nrow & ncol(name.dist.object) == Ncol) {
				for(i in 1:Nrow) {
					for(j in 1:Ncol) {
						if(is.na(Matrix[i, j])) Labels[i, j] <- name.dist.object[i, j] #first, second)
					}
				}
			} else {
				stop("Desired matrix and labels do not have the same dimensions")
			}
		} else {
			for(i in 1:Nrow) {
				for(j in 1:Ncol) {
					if(is.na(Matrix[i, j])) Labels[i, j] <- name.dist.object #first, second)
				}
			}
		}
		return(new("SimMatrix", free=Matrix, param=Labels))
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
