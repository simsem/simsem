# symMatrix
# Function -- simsem package
# Description: 	Create SymMatrix.c object that save free parameters and starting values, as well as fixed values. 
#		This will be used for model specification later, such as for factor residual correlation matrix or measurement error correlation matrix.
# Function: symMatrix(free, param = NULL)
# Argument:
#	free:		Matrix of free parameters. Use NA to specify free parameters. Use number as fixed value (including zero)
#	param:	Starting values. Can be either one element or matrix with the same dimension as free parameter matrix. 
#						Each element can be numbers (in either numeric or character format) or the name of distribution object VirtualDist.c.
# Return: 	SymMatrix.c object that will be used for model specification later.
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)

symMatrix <- function(free, param = NULL) {
	if(!isSymmetric(free)) {
		stop("The input matrix is not symmetric.")
	}
	Nrow <- nrow(free)
	Result <- simMatrix(free, param)
	if(Nrow > 1) {
	for(i in 2:Nrow) {
		for(j in 1:(i-1)) {
			Result@free[j,i] <- Result@free[i, j]
			Result@param[j, i] <- Result@param[i, j]
		}
	}
	}
	return(new("SymMatrix", free=Result@free, param=Result@param))
}

#Example:
#latent.cor <- matrix(NA, 3, 3)
#diag(latent.cor) <- 1
#PH <- symMatrix(latent.cor, 0.5)
#u46 <- simUnif(0.4, 0.6)
#factor.cor <- matrix(NA, 4, 4)
#diag(factor.cor) <- 1
#factor.cor.start <- matrix("u46", 4, 4)
#factor.cor.start[1, 2] <- factor.cor.start[2, 1] <- "0.5"
#PS <- symMatrix(factor.cor, factor.cor.start)
