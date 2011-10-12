# sym.matrix.object
# Function -- simsem package
# Description: 	Create symMatrix.c object that save free parameters and starting values, as well as fixed values. 
#		This will be used for model specification later, such as for factor residual correlation matrix or measurement error correlation matrix.
# Function: sym.matrix.object(Matrix, name.dist.object = NULL)
# Argument:
#	Matrix:		Matrix of free parameters. Use NA to specify free parameters. Use number as fixed value (including zero)
#	name.dist.object:	Starting values. Can be either one element or matrix with the same dimension as free parameter matrix. 
#						Each element can be numbers (in either numeric or character format) or the name of distribution object simDist.c.
# Return: 	symMatrix.c object that will be used for model specification later.
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 6, 2011

sym.matrix.object <- function(Matrix, name.dist.object = NULL) {
	if(!isSymmetric(Matrix)) {
		stop("The input matrix is not symmetric.")
	}
	Nrow <- nrow(Matrix)
	Result <- matrix.object(Matrix, name.dist.object)
	if(Nrow > 1) {
	for(i in 2:Nrow) {
		for(j in 1:(i-1)) {
			Result@Data[j,i] <- Result@Data[i, j]
			Result@Labels[j, i] <- Result@Labels[i, j]
		}
	}
	}
	return(new("symMatrix", Data=Result@Data, Labels=Result@Labels))
}

#Example:
#latent.cor <- matrix(NA, 3, 3)
#diag(latent.cor) <- 1
#PH <- sym.matrix.object(latent.cor, 0.5)
#u46 <- runif.object(0.4, 0.6)
#factor.cor <- matrix(NA, 4, 4)
#diag(factor.cor) <- 1
#factor.cor.start <- matrix("u46", 4, 4)
#factor.cor.start[1, 2] <- factor.cor.start[2, 1] <- "0.5"
#PS <- sym.matrix.object(factor.cor, factor.cor.start)
