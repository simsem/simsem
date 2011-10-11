# starting.values
# Methods -- simsem package
# Description:	Find starting values of free parameters based on pre-specified starting values. If the pre-specified starting values are numbers, 
#		the function will use that values. If they are distribution object, this function will randomly draw from the distribution 
#		10 times and take the average of those values.
# Generic Function: starting.values(object, trial, ...)
# Argument:
#	object: The target object that is used to find starting values
#	trial:	Number of random drawn to find starting values of simDist.c
# 	... : Other arguments, such as reduced for reducing X-Y set of matrices to Y set of matrices only
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 6, 2011

setMethod("starting.values", signature(object="simMatrix"), definition=function(object, trial, ...) {
		if(is.null.object(object)) return(new("nullMatrix"))
		Nrow <- nrow(run(object))
		Ncol <- ncol(run(object))
		Result <- matrix(0, Nrow, Ncol)
		for(i in 1:trial) {
			temp <- run(object)
			Result <- Result + temp
		}
		return(Result / trial)
	}
)
#Arguments: 
#	object:		simMatrix.c used to find starting values
# 	trial:		Number of random draws before taking the average
#Description: 	Draw samples from the simMatrix.c, take the average, and report the starting values as a matrix.
#Return: 	matrix.c of starting values
#Example:
#u89 <- runif.object(0.8, 0.9)
#loading <- matrix(0, 6, 2)
#loading[1:3, 1] <- NA
#loading[4:6, 2] <- NA
#loadingValues <- matrix(0, 6, 2)
#LX <- matrix.object(loading, "u89")
#starting.values(LX, 10)

setMethod("starting.values", signature(object="simVector"), definition=function(object, trial, ...) {
		if(is.null.object(object)) return(new("nullVector"))
		Length <- length(run(object))
		Result <- rep(0, Length)
		for(i in 1:trial) {
			temp <- run(object)
			Result <- Result + temp
		}
		return(Result / trial)
	}
)
#Arguments: 
#	object:		simVector.c used to find starting values
# 	trial:		Number of random draws before taking the average
#Description: 	Draw samples from the simVector.c, take the average, and report the starting values as a vector.
#Return: 	vector.c of starting values

setMethod("starting.values", signature(object="simMatrixSet"), definition=function(object, trial, reduced=FALSE) {
		result <- run(object)
		#browser()
		if(trial > 1) {
			for(i in 2:trial) {
				temp <- run(object)
				result <- combine.object(result, temp)
			}
			#browser()
			result <- divide.object(result, trial)
		}
		#browser()
		result@Tag <- object@Tag
		if(reduced == TRUE) result <- reduce.matrices(result)
		return(result)
	}
)
#Arguments: 
#	object:		simMatrixSet.c used to find starting values
# 	trial:		Number of random draws before taking the average
#	reduced:	TRUE if users wish to collapse exogenous and endogenous set of matrices to endogenous set of matrices only. The default is FALSE.
#Description: 	Draw samples from simMatrixSet.c, take the average, and report the starting values as a matrixSet.c.
#Return: 	matrixSet.c of starting values
#Example:
#u89 <- runif.object(0.8, 0.9)
#loading <- matrix(0, 6, 2)
#loading[1:3, 1] <- NA
#loading[4:6, 2] <- NA
#loadingValues <- matrix(0, 6, 2)
#LX <- matrix.object(loading, "u89")
#latent.cor <- matrix(NA, 2, 2)
#diag(latent.cor) <- 1
#PH <- sym.matrix.object(latent.cor, 0.5)
#error.cor <- matrix(0, 6, 6)
#diag(error.cor) <- 1
#TD <- sym.matrix.object(error.cor)
#CFA.Model <- matrix.CFA.object(LX = LX, PH = PH, TD = TD)
#result <- starting.values(CFA.Model, 10)
#summary(result)
