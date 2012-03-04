# divide.object 
# Methods -- simsem package
# Make a division on each elements of the object 
# Generic Function: divide.object(object, constant, ...)
# Argument:
#	object: A desired object
# 	constant: divisor
# 	... : Other arguments, such as whether the matrix is a correlation matrix
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 6, 2011

setMethod("divide.object", signature(object="vector", constant="numeric"), definition=function(object, constant) {
		if(is.null.object(object)) {
			return(new("NullVector"))
		} else {
			return(object / constant)
		}
	}
)
#Arguments: 	
#	object: 	vector.c that will be divided
#	constant:	divisor
#Description: 	Divide each element in the vector. If the vector is NullVector.c, it will do nothing.
#Return: 	Divided vector or NullVector.c

setMethod("divide.object", signature(object="matrix", constant="numeric"), definition=function(object, constant, correlation = FALSE) {
		if(is.null.object(object)) {
			return(new("NullMatrix"))
		} else {
			if(correlation) {
				temp <- diag(object)
				object <- object / constant
				diag(object) <- temp
				return(object)
			} else {
				return(object / constant)
			}
		}
	}
)
#Arguments: 	
#	object: 	matrix.c that will be divided
#	constant:	divisor
#	correlation:	TRUE if the matrix is a correlation matrix. The default is FALSE.
#Description: 	Divide each element in the matrix. If the matrix is NullMatrix.c, it will do nothing.
#Return: 	Divided matrix or NullMatrix.c

setMethod("divide.object", signature(object="MatrixSet", constant="numeric"), definition=function(object, constant) {
		#browser()
		LY <- divide.object(object@LY, constant)
		VTE <- divide.object(object@VTE, constant)
		RTE <- divide.object(object@RTE, constant, correlation = TRUE)
		VY <- divide.object(object@VY, constant)
		TY <- divide.object(object@TY, constant)
		MY <- divide.object(object@MY, constant)
		BE <- divide.object(object@BE, constant)
		VPS <- divide.object(object@VPS, constant)
		RPS <- divide.object(object@RPS, constant, correlation = TRUE)
		VE <- divide.object(object@VE, constant) 
		AL <- divide.object(object@AL, constant)
		ME <- divide.object(object@ME, constant) 
		LX <- divide.object(object@LX, constant) 
		VTD <- divide.object(object@VTD, constant)
		RTD <- divide.object(object@RTD, constant, correlation = TRUE)
		VX <- divide.object(object@VX, constant)
		TX <- divide.object(object@TX, constant)
		MX <- divide.object(object@MX, constant)
		GA <- divide.object(object@GA, constant)
		VPH <- divide.object(object@VPH, constant)
		RPH <- divide.object(object@RPH, constant, correlation = TRUE)
		KA <- divide.object(object@KA, constant)
		RTH <- divide.object(object@RTH, constant)
		Output <- new("MatrixSet", modelType=object@modelType, LY=LY, VTE=VTE, RTE=RTE, VY=VY, TY=TY, MY=MY, 
			BE=BE, VPS=VPS, RPS=RPS, VE=VE, AL=AL, ME=ME,
			LX=LX, VTD=VTD, RTD=RTD, VX=VX, TX=TX, MX=MX,
			GA=GA, VPH=VPH, RPH=RPH, KA=KA, RTH=RTH)
		return(Output)
	}
)
#Arguments: 	
#	object: 	MatrixSet.c that will be divided
#	constant:	divisor
#Description: 	Divide all matrices and vectors in the MatrixSet.c
#Return: 	Divided MatrixSet.c
