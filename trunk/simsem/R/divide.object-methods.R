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
		TE <- divide.object(object@TE, constant, correlation = TRUE)
		VY <- divide.object(object@VY, constant)
		TY <- divide.object(object@TY, constant)
		MY <- divide.object(object@MY, constant)
		BE <- divide.object(object@BE, constant)
		VPS <- divide.object(object@VPS, constant)
		PS <- divide.object(object@PS, constant, correlation = TRUE)
		VE <- divide.object(object@VE, constant) 
		AL <- divide.object(object@AL, constant)
		ME <- divide.object(object@ME, constant) 
		LX <- divide.object(object@LX, constant) 
		VTD <- divide.object(object@VTD, constant)
		TD <- divide.object(object@TD, constant, correlation = TRUE)
		VX <- divide.object(object@VX, constant)
		TX <- divide.object(object@TX, constant)
		MX <- divide.object(object@MX, constant)
		GA <- divide.object(object@GA, constant)
		VPH <- divide.object(object@VPH, constant)
		PH <- divide.object(object@PH, constant, correlation = TRUE)
		KA <- divide.object(object@KA, constant)
		TH <- divide.object(object@TH, constant)
		Output <- new("MatrixSet", modelType=object@modelType, LY=LY, VTE=VTE, TE=TE, VY=VY, TY=TY, MY=MY, 
			BE=BE, VPS=VPS, PS=PS, VE=VE, AL=AL, ME=ME,
			LX=LX, VTD=VTD, TD=TD, VX=VX, TX=TX, MX=MX,
			GA=GA, VPH=VPH, PH=PH, KA=KA, TH=TH)
		return(Output)
	}
)
#Arguments: 	
#	object: 	MatrixSet.c that will be divided
#	constant:	divisor
#Description: 	Divide all matrices and vectors in the MatrixSet.c
#Return: 	Divided MatrixSet.c
