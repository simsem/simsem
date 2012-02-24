# make.labels
# Methods -- simsem package
# Make parameter names for each element in matrices or vectors for analysis in OpenMx (or other possible packages)
# Generic Function: make.labels(object, ...)
# Argument:
#	object: The target object that is used to create labels
# 	... : Other arguments, such as package or whether an object is symmetric matrix
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: February 24, 2011

setMethod("make.labels", signature="vector", definition=function(object, name, package) {
	if(is.null.object(object)) {
		return(new("NullVector"))
	} else {
		Length <- length(object)
		if(package == "OpenMx") {
			for(i in 1:Length) {
				ifelse(is.na(object[i]), object[i] <- paste(name, i, sep = ""), object[i] <- NA)
			}
			return(object)
		} else if(package == "lavaan") {
			object[] <- ""
			return(object)
		}
	}
})
#Arguments: 
#	object:	vector.c that users wish to create labels from
# 	name:	name of the object (vector.c)
#	package:	Desired package, generally used for OpenMx
#Description: This function will create labels of each element by the vector name followed by number of elements in a vector
#Return: 	vector.c with labels in it

setMethod("make.labels", signature="matrix", definition=function(object, name, package, symmetric=FALSE) {
	if(is.null.object(object)) {
		return(new("NullMatrix"))
	} else {
		np <- nrow(object)
		nq <- ncol(object)
		if(package == "OpenMx") {
			if(symmetric) {
				for(i in 1:np) {
					for(j in 1:i) {
						if(is.na(object[i,j])) {
							object[i, j] <- paste(name, i, "_", j, sep = "")
						} else {
							object[i, j] <- NA
						}
						if(i != j) object[j, i] <- object[i,j]
					}
				}
			} else {
				for(i in 1:np) {
					for(j in 1:nq) {
						if(is.na(object[i,j])) {
							object[i, j] <- paste(name, i, "_", j, sep ="")
						} else {
							object[i, j] <- NA
						}
					}
				}
			}
		} else if(package == "lavaan") {
			object[,] <- ""
		}
		return(object)
	}
})
#Arguments: 
#	object:	matrix.c that users wish to create labels from
# 	name:	name of the object (matrix.c)
#	package:	Desired package, generally used for OpenMx
#	symmetric:	TRUE if the matrix is symmetric matrix. This function will provides the same labels for above and below diagonal elements if it is symmetric.
#Description: This function will create labels of each element by the matrix name followed by number of elements in a matrix
#Return: 	matrix.c with labels in it

setMethod("make.labels", signature="SimFreeParam", definition=function(object, package) {
	LY <- make.labels(object@LY, "LY", package)
	TE <- make.labels(object@TE, "TE", package, symmetric=TRUE)
	PS <- make.labels(object@PS, "PS", package, symmetric=TRUE)
	BE <- make.labels(object@BE, "BE", package)
	TY <- make.labels(object@TY, "TY", package)
	AL <- make.labels(object@AL, "AL", package)
	LX <- make.labels(object@LX, "LX", package)
	TD <- make.labels(object@TD, "TD", package, symmetric=TRUE)
	PH <- make.labels(object@PH, "PH", package, symmetric=TRUE)
	GA <- make.labels(object@GA, "GA", package)
	TX <- make.labels(object@TX, "TX", package)
	KA <- make.labels(object@KA, "KA", package)
	TH <- make.labels(object@TH, "TH", package)
	return(new("SimLabels", LY=LY, TE=TE, BE=BE, PS=PS, AL=AL, TY=TY,
			LX=LX, TD=TD, TX=TX, GA=GA, PH=PH, KA=KA, TH=TH, modelType=object@modelType))
})
#Arguments: 
#	object:	freeMatrixSet.c that users wish to create labels from
#	package:	Desired package, generally used for OpenMx
#Description: This function will create labels of each element by the object name followed by number of elements in a matrix in every matrix or vector in the free parameter object
#Return: 	SimLabels.c that contains labels of free parameters.

setMethod("make.labels", signature="VirtualDist", definition=function(object, digit=3) {
		indivAttr <- slotNames(object)
		val <- sapply(indivAttr, slot, object=object)
		val <- round(val, digit)
		lab <- mapply(paste, indivAttr, "=", val)
		lab <- paste(lab, collapse=", ")
		return(lab)
})
#Arguments: 
#	object:		Distribution object class that users wish to create a long label of attributes
#Description: This function will create a description of attributes
#Return: 	SimLabels.c that contains labels of free parameters.
