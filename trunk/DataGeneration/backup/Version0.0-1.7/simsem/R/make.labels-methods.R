# make.labels
# Methods -- simsem package
# Make parameter names for each element in matrices or vectors for analysis in OpenMx (or other possible packages)
# Generic Function: make.labels(object, name, ...)
# Argument:
#	object: The target object that is used to create labels
# 	... : Other arguments, such as package or whether an object is symmetric matrix
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 6, 2011

setMethod("make.labels", signature="vector", definition=function(object, name, package) {
	if(is.null.object(object)) {
		return(new("nullVector"))
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
		return(new("nullMatrix"))
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

