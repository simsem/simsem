# vectorizeObject 
# Methods -- simsem package
# Change an object to a vector with labels
# Generic Function: vectorizeObject(object, labels, ...)
# Argument:
#	object: The object that users wish to vectorize
# 	labels: The labels of each element in the object
# 	... : Other arguments, such as whether an object is symmetric matrix
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)

setMethod("vectorizeObject", signature(object="vector", labels="vector"), definition= function(object, labels) {
		result <- as.vector(object[!is.na(labels)])
		names(result) <- as.vector(labels[!is.na(labels)])
		return(result)
	}
)
#Arguments: 
#	object:	vector.c of values that wish to vectorize
#	labels:	vector.c of labels
#Description: 	This function will select elements in the object that the corresponding elements in labels are not NA and give the name for it. 
#Return: 	vector.c with labels name.

setMethod("vectorizeObject", signature(object="matrix", labels="matrix"), definition= function(object, labels, symmetric=FALSE) {
		result <- NULL
		if(symmetric) {
			object_lower <- object[lower.tri(object, diag=TRUE)]
			labels_lower <- labels[lower.tri(labels, diag=TRUE)]
			result <- vectorizeObject(object_lower, labels_lower)
		} else {
			result <- as.vector(object[!is.na(labels)])
			names(result) <- as.vector(labels[!is.na(labels)])	
		}
		return(result)
	}
)
#Arguments: 
#	object:	matrix.c of values that wish to vectorize
#	labels:	matrix.c of labels
# 	symmetric:	whether the matrix is symmetric matrix
#Description: 	This function will select elements in the object that the corresponding elements in labels are not NA, give the name for it, and then transform it to vector.
#Return: 	vector.c with labels name.

setMethod("vectorizeObject", signature(object="VirtualRSet", labels="SimLabels"), definition= function(object, labels) {
		result <- NULL
		if(!isNullObject(labels@LX)) result <- c(result, vectorizeObject(object@LX, labels@LX))
		if(!isNullObject(labels@LY)) result <- c(result, vectorizeObject(object@LY, labels@LY))
		if(!isNullObject(labels@GA)) result <- c(result, vectorizeObject(object@GA, labels@GA))
		if(!isNullObject(labels@BE)) result <- c(result, vectorizeObject(object@BE, labels@BE))
		if(!isNullObject(labels@PH)) result <- c(result, vectorizeObject(object@PH, labels@PH, symmetric=TRUE))
		if(!isNullObject(labels@PS)) result <- c(result, vectorizeObject(object@PS, labels@PS, symmetric=TRUE))
		if(!isNullObject(labels@TD)) result <- c(result, vectorizeObject(object@TD, labels@TD, symmetric=TRUE))
		if(!isNullObject(labels@TH)) result <- c(result, vectorizeObject(object@TH, labels@TH))
		if(!isNullObject(labels@TE)) result <- c(result, vectorizeObject(object@TE, labels@TE, symmetric=TRUE))
		if(!isNullObject(labels@KA)) result <- c(result, vectorizeObject(object@KA, labels@KA))
		if(!isNullObject(labels@AL)) result <- c(result, vectorizeObject(object@AL, labels@AL))
		if(!isNullObject(labels@TX)) result <- c(result, vectorizeObject(object@TX, labels@TX))
		if(!isNullObject(labels@TY)) result <- c(result, vectorizeObject(object@TY, labels@TY))
		return(result)
	}
)
#Arguments: 
#	object:	VirtualRSet.c of values that wish to vectorize. Now, use in parameter estimates or SE from SimModelOut.c
#	labels:	labels of each free elements.
#Description: 	This function will vectorize every matrix or vector in the object and combine them together to a single vector.
#Return: 	vector.c with labels name.
