# vectorize.object 
# Methods -- simsem package
# Change an object to a vector with labels
# Generic Function: vectorize.object(object, labels, ...)
# Argument:
#	object: The object that users wish to vectorize
# 	labels: The labels of each element in the object
# 	... : Other arguments, such as whether an object is symmetric matrix
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 11, 2011

setMethod("vectorize.object", signature(object="vector", labels="vector"), definition= function(object, labels) {
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

setMethod("vectorize.object", signature(object="matrix", labels="matrix"), definition= function(object, labels, symmetric=FALSE) {
		result <- NULL
		if(symmetric) {
			object_lower <- object[lower.tri(object, diag=TRUE)]
			labels_lower <- labels[lower.tri(labels, diag=TRUE)]
			result <- vectorize.object(object_lower, labels_lower)
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

setMethod("vectorize.object", signature(object="VirtualRSet", labels="SimLabels"), definition= function(object, labels) {
		result <- NULL
		if(!is.null.object(object@LX)) result <- c(result, vectorize.object(object@LX, labels@LX))
		if(!is.null.object(object@LY)) result <- c(result, vectorize.object(object@LY, labels@LY))
		if(!is.null.object(object@GA)) result <- c(result, vectorize.object(object@GA, labels@GA))
		if(!is.null.object(object@BE)) result <- c(result, vectorize.object(object@BE, labels@BE))
		if(!is.null.object(object@PH)) result <- c(result, vectorize.object(object@PH, labels@PH, symmetric=TRUE))
		if(!is.null.object(object@PS)) result <- c(result, vectorize.object(object@PS, labels@PS, symmetric=TRUE))
		if(!is.null.object(object@TD)) result <- c(result, vectorize.object(object@TD, labels@TD, symmetric=TRUE))
		if(!is.null.object(object@TH)) result <- c(result, vectorize.object(object@TH, labels@TH))
		if(!is.null.object(object@TE)) result <- c(result, vectorize.object(object@TE, labels@TE, symmetric=TRUE))
		if(!is.null.object(object@KA)) result <- c(result, vectorize.object(object@KA, labels@KA))
		if(!is.null.object(object@AL)) result <- c(result, vectorize.object(object@AL, labels@AL))
		if(!is.null.object(object@TX)) result <- c(result, vectorize.object(object@TX, labels@TX))
		if(!is.null.object(object@TY)) result <- c(result, vectorize.object(object@TY, labels@TY))
		return(result)
	}
)
#Arguments: 
#	object:	VirtualRSet.c of values that wish to vectorize. Now, use in parameter estimates or SE from SimModelOut.c
#	labels:	labels of each free elements.
#Description: 	This function will vectorize every matrix or vector in the object and combine them together to a single vector.
#Return: 	vector.c with labels name.
