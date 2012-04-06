# isNullObject
# Methods -- simsem package
# Check whether the object is the NULL type of that class
# Generic Function: isNullObject(target)
# Argument:
#	target: 	The checked object
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: February 23, 2011

setMethod("isNullObject", signature(target="vector"), definition=function(target) {
		is(target, "NullVector") || is.nan(target) || (sum(length(target)) == 0)
	}
)
#Arguments: 	vector.c that users wish to check
#Description: check whether the vector.c is NullVector.c, NaN, or zero length
#Return: 	TRUE if it is NullVector.c, NaN, or zero length

setMethod("isNullObject", signature(target="matrix"), definition=function(target) {
		is(target, "NullMatrix") || is.nan(target) || (sum(dim(target)) == 0)
	}
)
#Arguments: 	matrix.c that users wish to check
#Description: check whether the matrix.c is NullMatrix.c, NaN, or 0 x 0 dimension
#Return: 	TRUE if it is NullMatrix.c, NaN, or 0 x 0 dimension

setMethod("isNullObject", signature(target="SimMatrix"), definition=function(target) {
		is(target, "NullSimMatrix")
	}
)
#Arguments: 	SimMatrix.c that users wish to check
#Description: check whether the SimMatrix.c is NullSimMatrix.c
#Return: 	TRUE if it is NullSimMatrix.c

setMethod("isNullObject", signature(target="SymMatrix"), definition=function(target) {
		is(target, "NullSymMatrix")
	}
)
#Arguments: 	SymMatrix.c that users wish to check
#Description: check whether the SymMatrix.c is NullSymMatrix.c
#Return: 	TRUE if it is NullSymMatrix.c

setMethod("isNullObject", signature(target="SimVector"), definition=function(target) {
		is(target, "NullSimVector")
	}
)
#Arguments: 	SimVector.c that users wish to check
#Description: check whether the SimVector.c is NullSimVector.c
#Return: 	TRUE if it is NullSimVector.c

setMethod("isNullObject", signature="SimSet", definition=function(target) {
		is(target, "NullSimSet")
	}
)
#Arguments: 	SimSet.c that users wish to check
#Description: check whether the SimSet.c is NullSimSet.c
#Return: 	TRUE if it is NullSimSet.c

setMethod("isNullObject", signature="SimEqualCon", definition=function(target){
		is(target, "NullSimEqualCon")
	}
)
#Arguments: 	SimEqualCon.c that users wish to check
#Description: check whether the SimEqualCon.c is NullSimEqualCon.c
#Return: 	TRUE if it is NullSimEqualCon.c

setMethod("isNullObject", signature="SimREqualCon", definition=function(target){
		is(target, "NullSimREqualCon")
	}
)
#Arguments: 	SimREqualCon.c that users wish to check
#Description: check whether the SimREqualCon.c is NullSimREqualCon.c
#Return: 	TRUE if it is NullSimREqualCon.c

setMethod("isNullObject", signature="SimMisspec", definition=function(target) {
		is(target, "NullSimMisspec")
	}
)
#Arguments: 	SimMisspec.c that users wish to check
#Description: check whether the SimMisspec.c is NullSimMisspec.c
#Return: 	TRUE if it is NullSimMisspec.c

setMethod("isNullObject", signature="VirtualRSet", definition=function(target) {
		is(target, "NullRSet")
	}
)
#Arguments: 	SimMisspec.c that users wish to check
#Description: check whether the SimMisspec.c is NullSimMisspec.c
#Return: 	TRUE if it is NullSimMisspec.c

setMethod("isNullObject", signature="data.frame", definition=function(target) {
		(is(target, "NullDataFrame") || (nrow(target) == 0) || (ncol(target) == 0))
	}
)
#Arguments: 	data.frame.c that users wish to check
#Description: check whether the data.frame.c is NullDataFrame.c
#Return: 	TRUE if it is NullDataFrame.c

setMethod("isNullObject", signature="SimMissing", definition=function(target) {
		is(target, "NullSimMissing")
	}
)
#Arguments: 	an object in the SimMissing class that users wish to check
#Description: check whether the SimMissing class is the null object
#Return: 	TRUE if it is null

setMethod("isNullObject", signature="SimDataDist", definition=function(target) {
		is(target, "NullSimDataDist")
	}
)
#Arguments: 	an object in the SimDataDist class that users wish to check
#Description: check whether the SimDataDist class is the null object
#Return: 	TRUE if it is null
