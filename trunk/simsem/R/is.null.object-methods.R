# is.null.object
# Methods -- simsem package
# Check whether the object is the NULL type of that class
# Generic Function: is.null.object(target)
# Argument:
#	target: 	The checked object
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 6, 2011

setMethod("is.null.object", signature(target="vector"), definition=function(target) {
		is(target, "nullVector") || is.nan(target) || (sum(length(target)) == 0)
	}
)
#Arguments: 	vector.c that users wish to check
#Description: check whether the vector.c is nullVector.c, NaN, or zero length
#Return: 	TRUE if it is nullVector.c, NaN, or zero length

setMethod("is.null.object", signature(target="matrix"), definition=function(target) {
		is(target, "nullMatrix") || is.nan(target) || (sum(dim(target)) == 0)
	}
)
#Arguments: 	matrix.c that users wish to check
#Description: check whether the matrix.c is nullMatrix.c, NaN, or 0 x 0 dimension
#Return: 	TRUE if it is nullMatrix.c, NaN, or 0 x 0 dimension

setMethod("is.null.object", signature(target="simMatrix"), definition=function(target) {
		is(target, "nullSimMatrix")
	}
)
#Arguments: 	simMatrix.c that users wish to check
#Description: check whether the simMatrix.c is nullSimMatrix.c
#Return: 	TRUE if it is nullSimMatrix.c

setMethod("is.null.object", signature(target="symMatrix"), definition=function(target) {
		is(target, "nullSymMatrix")
	}
)
#Arguments: 	symMatrix.c that users wish to check
#Description: check whether the symMatrix.c is nullSymMatrix.c
#Return: 	TRUE if it is nullSymMatrix.c

setMethod("is.null.object", signature(target="simVector"), definition=function(target) {
		is(target, "nullSimVector")
	}
)
#Arguments: 	simVector.c that users wish to check
#Description: check whether the simVector.c is nullSimVector.c
#Return: 	TRUE if it is nullSimVector.c

setMethod("is.null.object", signature="simMatrixSet", definition=function(target) {
		is(target, "nullSimMatrixSet")
	}
)
#Arguments: 	simMatrixSet.c that users wish to check
#Description: check whether the simMatrixSet.c is nullSimMatrixSet.c
#Return: 	TRUE if it is nullSimMatrixSet.c

setMethod("is.null.object", signature="simConstraint", definition=function(target){
		is(target, "nullSimConstraint")
	}
)
#Arguments: 	simConstraint.c that users wish to check
#Description: check whether the simConstraint.c is nullSimConstraint.c
#Return: 	TRUE if it is nullSimConstraint.c

setMethod("is.null.object", signature="simReducedConstraint", definition=function(target){
		is(target, "nullSimReducedConstraint")
	}
)
#Arguments: 	simReducedConstraint.c that users wish to check
#Description: check whether the simReducedConstraint.c is nullSimReducedConstraint.c
#Return: 	TRUE if it is nullSimReducedConstraint.c

setMethod("is.null.object", signature="simMisspecifiedSet", definition=function(target) {
		is(target, "nullSimMisspecifiedSet")
	}
)
#Arguments: 	simMisspecifiedSet.c that users wish to check
#Description: check whether the simMisspecifiedSet.c is nullSimMisspecifiedSet.c
#Return: 	TRUE if it is nullSimMisspecifiedSet.c
