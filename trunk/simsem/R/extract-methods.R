# extract
# Methods -- simsem package
# Standardize the coefficients of an object
# Generic Function: standardize(object)
# Argument:
#	object: 	Desired object that users wish to standardize
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: February 21, 2012

setMethod("extract", signature="SimDataDist", definition=function(object, pos) {
		return(new("SimDataDist", p=length(pos), dist=object@dist[pos], keepScale=object@keepScale, reverse=object@reverse[pos]))
	}
)

setMethod("extract", signature="matrix", definition=function(object, row, col) {
	if(length(row) > 1 & length(col) > 1) {
		return(object[row, col])
	} else {
		if(length(col) == 1) return(as.matrix(object[row, col]))
		if(length(row) == 1) return(t(as.matrix(object[row, col])))
	}
}
)

