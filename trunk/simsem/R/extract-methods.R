# extract
# Methods -- simsem package
# Standardize the coefficients of an object
# Generic Function: standardize(object)
# Argument:
#	object: 	Desired object that users wish to standardize
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: February 28, 2012

setMethod("extract", signature="SimDataDist", definition=function(object, pos) {
		return(new("SimDataDist", p=length(pos), dist=object@dist[pos], keepScale=object@keepScale, reverse=object@reverse[pos]))
	}
)

setMethod("extract", signature="vector", definition=function(object, pos=NULL) {
	if(is.null.object(object)) return(object)
	if(is.null(pos)) {
		return(object)
	} else {
		return(object[pos])
	}
}
)

setMethod("extract", signature="matrix", definition=function(object, row=NULL, col=NULL) {
	if(is.null.object(object)) return(object)
	if(is.null(row)) row <- 1:nrow(object)
	if(is.null(col)) col <- 1:ncol(object)
	if(length(row) > 1 & length(col) > 1) {
		return(object[row, col])
	} else {
		if(length(col) == 1) return(as.matrix(object[row, col]))
		if(length(row) == 1) return(t(as.matrix(object[row, col])))
	}
}
)

setMethod("extract", signature="SimMatrix", definition=function(object, row=NULL, col=NULL) {
	if(is.null.object(object)) return(object)
	if(is.null(row)) row <- 1:nrow(object@free)
	if(is.null(col)) col <- 1:ncol(object@free)
	object@free <- extract(object@free, row, col)
	object@param <- extract(object@param, row, col)
	return(object)
}
)

setMethod("extract", signature="SimVector", definition=function(object, pos=NULL) {
	if(is.null.object(object)) return(object)
	if(is.null(pos)) pos <- 1:length(object@free)
	object@free <- object@free[pos]
	object@param <- object@param[pos]
	return(object)
}
)

setMethod("extract", signature="SimSet", definition=function(object, yOnly=FALSE, y=NULL, e=NULL, x=NULL, k=NULL) {
	if(yOnly) {
		if(object@modelType == "CFA") stop("The yOnly option can be used only for the object in path analysis or SEM model with X side.")
		if(!is.null(y) | !is.null(e) | !is.null(x) | !is.null(k)) stop("The 'y', 'e', 'x', and 'k' arguments can be used only when the yOnly argument is FALSE.")
		object@modelType <- gsub(".exo", "", object@modelType)
		object@LX <- new("NullSimMatrix")
		object@TD <- new("NullSymMatrix")
		object@VTD <- new("NullSimVector")
		object@PH <- new("NullSymMatrix")
		object@GA <- new("NullSimMatrix")
		object@TX <- new("NullSimVector")
		object@KA <- new("NullSimVector")
		object@MX <- new("NullSimVector")
		object@VPH <- new("NullSimVector")
		object@VX <- new("NullSimVector")
		object@TH <- new("NullSimMatrix")
	} else {
		if(object@modelType == "Path" | object@modelType == "Path.exo") {
			if(is.null(y)) y <- 1:nrow(object@PS@free)
			object@BE <- extract(object@BE, y, y)
			object@PS <- extract(object@PS, y, y)
			object@VPS <- extract(object@VPS, y)
			object@VE <- extract(object@VE, y)
			object@AL <- extract(object@AL, y)
			object@ME <- extract(object@ME, y)
			if(object@modelType == "Path.exo") {
				if(is.null(x)) x <- 1:nrow(object@PH@free)
				object@GA <- extract(object@GA, y, x)
				object@PH <- extract(object@PH, x, x)
				object@VPH <- extract(object@VPH, x)
				object@KA <- extract(object@KA, x)
			}
		} else {
			if(is.null(y)) y <- 1:nrow(object@LY@free)
			if(is.null(e)) e <- 1:ncol(object@LY@free)
			object@LY <- extract(object@LY, y, e)
			object@TE <- extract(object@TE, y, y)
			object@VTE <- extract(object@VTE, y)
			object@VY <- extract(object@VY, y)
			object@TY <- extract(object@TY, y)
			object@MY <- extract(object@MY, y)
			object@BE <- extract(object@BE, e, e)
			object@PS <- extract(object@PS, e, e)
			object@VPS <- extract(object@VPS, e)
			object@VE <- extract(object@VE, e)
			object@AL <- extract(object@AL, e)
			object@ME <- extract(object@ME, e)
			if(object@modelType == "SEM.exo") {
				if(is.null(x)) x <- 1:nrow(object@LX@free)
				if(is.null(k)) k <- 1:ncol(object@LX@free)
				object@LX <- extract(object@LX, x, k)
				object@TD <- extract(object@TD, x, x)
				object@VTD <- extract(object@VTD, x)
				object@VX <- extract(object@VX, x)
				object@TX <- extract(object@TX, x)
				object@MX <- extract(object@MX, x)
				object@GA <- extract(object@GA, e, k)
				object@PH <- extract(object@PH, k, k)
				object@VPH <- extract(object@VPH, k)
				object@KA <- extract(object@KA, k)
				object@TH <- extract(object@TH, x, y)	
			}
		}	
	}
	return(object)
}
)

setMethod("extract", signature="VirtualRSet", definition=function(object, yOnly=FALSE, y=NULL, e=NULL, x=NULL, k=NULL) {
	if(yOnly) {
		if(object@modelType == "CFA") stop("The yOnly option can be used only for the object in path analysis or SEM model with X side.")
		if(!is.null(y) | !is.null(e) | !is.null(x) | !is.null(k)) stop("The 'y', 'e', 'x', and 'k' arguments can be used only when the yOnly argument is FALSE.")
		object@modelType <- gsub(".exo", "", object@modelType)
		object@LX <- new("NullMatrix")
		object@TD <- new("NullMatrix")
		object@PH <- new("NullMatrix")
		object@GA <- new("NullMatrix")
		object@TX <- new("NullVector")
		object@KA <- new("NullVector")
		object@TH <- new("NullMatrix")
	} else {
		if(object@modelType == "Path" | object@modelType == "Path.exo") {
			if(is.null(y)) y <- 1:nrow(object@PS)
			object@BE <- extract(object@BE, y, y)
			object@PS <- extract(object@PS, y, y)
			object@AL <- extract(object@AL, y)
			if(object@modelType == "Path.exo") {
				if(is.null(x)) x <- 1:nrow(object@PH)
				object@GA <- extract(object@GA, y, x)
				object@PH <- extract(object@PH, x, x)
				object@KA <- extract(object@KA, x)
			}
		} else {
			if(is.null(y)) y <- 1:nrow(object@LY)
			if(is.null(e)) e <- 1:ncol(object@LY)
			object@LY <- extract(object@LY, y, e)
			object@TE <- extract(object@TE, y, y)
			object@TY <- extract(object@TY, y)
			object@BE <- extract(object@BE, e, e)
			object@PS <- extract(object@PS, e, e)
			object@AL <- extract(object@AL, e)
			if(object@modelType == "SEM.exo") {
				if(is.null(x)) x <- 1:nrow(object@LX)
				if(is.null(k)) k <- 1:ncol(object@LX)
				object@LX <- extract(object@LX, x, k)
				object@TD <- extract(object@TD, x, x)
				object@TX <- extract(object@TX, x)
				object@GA <- extract(object@GA, e, k)
				object@PH <- extract(object@PH, k, k)
				object@KA <- extract(object@KA, k)
				object@TH <- extract(object@TH, x, y)	
			}
		}	
	}
	return(object)
}
)
