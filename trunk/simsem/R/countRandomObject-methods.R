setMethod("countRandomObject", signature="SimMatrix", definition=function(object) {
		if(isNullObject(object)) {
			return(0)
		} else {
			Labels <- object@param
			return(sum(!is.na(Labels)))
		}
	}
)

setMethod("countRandomObject", signature="SymMatrix", definition=function(object) {
		if(isNullObject(object)) {
			return(0)
		} else {
			Labels <- object@param
			return(sum(!is.na(Labels[upper.tri(Labels, diag=TRUE)])))
		}
	}
)

setMethod("countRandomObject", signature="SimVector", definition=function(object) {
		if(isNullObject(object)) {
			return(0)
		} else {
			Labels <- object@param
			return(sum(!is.na(Labels)))
		}
	}
)

setMethod("countRandomObject", signature="SimSet", definition=function(object) {
	return(sum(c(countRandomObject(object@LY),
		countRandomObject(object@RTE),
		countRandomObject(object@VTE),
		countRandomObject(object@RPS),
		countRandomObject(object@VPS),
		countRandomObject(object@BE),
		countRandomObject(object@TY),
		countRandomObject(object@AL),
		countRandomObject(object@ME),
		countRandomObject(object@MY),
		countRandomObject(object@VE),
		countRandomObject(object@VY),
		countRandomObject(object@LX),
		countRandomObject(object@RTD),
		countRandomObject(object@VTD),
		countRandomObject(object@RPH),
		countRandomObject(object@GA),
		countRandomObject(object@TX),
		countRandomObject(object@KA),
		countRandomObject(object@MX),
		countRandomObject(object@VPH),
		countRandomObject(object@VX),
		countRandomObject(object@RTH))))
	}
)

setMethod("countRandomObject", signature="matrix", definition=function(object, symmetric=FALSE) {
	if(symmetric){
		return(sum(is.na(diag(object))) + sum(is.na(object[upper.tri(object)])))
	} else {
		return(sum(is.na(object)))
	}
})

setMethod("countRandomObject", signature="vector", definition=function(object){
	return(sum(is.na(object)))
})

setMethod("countRandomObject", signature="VirtualRSet", definition=function(object) {
	return(sum(c(countRandomObject(object@LY, symmetric=FALSE),
		countRandomObject(object@TE, symmetric=TRUE),
		countRandomObject(object@PS, symmetric=TRUE),
		countRandomObject(object@BE, symmetric=FALSE),
		countRandomObject(object@TY),
		countRandomObject(object@AL),
		countRandomObject(object@LX, symmetric=FALSE),
		countRandomObject(object@TD, symmetric=TRUE),
		countRandomObject(object@PH, symmetric=TRUE),
		countRandomObject(object@GA, symmetric=FALSE),
		countRandomObject(object@TX),
		countRandomObject(object@KA),
		countRandomObject(object@TH, symmetric=FALSE))))
	}
)
