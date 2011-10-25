setMethod("count.random.object", signature="simMatrix", definition=function(object) {
		if(is.null.object(object)) {
			return(0)
		} else {
			Labels <- object@Labels
			return(sum(!is.na(Labels)))
		}
	}
)

setMethod("count.random.object", signature="symMatrix", definition=function(object) {
		if(is.null.object(object)) {
			return(0)
		} else {
			Labels <- object@Labels
			return(sum(!is.na(Labels[upper.tri(Labels, diag=TRUE)])))
		}
	}
)

setMethod("count.random.object", signature="simVector", definition=function(object) {
		if(is.null.object(object)) {
			return(0)
		} else {
			Labels <- object@Labels
			return(sum(!is.na(Labels)))
		}
	}
)

setMethod("count.random.object", signature="simMatrixSet", definition=function(object) {
	return(sum(c(count.random.object(object@LY),
		count.random.object(object@TE),
		count.random.object(object@VTE),
		count.random.object(object@PS),
		count.random.object(object@VPS),
		count.random.object(object@BE),
		count.random.object(object@TY),
		count.random.object(object@AL),
		count.random.object(object@ME),
		count.random.object(object@MY),
		count.random.object(object@VE),
		count.random.object(object@VY),
		count.random.object(object@LX),
		count.random.object(object@TD),
		count.random.object(object@VTD),
		count.random.object(object@PH),
		count.random.object(object@GA),
		count.random.object(object@TX),
		count.random.object(object@KA),
		count.random.object(object@MX),
		count.random.object(object@VPH),
		count.random.object(object@VX),
		count.random.object(object@TH))))
	}
)

setMethod("count.random.object", signature="matrix", definition=function(object, symmetric=FALSE) {
	if(symmetric){
		return(sum(is.na(diag(object))) + sum(is.na(object[upper.tri(object)])))
	} else {
		return(sum(is.na(object)))
	}
})

setMethod("count.random.object", signature="vector", definition=function(object){
	return(sum(is.na(object)))
})

setMethod("count.random.object", signature="blankReducedMatrixSet", definition=function(object) {
	return(sum(c(count.random.object(object@LY, symmetric=FALSE),
		count.random.object(object@TE, symmetric=TRUE),
		count.random.object(object@PS, symmetric=TRUE),
		count.random.object(object@BE, symmetric=FALSE),
		count.random.object(object@TY),
		count.random.object(object@AL),
		count.random.object(object@LX, symmetric=FALSE),
		count.random.object(object@TD, symmetric=TRUE),
		count.random.object(object@PH, symmetric=TRUE),
		count.random.object(object@GA, symmetric=FALSE),
		count.random.object(object@TX),
		count.random.object(object@KA),
		count.random.object(object@TH, symmetric=FALSE))))
	}
)
