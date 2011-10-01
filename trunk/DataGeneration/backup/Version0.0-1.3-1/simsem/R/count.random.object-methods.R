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
