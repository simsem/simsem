setMethod("count.random.object", signature="symMatrix", definition=function(object) {
		if(is.null.object(object)) {
			return(0)
		} else {
			Labels <- object@Labels
			return(sum(!is.na(Labels[upper.tri(Labels, diag=TRUE)])))
		}
	}
)
