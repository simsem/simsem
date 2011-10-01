setMethod("count.random.object", signature="simVector", definition=function(object) {
		if(is.null.object(object)) {
			return(0)
		} else {
			Labels <- object@Labels
			return(sum(!is.na(Labels)))
		}
	}
)
