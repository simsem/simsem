setMethod("is.null.object", signature(target="vector"), definition=function(target) {
		if(length(target) == 1) {
			return(is.nan(target))
		} else {
			return(FALSE)
		}
	}
)