setMethod("is.null.object", signature(target="simMatrix"), definition=function(target) {
		if((length(target@Data) == 1) && length(target@Labels == 1)) {
			if(as.vector(is.nan(target@Data)) && as.vector(is.nan(target@Labels))) {
				return(TRUE)
			} else {
				return(FALSE)
			}
		} else {
			return(FALSE)
		}
	}
)