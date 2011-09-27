setMethod("is.null.object", signature(target="simVector"), definition=function(target) {
		if((length(target@Data) == 1) && length(target@Labels == 1)) {
			if(is.na(target@Data) && is.na(target@Labels)) {
				return(TRUE)
			} else {
				return(FALSE)
			}
		} else {
			return(FALSE)
		}
	}
)