setMethod("is.null.object", signature(target="simVector"), definition=function(target) {
		sum(is(target)=="nullSimVector") == 1
	}
)
