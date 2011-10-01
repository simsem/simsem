setMethod("is.null.object", signature(target="simMatrix"), definition=function(target) {
		sum(is(target)=="nullSimMatrix") == 1
	}
)
