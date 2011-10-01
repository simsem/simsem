setMethod("is.null.object", signature(target="symMatrix"), definition=function(target) {
		sum(is(target)=="nullSymMatrix") == 1
	}
)
