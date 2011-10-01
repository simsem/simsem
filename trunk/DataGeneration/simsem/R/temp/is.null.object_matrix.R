setMethod("is.null.object", signature(target="matrix"), definition=function(target) {
		(sum(is(target)=="nullMatrix") == 1) || is.nan(target) || (sum(dim(target)) == 0)
	}
)
