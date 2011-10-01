setMethod("is.null.object", signature(target="vector"), definition=function(target) {
		(sum(is(target)=="nullVector") == 1) || is.nan(target) || (sum(length(target)) == 0)
	}
)
