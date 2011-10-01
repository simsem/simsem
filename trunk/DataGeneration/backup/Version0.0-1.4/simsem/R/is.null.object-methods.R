setMethod("is.null.object", signature(target="vector"), definition=function(target) {
		(sum(is(target)=="nullVector") == 1) || is.nan(target) || (sum(length(target)) == 0)
	}
)
setMethod("is.null.object", signature(target="matrix"), definition=function(target) {
		(sum(is(target)=="nullMatrix") == 1) || is.nan(target) || (sum(dim(target)) == 0)
	}
)

setMethod("is.null.object", signature(target="simMatrix"), definition=function(target) {
		sum(is(target)=="nullSimMatrix") == 1
	}
)

setMethod("is.null.object", signature(target="symMatrix"), definition=function(target) {
		sum(is(target)=="nullSymMatrix") == 1
	}
)

setMethod("is.null.object", signature(target="simVector"), definition=function(target) {
		sum(is(target)=="nullSimVector") == 1
	}
)

setMethod("is.null.object", signature="simMatrixSet", definition=function(target) {
		sum(is(target)=="nullMatrixSet") == 1
	}
)

