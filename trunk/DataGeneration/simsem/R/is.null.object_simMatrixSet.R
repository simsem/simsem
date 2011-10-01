setMethod("is.null.object", signature="simMatrixSet", definition=function(target) {
		sum(is(target)=="nullMatrixSet") == 1
	}
)
