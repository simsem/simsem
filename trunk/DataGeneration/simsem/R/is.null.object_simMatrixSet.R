setMethod("is.null.object", signature="simMatrixSet", definition=function(target) {
		target@Tag == "NA"
	}
)