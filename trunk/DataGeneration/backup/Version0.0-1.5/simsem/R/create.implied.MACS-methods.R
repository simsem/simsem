setMethod("create.implied.MACS", signature="matrixSet", definition=function(object) {
		new.object <- reduce.matrices(object)
		result <- create.implied.MACS(new.object)
		return(result)
	}
)
