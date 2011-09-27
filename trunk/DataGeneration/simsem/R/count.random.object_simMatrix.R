setMethod("count.random.object", signature="simMatrix", definition=function(object) {
		if(is.null.object(object)) {
			return(0)
		} else {
			Data <- object@Data
			return(sum(is.na(Data)))
		}
	}
)