setMethod("count.random.object", signature="symMatrix", definition=function(object) {
		if(is.null.object(object)) {
			return(0)
		} else {
			Data <- object@Data
			return(sum(is.na(Data[upper.tri(Data, diag=TRUE)])))
		}
	}
)