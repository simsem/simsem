setMethod("run", signature="simVector", definition= function(object) {
		if(is.null.object(object)) return(.NULL.vector)
		Vector <- object@Data
		Length <- length(Vector)
		for(i in 1:Length) {
			if(is.na(Vector[i])) { 
				temp <- suppressWarnings(as.numeric(object@Labels[i]))
				if(is.na(temp)) {
					Vector[i] <- run(get(object@Labels[i]))  #first, second)
				} else {
					Vector[i] <- temp
				}
			}
		}
		return(Vector)
	}
)