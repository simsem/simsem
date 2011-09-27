setMethod("run", signature="simMatrix", definition= function(object) {
		if(is.null.object(object)) return(.NULL.matrix)
		Matrix <- object@Data
		Nrow <- nrow(Matrix)
		Ncol <- ncol(Matrix)
		for(i in 1:Nrow) {
			for(j in 1:Ncol) {
				if(is.na(Matrix[i, j])) {
					temp <- suppressWarnings(as.numeric(object@Labels[i,j]))
					if(is.na(temp)) {
						Matrix[i, j] <- run(get(object@Labels[i,j])) #first, second)
					} else {
						Matrix[i, j] <- temp
					}
				}
			}
		}
		return(Matrix)
	}
)