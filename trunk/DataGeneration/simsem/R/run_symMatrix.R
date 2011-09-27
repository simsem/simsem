setMethod("run", signature="symMatrix", definition= function(object) {
		if(is.null.object(object)) return(.NULL.matrix)
		Matrix <- object@Data
		Nrow <- nrow(Matrix)
		Ncol <- ncol(Matrix)
		for(i in 1:Nrow) {
			for(j in 1:i) {
				if(is.na(Matrix[i, j])) {
					temp <- suppressWarnings(as.numeric(object@Labels[i,j]))
					if(is.na(temp)) {
						Matrix[i, j] <- run(get(object@Labels[i,j])) #first, second)
					} else {
						Matrix[i, j] <- temp
					}
					Matrix[j, i] <- Matrix[i, j]
				}
			}
		}
		return(Matrix)
	}
)