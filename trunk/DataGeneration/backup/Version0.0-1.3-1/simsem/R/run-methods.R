setMethod("run",
    signature(object = "Rnorm"),
    function (object) 
    {
        rnorm(1,object@Mean, object@SD)
    }
)

setMethod("run",
    signature(object = "Runif"),
    function (object) 
    {
        runif(1,object@Lower, object@Upper)
    }
)

setMethod("run",
    signature(object = "simMatrix"),
    function (object) 
    {
		Matrix <- object@Data
		Nrow <- nrow(Matrix)
		Ncol <- ncol(Matrix)
		for(i in 1:Nrow) {
			for(j in 1:Ncol) {
				if(is.na(Matrix[i, j]) & !is.nan(Matrix[i, j])) {
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

setMethod("run", signature="symMatrix", definition= function(object) {
		if(is.null.object(object)) return(new("nullMatrix"))
		Matrix <- object@Data
		Nrow <- nrow(Matrix)
		Ncol <- ncol(Matrix)
		for(i in 1:Nrow) {
			for(j in 1:i) {
				if(is.na(Matrix[i, j]) & !is.nan(Matrix[i, j])) {
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

setMethod("run", signature="simVector", definition= function(object) {
		if(is.null.object(object)) return(new("nullVector"))
		Vector <- object@Data
		Length <- length(Vector)
		for(i in 1:Length) {
			if(is.na(Vector[i]) & !is.nan(Vector[i])) { 
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

setMethod("run",
    signature(object = "nullSimMatrix"),
    function (object) 
    {
		return(new("nullMatrix"))
    }
)

setMethod("run", signature="nullSymMatrix", definition= function(object) {
		return(new("nullMatrix"))
	}
)

setMethod("run", signature="nullSimVector", definition= function(object) {
		return(new("nullVector"))
	}
)

