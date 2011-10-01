setMethod("starting.values", signature(object="simMatrix"), definition=function(object, trial, ...) {
		if(is.null.object(object)) return(new("nullMatrix"))
		Nrow <- nrow(run(object))
		Ncol <- ncol(run(object))
		Result <- matrix(0, Nrow, Ncol)
		for(i in 1:trial) {
			temp <- run(object)
			Result <- Result + temp
		}
		return(Result / trial)
	}
)

setMethod("starting.values", signature(object="simVector"), definition=function(object, trial, ...) {
		if(is.null.object(object)) return(new("nullVector"))
		Length <- length(run(object))
		Result <- rep(0, Length)
		for(i in 1:trial) {
			temp <- run(object)
			Result <- Result + temp
		}
		return(Result / trial)
	}
)
