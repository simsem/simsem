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
