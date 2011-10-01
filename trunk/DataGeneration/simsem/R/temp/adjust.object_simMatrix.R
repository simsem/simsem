setMethod("adjust.object", signature(target="simMatrix"), definition=function(target, simDist, position, constant.fixed=TRUE) {
		if(is.vector(position) && (length(position) == 2)) position <- matrix(position, ncol=2)
		for(i in 1:nrow(position)) {
			if(is.character(simDist)) {
				target@Labels[position[i,1], position[i,2]] <- simDist
				target@Data[position[i,1], position[i,2]] <- NA
				if(is(target, "symMatrix")) {
					target@Labels[position[i,2], position[i,1]] <- simDist
					target@Data[position[i,2], position[i,1]] <- NA
				}
			} else if(is.numeric(simDist)) {
				if(constant.fixed) {
				target@Labels[position[i,1], position[i,2]] <- ""
				target@Data[position[i,1], position[i,2]] <- simDist
				if(is(target, "symMatrix")) {
					target@Labels[position[i,2], position[i,1]] <- ""
					target@Data[position[i,2], position[i,1]] <- simDist
				}
				} else {
				target@Labels[position[i,1], position[i,2]] <- as.character(simDist)
				target@Data[position[i,1], position[i,2]] <- NA
				if(is(target, "symMatrix")) {
					target@Labels[position[i,2], position[i,1]] <- as.character(simDist)
					target@Data[position[i,2], position[i,1]] <- NA
				}
				}
			} else {
				stop("Please put a number or a name of random distribution object to the simDist attribute")
			}
		}
		return(target) #new("simMatrix", Data=target@Data, Labels=target@Labels))
	}
)
