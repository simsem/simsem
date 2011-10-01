setMethod("adjust.object", signature(target="simVector"), definition=function(target, simDist, position, constant.fixed=TRUE) {
		for(i in 1:length(position)) {
			if(is.character(simDist)) {
				target@Labels[position[i]] <- simDist
				target@Data[position[i]] <- NA
			} else if(is.numeric(simDist)) {
				if(constant.fixed) {
				target@Labels[position[i]] <- ""
				target@Data[position[i]] <- simDist
				} else {
				target@Labels[position[i]] <- as.character(simDist)
				target@Data[position[i]] <- NA
				}
			} else {
				stop("Please put a number or a name of random distribution object to the simDist attribute")
			}
		}
		return(target) #new("simMatrix", Data=target@Data, Labels=target@Labels))
	}
)
