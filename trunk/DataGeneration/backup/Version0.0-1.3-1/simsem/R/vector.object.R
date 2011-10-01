vector.object <- function(Vector, name.dist.object = NULL) {
	Length <- length(Vector)
	Labels <- rep("", Length)
	if(is.null(name.dist.object)) {
		return(new("simVector", Data=Vector, Labels=Labels))
	} else {
		if(length(name.dist.object) > 1) {
			if(length(name.dist.object) == Length) {
				for(i in 1:Length) {
					if(is.na(Vector[i])) Labels[i] <- name.dist.object[i]
				}
			} else {
				stop("The length of desired vector and label are not equal")
			}
		} else {
			for(i in 1:Length) {
				if(is.na(Vector[i])) Labels[i] <- name.dist.object
			}
		}
		return(new("simVector", Data=Vector, Labels=Labels))
	}
}
