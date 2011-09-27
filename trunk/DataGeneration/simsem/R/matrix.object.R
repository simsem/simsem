matrix.object <-
function(Matrix, name.dist.object = NULL) {
	Nrow <- nrow(Matrix)
	Ncol <- ncol(Matrix)
	Labels <- matrix("", Nrow, Ncol)
	if(is.null(name.dist.object)) {
		return(new("simMatrix", Data=Matrix, Labels=Labels))
	} else {
		if(is.matrix(name.dist.object)) {
			if(nrow(name.dist.object) == Nrow & ncol(name.dist.object) == Ncol) {
				for(i in 1:Nrow) {
					for(j in 1:Ncol) {
						if(is.na(Matrix[i, j])) Labels[i, j] <- name.dist.object[i, j] #first, second)
					}
				}
			} else {
				stop("Desired matrix and labels do not have the same dimensions")
			}
		} else {
			for(i in 1:Nrow) {
				for(j in 1:Ncol) {
					if(is.na(Matrix[i, j])) Labels[i, j] <- name.dist.object #first, second)
				}
			}
		}
		return(new("simMatrix", Data=Matrix, Labels=Labels))
	}
}

