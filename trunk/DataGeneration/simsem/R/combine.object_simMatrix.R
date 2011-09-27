setMethod("combine.object", signature(object1="simMatrix", object2="simMatrix"), definition= function(object1, object2) {
		type <- "simMatrix"
		if(is(object1, "symMatrix") && is(object2, "symMatrix")) type <- "symMatrix"
		Labels1 <- object1@Labels
		Labels2 <- object2@Labels
		Nrow <- nrow(Labels1)
		Ncol <- ncol(Labels2)
		new.Labels <- matrix(NA, Nrow, Ncol)
		new.Data <- matrix(NA, Nrow, Ncol)
		if((Nrow != nrow(Labels2)) | (Ncol != ncol(Labels2))) stop("The dimension of objects are not equal")
		for(i in 1:Nrow) {
			for(j in 1:Ncol) {
				if(is.na(Labels1[i, j])) {
					if(is.na(Labels2[i, j])) {
						new.Data[i, j] <- object1@Data[i, j]
					} else {
						new.Labels[i, j] <- Labels2[i, j]
					}
				} else {
					if(is.na(Labels2[i, j])) {
						new.Labels[i, j] <- Labels1[i, j]
					} else {
						new.Labels[i, j] <- Labels2[i, j]
					}
				}
			}
		}
		return(new(type, Data = new.Data, Labels = new.Labels))
	}
)