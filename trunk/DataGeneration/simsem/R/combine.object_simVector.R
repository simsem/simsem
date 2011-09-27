setMethod("combine.object", signature(object1="simVector", object2="simVector"), definition= function(object1, object2) {
		Labels1 <- object1@Labels
		Labels2 <- object2@Labels
		Length <- length(Labels1)
		new.Labels <- rep(NA, Length)
		new.Data <- rep(NA, Length)
		if(Length != length(Labels2))  stop("The dimension of objects are not equal")
		for(i in 1:Length) {
			if(is.na(Labels1[i])) {
				if(is.na(Labels2[i])) {
					new.Data[i] <- object1@Data[i]
				} else {
					new.Labels[i] <- Labels2[i]
				}
			} else {
				if(is.na(Labels2[i])) {
					new.Labels[i] <- Labels1[i]
				} else {
					new.Labels[i] <- Labels2[i]
				}
			}
		}
		return(new(type, Data = new.Data, Labels = new.Labels))
	}
)