setMethod("summary.short", signature="simMatrix", definition = function(object) {
		Data <- object@Data
		Labels <- object@Labels
		for(i in 1:nrow(Data)) {
			for(j in 1:ncol(Data)) {
				if(is.na(Labels[i,j])) Labels[i,j] <- as.character(Data[i,j])
			}
		}
		print(Labels)
	}
)