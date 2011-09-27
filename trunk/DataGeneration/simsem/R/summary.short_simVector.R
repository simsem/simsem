setMethod("summary.short", signature="simVector", definition = function(object) {
		Data <- object@Data
		Labels <- object@Labels
		for(i in 1:length(Data)) {
			if(is.na(Labels[i])) Labels[i] <- as.character(Data[i])
		}
		print(Labels)
	}
)