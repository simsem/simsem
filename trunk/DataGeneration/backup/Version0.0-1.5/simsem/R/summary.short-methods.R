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

setMethod("summary.short", signature="simVector", definition = function(object) {
		Data <- object@Data
		Labels <- object@Labels
		for(i in 1:length(Data)) {
			if(is.na(Labels[i])) Labels[i] <- as.character(Data[i])
		}
		print(Labels)
	}
)

setMethod("summary.short", signature="vector", definition=function(object) {
		print(object)
	}
)

setMethod("summary.short", signature="matrix", definition=function(object) {
		print(object)
	}
)
