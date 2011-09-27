setMethod("summary", signature="symMatrix", definition = function(object) {
		print("Random Symmetric Matrix Object.")
		print("Parameters:")
		print(object@Data)
		print("Starting Values:")
		print(object@Labels)
	}	
)