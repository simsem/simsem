setMethod("summary", signature="simMatrix", definition = function(object) {
		print("Random Full Matrix Object.")
		print("Parameters:")
		print(object@Data)
		print("Starting Values:")
		print(object@Labels)
	}	
)
