setMethod("summary", signature="simVector", definition = function(object) {
		print("Random Vector Object.")
		print("Parameters:")
		print(object@Data)
		print("Starting Values:")
		print(object@Labels)
	}	
)