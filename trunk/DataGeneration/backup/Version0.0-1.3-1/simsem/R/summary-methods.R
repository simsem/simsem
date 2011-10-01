setMethod("summary",
    signature(object = "Rnorm"),
    function (object)
    {
		print("Random Normal Distribution Object.")
		print(paste("Mean is ", format(object@Mean, digits=3), ".", sep=""))
		print(paste("Standard deviation is ", format(object@SD, digits=3), ".", sep=""))
    }
)

setMethod("summary",
    signature(object = "Runif"),
    function (object) 
    {
		print("Random Uniform Distribution Object.")
		print(paste("Lower bound is ", format(object@Lower, digits=3), ".", sep=""))
		print(paste("Upper bound is ", format(object@Upper, digits=3), ".", sep=""))
    }
)

setMethod("summary", signature="simMatrix", definition = function(object) {
		print("Random Full Matrix Object.")
		print("Parameters:")
		print(object@Data)
		print("Starting Values:")
		print(object@Labels)
	}	
)

setMethod("summary", signature="symMatrix", definition = function(object) {
		print("Random Symmetric Matrix Object.")
		print("Parameters:")
		print(object@Data)
		print("Starting Values:")
		print(object@Labels)
	}	
)

setMethod("summary", signature="simVector", definition = function(object) {
		print("Random Vector Object.")
		print("Parameters:")
		print(object@Data)
		print("Starting Values:")
		print(object@Labels)
	}	
)
