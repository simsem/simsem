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
