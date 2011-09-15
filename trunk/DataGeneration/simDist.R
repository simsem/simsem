loading.from.alpha <- function(alpha, ni) {
	denominator <- ni - ((ni - 1) * alpha)
	result <- sqrt(alpha/ denominator)
	return(result)
}

run <- function(object, ...) {}
setGeneric("run")

name <- function(x) deparse(substitute(x))

setClass("Runif",
	representation(
		Lower="numeric",
		Upper="numeric"
	)
)

runif.object <- function(Lower, Upper) {
	temp <- new("Runif", Lower=Lower, Upper=Upper)
}

setMethod("run", signature="Runif", definition=function(object) 
		runif(1,object@Lower, object@Upper)
)

setMethod("summary", signature="Runif", definition=function(object) {
		print("Random Uniform Distribution Object.")
		print(paste("Lower bound is ", format(object@Lower, digits=3), ".", sep=""))
		print(paste("Upper bound is ", format(object@Upper, digits=3), ".", sep=""))
	}
)

setClass("Rnorm",
	representation(
		Mean="numeric",
		SD="numeric"
	)
)

rnorm.object <- function(Mean, SD) {
	temp <- new("Rnorm", Mean=Mean, SD=SD)
}

setMethod("run", signature="Rnorm", definition=function(object) 
		rnorm(1,object@Mean, object@SD)
)

setMethod("summary", signature="Rnorm", definition=function(object) {
		print("Random Normal Distribution Object.")
		print(paste("Mean is ", format(object@Mean, digits=3), ".", sep=""))
		print(paste("Standard deviation is ", format(object@SD, digits=3), ".", sep=""))
	}
)

setClassUnion("simDist", c("Runif", "Rnorm"))
