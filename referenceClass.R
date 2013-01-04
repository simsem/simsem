########################## Super generic class ###################################

SimSem <- setRefClass("SimSem")

# Create the method for users to use reference class by usual command 
# like run(u01) not u01$run()
# We will never use this function inside our codes!

setGeneric("run", function(object, ...) { 
	return(standardGeneric("run")) 
} )

setMethod("run",
    signature(object = "SimSem"),
    function (object, ...) 
    {
		object$run(...)
    }
)

setMethod("summary",
    signature(object = "SimSem"),
    function (object) 
    {
		object$summary()
    }
)

########################## Purposive class #################################

SimUnif <- setRefClass("SimUnif",
      fields = list( lower = "numeric",
        upper = "numeric"),
	  contain = "SimSem",
      methods = list(
     run = function(n = 1) {
       'Random a value from a uniform distribution object
        '
        runif(n, lower, upper)
     },
     summary = function() {
       'Provide a summary of a uniform distribution object
        '
        cat("Uniform Distribution Object\n")
		cat(paste("Lower bound =", lower, "\n"))
		cat(paste("Upper bound =", upper, "\n"))
     }
     ))

SimNorm <- setRefClass("SimNorm",
      fields = list( mean = "numeric",
        sd = "numeric"),
	  contain = "SimSem",
      methods = list(
     run = function(k = 1) { # Intend to use different symbols here
       'Random a value from a normal distribution object
        '
        rnorm(k, mean, sd)
     },
     summary = function() {
       'Provide a summary of a normal distribution object
        '
        cat("Normal Distribution Object\n")
		cat(paste("Mean =", mean, "\n"))
		cat(paste("Standard Deviation =", sd, "\n"))
     },
	 runagain = function() {
		run()
	 }
     ))

######################### Constructor #############################

simUnif <- function(lower, upper) {
	SimUnif$new(lower = lower, upper = upper)
}

simNorm <- function(mean, sd) {
	SimNorm$new(mean = mean, sd = sd)
}

########################## Example  ##############################	 

obj <- SimUnif$new(lower = 0, upper = 1)
obj$run()

obj2 <- SimNorm$new(mean = 0, sd = 1)
obj2$run(3)


run(obj, n=2)
run(obj, 2)
run(obj, k=2) # Expect an error here
run(obj2, k=2)
run(obj2, 2)
run(obj2, n=2) # Expect an error here

u01 <- simUnif(0, 1)
run(u01)
summary(u01)

n01 <- simNorm(0, 1)
run(n01)
summary(n01)
