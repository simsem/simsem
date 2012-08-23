## # startingValues: Find starting values of free parameters based on pre-specified starting values. If the pre-specified starting
## # values are numbers, the function will use that values. If they are distribution object, this function will randomly draw from the
## # distribution 10 times and take the average of those values.

## setMethod("startingValues", signature(object = "SimMatrix"), definition = function(object, trial, ...) {
##     if (isNullObject(object)) 
##         return(new("NullMatrix"))
##     Nrow <- nrow(run(object))
##     Ncol <- ncol(run(object))
##     Result <- matrix(0, Nrow, Ncol)
##     for (i in 1:trial) {
##         temp <- run(object)
##         Result <- Result + temp
##     }
##     return(Result/trial)
## })


## setMethod("startingValues", signature(object = "SimVector"), definition = function(object, trial, ...) {
##     if (isNullObject(object)) 
##         return(new("NullVector"))
##     Length <- length(run(object))
##     Result <- rep(0, Length)
##     for (i in 1:trial) {
##         temp <- run(object)
##         Result <- Result + temp
##     }
##     return(Result/trial)
## })

## setMethod("startingValues", signature(object = "SimSet"), definition = function(object, trial, reduced = FALSE) {
##     result <- run(object)
##     if (trial > 1) {
##         for (i in 2:trial) {
##             temp <- run(object)
##             result <- combineObject(result, temp)
##         }
##         result <- divideObject(result, trial)
##     }
##     result@modelType <- object@modelType
##     if (reduced == TRUE) 
##         result <- reduceMatrices(result)
##     return(result)
## })
 
