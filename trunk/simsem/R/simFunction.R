# simFunction: Constructor of the SimFunction class

simFunction <- function(fun, ...) {
    List <- list(...)
    mc <- match.call()
    return(new("SimFunction", fun = fun, attribute = List, callfun = mc))
} 
