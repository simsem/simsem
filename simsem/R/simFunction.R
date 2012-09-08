# simFunction: Constructor of the SimFunction class

fun <- function(f, ...) {
    List <- list(...)
    mc <- match.call()
    return(new("SimFunction", fun = f, attribute = List, callfun = mc))
} 

execute <- function(object, x) {
	if (is.list(object) && "data" %in% names(object)) x <- x$data
    out <- list()
    out[[1]] <- object@fun
    out[[2]] <- x
    outlength <- length(object@attribute)
    for (i in 1:outlength) {
        out[[i + 2]] <- object@attribute[[i]]
    }
    names(out) <- c("", "", names(object@attribute))
    eval(as.call(out))
}
