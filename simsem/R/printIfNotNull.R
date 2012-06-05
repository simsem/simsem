# printIfNotNull: Provide basic summary of each object if that object is not NULL. Mainly call from summary function from SimSet.c
# object.

printIfNotNull <- function(object, name = NULL) {
    if (!isNullObject(object)) {
        if (!is.null(name)) 
            cat(name, "\n")
        summaryShort(object)
    }
} 
