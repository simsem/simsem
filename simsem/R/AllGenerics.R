setMethod("summary",
    signature(object = "SimSemParentClass"),
    function (object) 
    {
		object$summary()
    }
)



setGeneric("summaryShort", function(object, ...) {
    return(standardGeneric("summaryShort"))
})

setMethod("summaryShort",
    signature(object = "SimSemParentClass"),
    function (object, ...) 
    {
		object$summaryShort(...)
    }
)

setGeneric("getCutoff", function(object, alpha, revDirec = FALSE, usedFit = NULL, 
    ...) {
    return(standardGeneric("getCutoff"))
})

setGeneric("plotCutoff", function(object, ...) {
    return(standardGeneric("plotCutoff"))
})

setGeneric("pValue", function(target, dist, ...) {
    return(standardGeneric("pValue"))
})
