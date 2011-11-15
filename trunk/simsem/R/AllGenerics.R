setGeneric("run", function(object, ...) { 
	return(standardGeneric("run")) 
} )

setGeneric("adjust", function(target, VirtualDist, position, constant.fixed=TRUE) { 
	return(standardGeneric("adjust")) 
} )

setGeneric("combine.object", function(object1, object2, ...) { 
	return(standardGeneric("combine.object")) 
} )

setGeneric("constrain.matrices", function(object, SimEqualCon, ...) { 
	return(standardGeneric("constrain.matrices")) 
} )

setGeneric("count.random.object", function(object, ...) { 
	return(standardGeneric("count.random.object")) 
} )

setGeneric("create.implied.MACS", function(object) { 
	return(standardGeneric("create.implied.MACS")) 
} )

setGeneric("find.OpenMx.values", function(Parameters, Starting.Values) { 
	return(standardGeneric("find.OpenMx.values")) 
} )

setGeneric("is.null.object", function(target) { 
	return(standardGeneric("is.null.object")) 
} )

setGeneric("divide.object", function(object, constant, ...) { 
	return(standardGeneric("divide.object")) 
} )

setGeneric("make.labels", function(object, ...) { 
	return(standardGeneric("make.labels")) 
} )

setGeneric("simModel", function(object, ...) { 
	return(standardGeneric("simModel")) 
} )

setGeneric("starting.values", function(object, trial, ...) { 
	return(standardGeneric("starting.values")) 
} )

setGeneric("summary.short", function(object, ...) { 
	return(standardGeneric("summary.short")) 
} )

setGeneric("tag.headers", function(object, ...) { 
	return(standardGeneric("tag.headers")) 
} )

setGeneric("getCutoff", function(object, alpha, reverse=FALSE, used.fit=NULL) { 
	return(standardGeneric("getCutoff")) 
} )

setGeneric("plotCutoff", function(object, ...) { 
	return(standardGeneric("plotCutoff")) 
} )

setGeneric("plotPower", function(object.alt, object.null, ...) { 
	return(standardGeneric("plotPower")) 
} )

setGeneric("getPower", function(object.alt, cutoff, reverse = FALSE, used.fit=NULL) { 
	return(standardGeneric("getPower")) 
} )

setGeneric("vectorize.object", function(object, labels, ...) { 
	return(standardGeneric("vectorize.object")) 
} )

setGeneric("summaryParam", function(object, ...) { 
	return(standardGeneric("summaryParam")) 
} )
